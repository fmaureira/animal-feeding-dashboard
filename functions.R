# Flag temporally overlapping feeding events for each animal and group them
# into overlap chains. An animal cannot physically be in two places at once,
# so any records for the same animal that overlap in time are sensor
# artefacts and must be flagged regardless of the feeder they were recorded
# at (the same animal can even be double-registered at the same feeder).
#
# Events are clustered per animal in StartTime order: an event joins the
# current cluster when its StartTime falls within `tolerance_sec` of the
# running maximum EndTime of that cluster (so a single long event keeps the
# cluster "open" even when shorter events are interleaved). Clusters with two
# or more members receive an overlap_id; singletons stay NA.
identify_overlap = function(df, input){
  tol <- as.numeric(input$tolerance_sec)
  if (length(tol) == 0 || is.na(tol)) tol <- 0

  df[, c("overlap_flag", "overlap_id") := {
    n <- .N
    flag   <- rep(FALSE, n)
    id_vec <- rep(NA_integer_, n)

    if (n > 1) {
      st  <- as.numeric(StartTime)   # seconds since epoch
      en  <- as.numeric(EndTime)
      ord <- order(st, en)           # chronological, longer event last on ties

      next_id      <- 1L
      cluster_first <- ord[1]        # original index of first event in cluster
      cluster_end   <- en[ord[1]]    # running max EndTime of current cluster
      cluster_id    <- NA_integer_

      for (k in 2:n) {
        idx <- ord[k]
        gap <- st[idx] - cluster_end   # seconds between this start and cluster end

        if (!is.na(gap) && gap <= tol) {
          # Overlaps the current cluster -> same chain
          if (is.na(cluster_id)) {
            cluster_id <- next_id
            next_id    <- next_id + 1L
            id_vec[cluster_first] <- cluster_id
            flag[cluster_first]   <- TRUE
          }
          id_vec[idx] <- cluster_id
          flag[idx]   <- TRUE
          cluster_end <- max(cluster_end, en[idx], na.rm = TRUE)
        } else {
          # Gap too large -> start a fresh cluster
          cluster_first <- idx
          cluster_end   <- en[idx]
          cluster_id    <- NA_integer_
        }
      }
    }

    list(flag, id_vec)
  }, by = AnimalTag]
}


# ---------------------------------------------------------------------------
# Time parsing
# ---------------------------------------------------------------------------

# Parse the StartTime / EndTime columns to POSIXct and reconcile timezone
# mismatches between them. Some source files store StartTime in local time
# and EndTime in UTC (a trailing "Z"), which leaves the two columns offset by
# a whole number of hours even though they describe events seconds apart.
# DurationSec is the reliable column, so when EndTime - StartTime departs
# systematically from DurationSec we rebuild EndTime as StartTime + Duration,
# which puts both columns on the same clock and in the same format.
parse_feeding_times <- function(dt) {
  dt <- as.data.table(dt)
  orders <- c("mdY HM p", "mdY HMS p", "mdY HMS", "mdY HM",
              "ymd HMS", "ymd HMSz", "ymd HM", "ymd")

  if (!inherits(dt$StartTime, "POSIXct"))
    dt[, StartTime := parse_date_time(StartTime, orders = orders, tz = "UTC", quiet = TRUE)]
  if (!inherits(dt$EndTime, "POSIXct"))
    dt[, EndTime   := parse_date_time(EndTime,   orders = orders, tz = "UTC", quiet = TRUE)]

  if ("DurationSec" %in% names(dt)) {
    disc <- as.numeric(difftime(dt$EndTime, dt$StartTime, units = "secs")) - dt$DurationSec
    offset <- stats::median(disc, na.rm = TRUE)
    # A systematic offset of a minute or more signals a timezone mismatch
    if (is.finite(offset) && abs(offset) >= 60) {
      dt[, EndTime := StartTime + DurationSec]
    }
  }
  dt
}


# ---------------------------------------------------------------------------
# Meal-interval analysis
# ---------------------------------------------------------------------------

# Compute the interval between consecutive feeding events for each animal.
# The interval is measured from the EndTime of one event to the StartTime of
# the next (per animal, ordered by time). A non-negative value is expected;
# a negative value means events still overlap and the merge/delete step did
# not fully resolve them. A LogInterval column (natural log) is added for the
# mixture model; non-positive intervals cannot be log-transformed and are NA.
compute_meal_intervals <- function(df) {
  df <- as.data.table(copy(df))

  # A file that has been through this app before (or through Excel) already
  # carries these columns, and not necessarily as numbers: an exported
  # IntervalSec often comes back as text, with "." standing in for the first
  # row's missing gap. data.table assigns into an existing column rather than
  # replacing it, so the freshly computed seconds would be coerced back to
  # text and log() would fail on them. Drop any stale copy and recompute.
  stale <- intersect(c("IntervalSec", "IntervalNegative", "LogInterval"), names(df))
  if (length(stale)) df[, (stale) := NULL]

  if (!inherits(df$StartTime, "POSIXct")) df[, StartTime := as.POSIXct(StartTime)]
  if (!inherits(df$EndTime, "POSIXct"))   df[, EndTime   := as.POSIXct(EndTime)]

  # Order by start, breaking ties on EndTime so a longer event is never placed
  # after a shorter event that started in the same minute (which would yield a
  # spurious large negative gap).
  setorder(df, AnimalTag, StartTime, EndTime)

  # Interval = time since the animal last FINISHED eating. We compare each
  # StartTime against the running maximum of all prior EndTimes, not merely the
  # previous row's EndTime: with overlapping or nested events the immediately
  # preceding row may not hold the latest end time, which is what produced the
  # large negative intervals.
  df[, IntervalSec := {
    prev_end_max <- shift(cummax(as.numeric(EndTime)), type = "lag")
    as.numeric(StartTime) - prev_end_max
  }, by = AnimalTag]

  # StartTime is often recorded only to the minute, so a gap can be off by up
  # to ~60s purely from rounding (and nested events resolve to ~0). Treat tiny
  # negative gaps as zero and only flag larger negatives as genuinely
  # unresolved overlaps.
  round_tol <- 60
  df[!is.na(IntervalSec) & IntervalSec < 0 & IntervalSec >= -round_tol,
     IntervalSec := 0]
  df[, IntervalNegative := !is.na(IntervalSec) & IntervalSec < 0]

  df[, LogInterval := fifelse(!is.na(IntervalSec) & IntervalSec > 0,
                              log(IntervalSec), NA_real_)]
  df[]
}


# Gaps that run across a missing or left-out day (e.g. a power cut) are not a
# real break the animal took, so they are blanked (NA): they stay out of the
# mixture fit, and assign_visits starts a new meal after them. `days` are the
# left-out dates; a gap is blanked if it overlaps any part of one of them.
drop_gaps_over_days <- function(df, days) {
  days <- unique(as.Date(days))
  days <- days[!is.na(days)]
  if (!length(days) || !nrow(df)) return(df)

  tz <- attr(df$StartTime, "tzone")
  if (is.null(tz)) tz <- ""
  start    <- as.numeric(df$StartTime)
  prev_end <- start - df$IntervalSec
  day_start <- as.numeric(as.POSIXct(format(days), tz = tz))
  day_end   <- as.numeric(as.POSIXct(format(days + 1), tz = tz))

  spans <- rep(FALSE, nrow(df))
  for (j in seq_along(days)) {
    spans <- spans | (!is.na(prev_end) & prev_end < day_end[j] &
                        start > day_start[j])
  }
  df[spans, `:=`(IntervalSec = NA_real_, LogInterval = NA_real_)]
  df[]
}


# Largest time step the recorded intervals are consistent with: feeding times
# are usually stored to the whole minute, which makes every derived gap a
# multiple of 60 s. Returns 1 (no rounding detected) unless at least 90% of
# the positive gaps sit exactly on the candidate grid.
detect_time_grid <- function(sec) {
  s <- sec[is.finite(sec) & sec > 0]
  if (length(s) < 20) return(1)
  for (g in c(300, 120, 60, 30, 15, 10, 5)) {
    if (mean(abs(s - round(s / g) * g) < 1e-6) >= 0.9) return(g)
  }
  1
}


# Undo the effect of that rounding before the mixture is fitted. A gap stored
# as "1 minute" is really anything from a few seconds to about two minutes,
# but every one of them lands on exactly log(60), which is what pins a mixture
# component onto a zero-width spike and drags the meal threshold down to the
# rounding step itself. Spreading each gap uniformly across the step it was
# rounded to restores a continuous distribution without changing its centre.
#
# The jitter is drawn from a fixed seed (and the global RNG stream is left
# untouched) so the figure and the threshold do not move between redraws.
# Gaps of zero or less are left alone: those are unresolved overlaps, not
# rounded measurements, and they stay out of the fit.
dequantise_intervals <- function(sec, grid = NULL, seed = 1) {
  if (is.null(grid)) grid <- detect_time_grid(sec)
  if (!is.finite(grid) || grid <= 1) return(sec)

  idx <- which(is.finite(sec) & sec > 0)
  if (length(idx) == 0) return(sec)

  had_seed <- exists(".Random.seed", envir = .GlobalEnv)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  set.seed(seed)
  noise <- runif(length(idx), -grid / 2, grid / 2)
  if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv)

  out <- sec
  # Keep a floor well below the grid so the log stays finite without inventing
  # gaps far shorter than anything the recorder could have resolved.
  out[idx] <- pmax(sec[idx] + noise, grid / 20)
  attr(out, "grid") <- grid
  out
}


# ---------------------------------------------------------------------------
# Weibull component
# ---------------------------------------------------------------------------

# Weighted maximum-likelihood fit of a Weibull to positive values t. Solves
# the profile equation for the shape, then reads the scale off it. The values
# are divided by their maximum first so that t^shape cannot overflow; the
# scale is put back afterwards.
#
# The profile equation is monotone in the shape, so Newton's method converges
# from any starting point once it is clamped to the bracket. `start` takes the
# shape from the previous EM round, which brings each re-fit down to two or
# three passes over the data - a bracketed search would spend a few dozen, and
# this runs inside every EM iteration.
fit_weibull_weighted <- function(t, r, start = NULL, max_iter = 50, tol = 1e-10,
                                 lower = 0.05, upper = 50) {
  ok <- is.finite(t) & t > 0 & is.finite(r) & r > 0
  t <- t[ok]; r <- r[ok]
  if (length(t) < 2) return(NULL)
  R <- sum(r)
  if (!is.finite(R) || R <= 0) return(NULL)

  m  <- max(t)
  u  <- t / m
  lu <- log(u)
  rl <- sum(r * lu) / R

  # value and derivative of the profile equation at k
  fdf <- function(k) {
    ruk <- r * exp(k * lu)
    A   <- sum(ruk)
    if (!is.finite(A) || A <= 0) return(NULL)
    A1 <- sum(ruk * lu)
    A2 <- sum(ruk * lu * lu)
    list(f = A1 / A - 1 / k - rl,
         d = (A2 * A - A1 * A1) / (A * A) + 1 / (k * k))
  }

  k <- if (is.null(start) || !is.finite(start) || start <= 0)
         1 else min(max(start, lower), upper)
  for (i in seq_len(max_iter)) {
    v <- fdf(k)
    if (is.null(v) || !is.finite(v$f) || !is.finite(v$d) || v$d <= 0) return(NULL)
    k_new <- min(max(k - v$f / v$d, lower), upper)
    done  <- abs(k_new - k) < tol * max(1, k)
    k     <- k_new
    if (done) break
  }

  shape <- k
  scale <- m * (sum(r * exp(shape * lu)) / R)^(1 / shape)
  if (!is.finite(shape) || !is.finite(scale) || scale <= 0) return(NULL)
  list(shape = shape, scale = scale)
}


# Density of each component at xs, already multiplied by its mixing weight, as
# a matrix with one column per component. A Weibull component describes the
# gaps in real time, so on the log axis the plots and the likelihood use it
# through the change of variables f(exp(x)) * exp(x).
component_densities <- function(fit, xs) {
  fams <- if (is.null(fit$families)) rep("normal", length(fit$mu)) else fit$families
  out <- vapply(seq_along(fit$mu), function(j) {
    if (fams[j] == "weibull") {
      tt <- exp(xs)
      fit$lambda[j] * dweibull(tt, fit$shape, fit$scale) * tt
    } else {
      fit$lambda[j] * dnorm(xs, fit$mu[j], fit$sigma[j])
    }
  }, numeric(length(xs)))
  matrix(out, nrow = length(xs), ncol = length(fit$mu))
}


# How well the fitted curve actually describes the data it was fitted to.
# R2 compares the combined curve with the histogram the reader sees (the same
# bins the figure draws), which is the question "does the line follow the
# bars". BIC judges the model itself and is only comparable between fits of
# the same data under the same weighting - use it to choose between 2, 3 or 4
# components, or between the normal and Weibull versions.
mixture_fit_quality <- function(x, weights = NULL, fit, bins = 60) {
  keep <- is.finite(x)
  if (!is.null(weights)) keep <- keep & is.finite(weights) & weights > 0
  xx <- x[keep]
  wt <- if (is.null(weights)) rep(1, length(xx)) else weights[keep]
  if (length(xx) == 0 || is.null(fit) || diff(range(xx)) == 0) return(NULL)

  brks <- seq(min(xx), max(xx), length.out = bins + 1)
  bin  <- cut(xx, breaks = brks, include.lowest = TRUE)
  bw   <- tapply(wt, bin, sum)
  bw[is.na(bw)] <- 0
  obs  <- as.numeric(bw) / (sum(wt) * diff(brks))
  mid  <- (brks[-1] + brks[-length(brks)]) / 2
  pred <- rowSums(component_densities(fit, mid))

  ss_res <- sum((obs - pred)^2)
  ss_tot <- sum((obs - mean(obs))^2)
  n_par  <- 3 * length(fit$mu) - 1

  list(r2   = if (ss_tot > 0) 1 - ss_res / ss_tot else NA_real_,
       rmse = sqrt(mean((obs - pred)^2)),
       bic  = -2 * fit$loglik + n_par * log(fit$n),
       aic  = -2 * fit$loglik + 2 * n_par)
}


# Fit a k-component univariate mixture by EM (base R; no external
# dependencies). Returns components ordered by increasing centre, with the
# mixing weights (lambda), centres (mu), widths (sigma) and the final
# log-likelihood.
#
# `w` optionally gives each observation a weight, so the mixture describes the
# distribution per unit of weight rather than per event. Weighting the meal
# intervals by the mass eaten in the event that follows lets a 2 kg meal count
# for more than a 20 g nibble. Observations with a zero or non-finite weight
# drop out of the fit.
#
# `family` chooses the shape of the first (shortest) component:
#   "normal"  - every component is a normal on the log-interval axis, the
#               familiar meal-criterion model.
#   "weibull" - the shortest component is a Weibull in real time, which lets
#               the within-meal gaps be skewed rather than symmetric once
#               logged. It enters the mixture through the change of variables
#               f(exp(x)) * exp(x), so the likelihood and the figure stay on
#               the same log axis as the normal components and the two
#               families can be compared directly (see mixture_fit_quality).
# For a Weibull component mu and sigma are reported as its median and the
# standard deviation of log(gap), so it can be ordered and labelled alongside
# the normal ones; shape and scale are returned as well.
fit_normal_mixture <- function(x, k = 2, w = NULL, family = c("normal", "weibull"),
                               max_iter = 1000, tol = 1e-7) {
  family <- match.arg(family)
  if (is.null(w)) w <- rep(1, length(x))
  if (length(w) != length(x)) stop("w must be the same length as x")

  keep <- is.finite(x) & is.finite(w) & w > 0
  x <- x[keep]
  w <- w[keep]
  n <- length(x)
  if (n < k * 2) return(NULL)

  W       <- sum(w)
  w_mean  <- sum(w * x) / W
  w_sd    <- sqrt(sum(w * (x - w_mean)^2) / W)
  if (!is.finite(w_sd) || w_sd <= 0) return(NULL)

  # Initialise means from quantiles, equal weights, global sd
  mu     <- as.numeric(quantile(x, probs = (seq_len(k) - 0.5) / k))
  sigma  <- rep(w_sd, k)
  lambda <- rep(1 / k, k)
  ll_old <- -Inf

  # Lower bound on the component widths. Feeding times are usually recorded
  # only to the minute, so a large share of the log-intervals land on exactly
  # the same value (log 60). Without a floor a component shrinks onto that
  # atom (sigma -> 0, the classic EM variance collapse), which produces a
  # spike of near-infinite density, a meaningless threshold and a figure in
  # which the data itself is flattened to a line.
  sigma_min <- max(1e-3, 0.05 * w_sd)

  # Collapse repeated values before iterating. Every sum in the loop below
  # reaches x only through the distinct values and the weight standing behind
  # each of them, so adding the weights of equal values up front gives the
  # same fit from far less arithmetic. Intervals are whole seconds, so a
  # season of records holds only tens of thousands of distinct log-intervals
  # out of hundreds of thousands of rows. The starting values above are still
  # taken from the full sample, so only the arithmetic changes, not the fit.
  if (anyDuplicated(x)) {
    agg <- data.table(x = x, w = w)[, .(w = sum(w)), by = x]
    xf <- agg$x
    wf <- agg$w
  } else {
    xf <- x
    wf <- w
  }

  fams <- rep("normal", k)
  shape <- NA_real_
  scale <- NA_real_
  tf <- NULL
  if (family == "weibull") {
    fams[1] <- "weibull"
    tf <- exp(xf)                       # the gaps themselves, in seconds
    # Start from the Weibull that matches the first component's starting
    # normal: equal spread of log(gap) and the same median.
    shape <- (pi / sqrt(6)) / sigma[1]
    scale <- exp(mu[1]) / log(2)^(1 / shape)
  }

  dens <- matrix(0, length(xf), k)
  for (iter in seq_len(max_iter)) {
    # E-step: responsibilities
    for (j in seq_len(k)) {
      dens[, j] <- if (fams[j] == "weibull")
        lambda[j] * dweibull(tf, shape, scale) * tf
      else
        lambda[j] * dnorm(xf, mu[j], sigma[j])
    }
    dens[dens < 1e-300] <- 1e-300
    tot  <- rowSums(dens)
    resp <- dens / tot

    # M-step, with every sum taken over the observation weights
    rw     <- resp * wf
    Nk     <- colSums(rw)
    lambda <- Nk / W
    for (j in seq_len(k)) {
      if (fams[j] == "weibull") {
        wb <- fit_weibull_weighted(tf, rw[, j], start = shape)
        if (!is.null(wb)) {
          shape <- wb$shape
          scale <- wb$scale
        }
        mu[j]    <- log(scale) + log(log(2)) / shape   # median, on the log axis
        sigma[j] <- (pi / sqrt(6)) / shape             # sd of log(gap)
      } else {
        mu[j]    <- sum(rw[, j] * xf) / Nk[j]
        sigma[j] <- sqrt(sum(rw[, j] * (xf - mu[j])^2) / Nk[j])
        if (!is.finite(sigma[j]) || sigma[j] < sigma_min) sigma[j] <- sigma_min
      }
    }

    ll <- sum(wf * log(tot))
    if (is.finite(ll) && abs(ll - ll_old) < tol) break
    ll_old <- ll
  }

  ord <- order(mu)
  list(lambda = lambda[ord], mu = mu[ord], sigma = sigma[ord],
       loglik = ll, k = k, n = n, weighted = !all(w == w[1]),
       family = family, families = fams[ord], shape = shape, scale = scale)
}


# The meal criterion: the point where the highest component stops being the
# likelier explanation of a gap. The component with the largest mean is the
# between-meal population; every component below it describes intervals inside
# a meal (with three components those are the within-visit and between-visit
# intervals, which is the classic three-population meal model). The criterion
# is therefore always taken between the top two components, so raising k
# splits the within-meal end of the distribution without dragging the
# threshold down with it. With k = 2 this is the familiar two-population meal
# criterion, unchanged.
#
# Solving lambda1*N(x|mu1,s1) = lambda2*N(x|mu2,s2) for those two components
# reduces to a quadratic in x; the root between the two means is the
# threshold, and intervals below it belong to the same meal.
mixture_intersection <- function(fit) {
  if (is.null(fit) || fit$k < 2) return(NA_real_)

  j_top <- length(fit$mu)      # between-meal population
  j_sub <- j_top - 1L          # last within-meal population

  # A Weibull component has no closed-form crossing with a normal, so when
  # either of the two is not normal the same point is found numerically: the
  # crossing is where one weighted component density overtakes the other,
  # which is a sign change of their log difference between the two centres.
  fams <- if (is.null(fit$families)) rep("normal", length(fit$mu)) else fit$families
  if (!all(fams[c(j_sub, j_top)] == "normal")) {
    lo <- min(fit$mu[j_sub], fit$mu[j_top])
    hi <- max(fit$mu[j_sub], fit$mu[j_top])
    gap <- function(z) {
      d <- component_densities(fit, z)
      log(pmax(d[, j_sub], 1e-300)) - log(pmax(d[, j_top], 1e-300))
    }
    if (!is.finite(lo) || !is.finite(hi) || hi <= lo) return((lo + hi) / 2)
    if (gap(lo) * gap(hi) > 0) return((lo + hi) / 2)
    return(uniroot(gap, c(lo, hi), tol = 1e-8)$root)
  }

  mu1 <- fit$mu[j_sub];     mu2 <- fit$mu[j_top]
  s1  <- fit$sigma[j_sub];  s2  <- fit$sigma[j_top]
  l1  <- fit$lambda[j_sub]; l2  <- fit$lambda[j_top]

  a <- 1 / (2 * s2^2) - 1 / (2 * s1^2)
  b <- mu1 / s1^2 - mu2 / s2^2
  K <- log(l2 / s2) - log(l1 / s1)
  cc <- mu2^2 / (2 * s2^2) - mu1^2 / (2 * s1^2) - K

  lo <- min(mu1, mu2); hi <- max(mu1, mu2)

  if (abs(a) < 1e-12) {                 # equal variances -> linear
    if (abs(b) < 1e-12) return((mu1 + mu2) / 2)
    return(-cc / b)
  }

  disc <- b^2 - 4 * a * cc
  if (disc < 0) return((mu1 + mu2) / 2)

  roots <- (-b + c(-1, 1) * sqrt(disc)) / (2 * a)
  between <- roots[roots >= lo & roots <= hi]
  if (length(between) >= 1) return(between[1])
  roots[which.min(abs(roots - (mu1 + mu2) / 2))]
}


# Assign a per-animal meal (visit) ID. Within each animal, ordered by time,
# the ID starts at 1 and increments whenever the gap to the previous event is
# at least the threshold (a new meal begins). Gaps below the threshold keep
# the current ID (same meal). threshold_log is the intersection on the log
# scale; it is compared against IntervalSec via exp().
assign_visits <- function(df, threshold_log) {
  df <- as.data.table(copy(df))
  # Same reason as in compute_meal_intervals: a re-uploaded file may already
  # carry a Visits column, which would swallow the meal ids computed here.
  if ("Visits" %in% names(df)) df[, Visits := NULL]
  setorder(df, AnimalTag, StartTime)
  thr_sec <- if (is.finite(threshold_log)) exp(threshold_log) else Inf

  df[, Visits := {
    n <- .N
    v <- integer(n)
    if (n > 0) {
      v[1] <- 1L
      if (n > 1) {
        for (i in 2:n) {
          gap <- IntervalSec[i]
          v[i] <- if (is.na(gap) || gap >= thr_sec) v[i - 1] + 1L else v[i - 1]
        }
      }
    }
    v
  }, by = AnimalTag]
  df[]
}


# Short human-readable label for a number of seconds ("45 s", "3 min", "2 h",
# "1.5 d"), used for axis breaks and annotations on the interval plots.
fmt_interval <- function(sec) {
  vapply(sec, function(s) {
    if (!is.finite(s))  NA_character_
    else if (s < 60)    paste0(signif(s, 2), " s")
    else if (s < 3600)  paste0(signif(s / 60, 2), " min")
    else if (s < 86400) paste0(signif(s / 3600, 2), " h")
    else                paste0(signif(s / 86400, 2), " d")
  }, character(1))
}


# Plot the log-interval histogram, the fitted normal components and the
# threshold intersection. Two things otherwise make the figure unreadable:
# log-seconds are impossible to read off (the axis is therefore labelled in
# real time units), and feeding times recorded to the minute pile a third or
# more of the gaps onto the single smallest possible interval, so that one bar
# - and any component fitted to it - towers over the rest of the distribution
# and flattens it to a line. The y axis is scaled to the rest of the data and
# the caption reports what runs off the top.
#
# `weights` must be the same weights handed to fit_normal_mixture (mass
# consumed, typically): the bars are weighted alongside the curves, otherwise
# the fitted mixture would not describe the histogram drawn underneath it.
# `dequantised` only annotates the figure: it records that the intervals were
# spread across their rounding step before the fit, so the reader knows the
# bars are no longer the raw recorded values.
plot_meal_mixture <- function(log_intervals, fit, threshold_log, weights = NULL,
                              dequantised = FALSE) {
  keep <- is.finite(log_intervals)
  if (!is.null(weights)) keep <- keep & is.finite(weights) & weights > 0
  x  <- log_intervals[keep]
  wt <- if (is.null(weights)) rep(1, length(x)) else weights[keep]

  # A zero-width range (every interval identical) leaves nothing to bin, and
  # every histogram break would collapse onto the same value.
  if (length(x) == 0 || is.null(fit) || diff(range(x)) == 0) {
    return(ggplot() + geom_blank() +
             labs(title = "Not enough data to fit a meal-interval mixture") +
             theme_minimal())
  }

  weighted <- !is.null(weights)
  df_hist <- data.frame(LogInterval = x, wt = wt)
  bins <- 60
  brks <- seq(min(x), max(x), length.out = bins + 1)
  xs   <- seq(min(x), max(x), length.out = 512)

  fams  <- if (is.null(fit$families)) rep("normal", length(fit$mu)) else fit$families
  dens_j <- component_densities(fit, xs)

  comps <- do.call(rbind, lapply(seq_along(fit$mu), function(j) {
    data.frame(x = xs,
               density   = dens_j[, j],
               Component = paste0("Part ", j, ": ", fmt_interval(exp(fit$mu[j])),
                                  " gaps, ", round(100 * fit$lambda[j]), "% of the data",
                                  if (fams[j] == "weibull") " (Weibull)" else ""))
  }))
  # The combined curve - the one that should follow the bars. Each component
  # on its own only carries its share of the data, so it sits below them.
  mix <- data.frame(x = xs, density = rowSums(dens_j))

  # Bar heights on the same (weighted) density scale ggplot draws them on, so
  # the y limit below is computed from exactly what is plotted.
  bin_of   <- cut(x, breaks = brks, include.lowest = TRUE)
  bin_wt   <- tapply(wt, bin_of, sum)
  bin_wt[is.na(bin_wt)] <- 0
  bin_dens <- as.numeric(bin_wt) / (sum(wt) * diff(brks))

  # Scale to the body of the distribution, not to its tallest spike. Because
  # the recorded times are discrete (whole minutes), the few shortest gaps sit
  # on isolated log values and produce bars many times higher than everything
  # else; scaling to them is what flattened the figure. The reference height
  # is therefore a high quantile of the occupied bars together with the peak
  # of the mixture beyond the threshold, and the tall bars are simply clipped.
  atom_share <- sum(wt[x == min(x)]) / sum(wt)
  occupied   <- bin_dens[bin_dens > 0]
  ref        <- as.numeric(stats::quantile(occupied, 0.9, na.rm = TRUE))
  tail_peak  <- if (is.finite(threshold_log))
                  max(mix$density[xs >= threshold_log], na.rm = TRUE) else 0
  y_max      <- 2 * max(ref, tail_peak, na.rm = TRUE)
  clipped    <- sum(bin_dens > y_max)

  # Breaks in real time units; the log values stay available on the top axis.
  cand <- c(30, 60, 120, 300, 600, 1800, 3600, 2 * 3600, 6 * 3600,
            12 * 3600, 86400, 2 * 86400, 7 * 86400)
  # A hair of tolerance so a break sitting on the very first or last value
  # (the recording step itself, typically) is not dropped.
  cand <- cand[log(cand) >= min(x) - 0.05 & log(cand) <= max(x) + 0.05]
  if (length(cand) < 2) cand <- exp(pretty(x, 6))

  p <- ggplot() +
    geom_histogram(data = df_hist,
                   aes(x = LogInterval, y = after_stat(density), weight = wt),
                   breaks = brks, fill = "grey85", colour = "white") +
    geom_line(data = comps, aes(x = x, y = density, colour = Component),
              linewidth = 0.7, linetype = "22") +
    geom_line(data = mix, aes(x = x, y = density),
              colour = "black", linewidth = 1.2)

  if (is.finite(threshold_log)) {
    p <- p +
      geom_vline(xintercept = threshold_log, linetype = "dashed",
                 colour = "red", linewidth = 0.8) +
      annotate("label", x = threshold_log, y = y_max * 0.92,
               label = paste0("Meal threshold: ",
                              fmt_interval(exp(threshold_log))),
               colour = "red", hjust = -0.06, size = 3.4, label.size = 0)
  }

  # Say plainly what the eye cannot check: how closely the combined curve
  # follows the bars, how much of the data sits on the single shortest
  # interval, and which bars were cut off by the y limit.
  gof <- mixture_fit_quality(log_intervals, weights, fit, bins = bins)

  notes <- character(0)
  if (!is.null(gof) && is.finite(gof$bic)) {
    notes <- c(notes, paste0("BIC ", format(round(gof$bic), big.mark = ","),
                             " - lower is better, for comparing this against ",
                             "other numbers of parts or the other curve shape ",
                             "on the same data."))
  }
  if (atom_share >= 0.05) {
    notes <- c(notes, paste0(round(100 * atom_share), "% of the ",
                             if (weighted) "mass" else "gaps",
                             " sits on gaps of exactly ",
                             fmt_interval(exp(min(x))),
                             " because the times are recorded to the minute."))
  }
  if (clipped > 0) {
    notes <- c(notes, paste0("The ", clipped,
                             if (clipped == 1) " tallest bar" else " tallest bars",
                             " at the short-interval end (up to ",
                             round(max(bin_dens) / y_max, 1),
                             "x the panel height) ",
                             if (clipped == 1) "is" else "are",
                             " clipped so the rest of the distribution stays readable."))
  }
  caption <- if (length(notes))
    paste(strwrap(paste(notes, collapse = " "), width = 110), collapse = "\n") else NULL

  # Wrapped so it cannot run off the panel on a narrow screen.
  subtitle <- paste0("Grey bars = ",
                     if (weighted) "mass eaten after each gap" else "observed gaps",
                     if (dequantised) ", spread across the recording step" else "",
                     "; black line = the fitted curve, which should follow them; ",
                     "dashed colours = its ", length(fit$mu), " parts; ",
                     "dashed red = meal threshold.",
                     if (!is.null(gof) && is.finite(gof$r2))
                       paste0(" The curve follows the bars ",
                              round(100 * max(gof$r2, 0)), "% of the way.") else "")
  subtitle <- paste(strwrap(subtitle, width = 95), collapse = "\n")

  p +
    scale_x_continuous(breaks = log(cand), labels = fmt_interval(cand),
                       sec.axis = sec_axis(~ ., name = "log(interval, seconds)")) +
    coord_cartesian(ylim = c(0, y_max)) +
    labs(title = paste("Meal-Interval Distribution with Fitted Mixture",
                       if (weighted) "(weighted by mass consumed)" else ""),
         subtitle = subtitle,
         x = "Interval between feeding events (log scale)",
         y = if (weighted) "Density (per kg consumed)" else "Density",
         colour = "", caption = caption) +
    theme_minimal() +
    theme(legend.position = "bottom",
          plot.caption = element_text(hjust = 0, colour = "grey30", size = 8))
}


# Collapse the feeding records into one row per meal. A meal is a run of
# events sharing a Visits id: it starts when its first event starts, ends when
# its last event ends, and pools their time on the feeder and their mass. The
# distinction that matters is between the meal's span (which includes the
# short pauses inside it) and the time actually spent eating.
summarize_meals <- function(df) {
  # Take a copy of only the columns needed, which leaves the caller's table
  # alone without duplicating the twenty-odd columns an upload can carry.
  cols <- c("AnimalTag", "Visits", "StartTime", "EndTime", "DurationSec",
            "MassDiffKg", "Feeder")
  d <- as.data.table(df)[, ..cols]
  if (!inherits(d$StartTime, "POSIXct")) d[, StartTime := as.POSIXct(StartTime)]
  if (!inherits(d$EndTime,   "POSIXct")) d[, EndTime   := as.POSIXct(EndTime)]
  tz <- attr(d$StartTime, "tzone")
  if (is.null(tz)) tz <- ""

  # Aggregate over plain numbers. min/max/sum/.N on a numeric column take
  # data.table's optimised grouping path, whereas the same expressions wrapped
  # in difftime(), as.Date() or uniqueN() - or taken over POSIXct - fall back
  # to running R code once per group. With a meal per group that is >100k
  # evaluations on a season of data, which is where the wait came from.
  d[, `:=`(st_num = as.numeric(StartTime), en_num = as.numeric(EndTime))]

  meals <- d[, .(st_num = min(st_num),
                 en_num = max(en_num),
                 Events = .N,
                 eat_sec = sum(DurationSec, na.rm = TRUE),
                 mass_kg = sum(MassDiffKg, na.rm = TRUE)),
             by = .(AnimalTag, Meal = Visits)]

  # Feeders per meal: one pass over the distinct animal/meal/feeder triples
  # instead of a uniqueN() call inside every group.
  feeders <- unique(d[, .(AnimalTag, Meal = Visits, Feeder)])[
    , .(Feeders = .N), by = .(AnimalTag, Meal)]
  meals[feeders, Feeders := i.Feeders, on = .(AnimalTag, Meal)]

  # Everything else is a vectorised pass over the meals, not over the records.
  meals[, `:=`(
    StartTime           = as.POSIXct(st_num, origin = "1970-01-01", tz = tz),
    EndTime             = as.POSIXct(en_num, origin = "1970-01-01", tz = tz),
    `Meal Length (min)` = round((en_num - st_num) / 60, 1),
    `Eating Time (min)` = round(eat_sec / 60, 1),
    `Mass (Kg)`         = round(mass_kg, 3)
  )]
  meals[, Date := as.Date(StartTime)]
  meals[, c("st_num", "en_num", "eat_sec", "mass_kg") := NULL]

  setcolorder(meals, c("AnimalTag", "Meal", "Date", "StartTime", "EndTime",
                       "Events", "Meal Length (min)", "Eating Time (min)",
                       "Mass (Kg)", "Feeders"))
  setorder(meals, AnimalTag, StartTime)
  meals[]
}


# Per-animal summary of those meals: how many, how often, and what an average
# meal looks like. Covers every animal in the dataset.
summarize_visits <- function(df) {
  summarize_meal_averages(summarize_meals(df))
}


# The averaging half of the summary, split out so a caller that already holds
# the per-meal table (the app builds it once for both the table and its
# download) does not pay for collapsing the records a second time.
summarize_meal_averages <- function(meals) {
  meals <- as.data.table(meals)

  out <- meals[, .(
    Meals                      = .N,
    Days                       = uniqueN(Date),
    `Events/Meal`              = round(mean(Events), 1),
    `Meal Length (min)`        = round(mean(`Meal Length (min)`), 1),
    `Eating Time/Meal (min)`   = round(mean(`Eating Time (min)`), 1),
    `Mass/Meal (Kg)`           = round(mean(`Mass (Kg)`), 3),
    `Total Mass Consumed (Kg)` = round(sum(`Mass (Kg)`), 3)
  ), by = AnimalTag]

  out[, `Meals/Day` := round(Meals / Days, 2)]
  out[, `Mass/Day (Kg)` := round(`Total Mass Consumed (Kg)` / Days, 3)]
  setcolorder(out, c("AnimalTag", "Meals", "Days", "Meals/Day", "Events/Meal",
                     "Meal Length (min)", "Eating Time/Meal (min)",
                     "Mass/Meal (Kg)", "Mass/Day (Kg)",
                     "Total Mass Consumed (Kg)"))
  setorder(out, AnimalTag)
  out[]
}


# Per-animal figure: number of visits (meals) per day for one animal.
plot_animal_visits <- function(df, animal_tag) {
  df <- as.data.table(copy(df))
  if (!inherits(df$StartTime, "POSIXct")) df[, StartTime := as.POSIXct(StartTime)]
  df <- df[AnimalTag == animal_tag]

  if (nrow(df) == 0) {
    return(ggplot() + geom_blank() +
             labs(title = "No data for the selected animal") +
             theme_minimal())
  }

  daily <- df[, .(Visits = uniqueN(Visits),
                  Mass   = round(sum(MassDiffKg, na.rm = TRUE), 2)),
              by = .(Date = as.Date(StartTime))]
  setorder(daily, Date)

  ggplot(daily, aes(x = Date, y = Visits)) +
    geom_col(fill = "#1f77b4", width = 0.7) +
    geom_text(aes(label = Visits), vjust = -0.3, size = 3) +
    scale_x_date(date_labels = "%b %d") +
    labs(title = paste("Daily Visits (Meals) -", animal_tag),
         subtitle = "Bars show the number of distinct meals per day",
         x = "Date", y = "Number of visits (meals)") +
    theme_minimal()
}


# ---------------------------------------------------------------------------
# Animal identifier column
# ---------------------------------------------------------------------------

# Rank the uploaded column names by how likely each is to hold the animal
# identifier, so the ID-column dropdown can offer the obvious candidates
# first. Names matching a known ID pattern are "suggested" (best-matching
# pattern first, alphabetical within a pattern); every remaining column is
# listed afterwards alphabetically, so any column can still be chosen.
id_column_choices <- function(cols) {
  cols <- unique(cols[!is.na(cols) & nzchar(cols)])
  if (length(cols) == 0) return(list(suggested = character(0), others = character(0)))

  patterns <- c("^animal_?tag$", "^animal_?id$", "^(e|rf|visual|ear)_?id$",
                "^tag(_?id)?$", "^animal", "tag", "transponder", "rfid",
                "id$", "(^|[^[:alnum:]])id([^[:alnum:]]|$)")

  score <- vapply(cols, function(x) {
    hit <- which(vapply(patterns,
                        function(p) grepl(p, x, ignore.case = TRUE),
                        logical(1)))
    if (length(hit)) hit[1] else NA_integer_
  }, integer(1))

  suggested <- cols[!is.na(score)]
  suggested <- suggested[order(score[!is.na(score)], tolower(suggested))]
  others    <- sort(cols[is.na(score)])

  list(suggested = suggested, others = others)
}
