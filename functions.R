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


# Fit a k-component univariate Gaussian mixture by EM (base R; no external
# dependencies). Returns components ordered by increasing mean, with the
# mixing weights (lambda), means (mu), standard deviations (sigma) and the
# final log-likelihood.
fit_normal_mixture <- function(x, k = 2, max_iter = 1000, tol = 1e-7) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < k * 2) return(NULL)

  # Initialise means from quantiles, equal weights, global sd
  mu     <- as.numeric(quantile(x, probs = (seq_len(k) - 0.5) / k))
  sigma  <- rep(sd(x), k)
  lambda <- rep(1 / k, k)
  ll_old <- -Inf

  for (iter in seq_len(max_iter)) {
    # E-step: responsibilities
    dens <- sapply(seq_len(k), function(j) lambda[j] * dnorm(x, mu[j], sigma[j]))
    dens[dens < 1e-300] <- 1e-300
    tot  <- rowSums(dens)
    resp <- dens / tot

    # M-step
    Nk     <- colSums(resp)
    lambda <- Nk / n
    mu     <- colSums(resp * x) / Nk
    sigma  <- sqrt(colSums(resp * (outer(x, mu, "-")^2)) / Nk)
    sigma[!is.finite(sigma) | sigma < 1e-3] <- 1e-3

    ll <- sum(log(tot))
    if (is.finite(ll) && abs(ll - ll_old) < tol) break
    ll_old <- ll
  }

  ord <- order(mu)
  list(lambda = lambda[ord], mu = mu[ord], sigma = sigma[ord],
       loglik = ll, k = k, n = n)
}


# Intersection point of the first two mixture components (the lower two by
# mean). Solving lambda1*N(x|mu1,s1) = lambda2*N(x|mu2,s2) reduces to a
# quadratic in x. The root that falls between the two means is the meal
# threshold: intervals below it belong to the same meal.
mixture_intersection <- function(fit) {
  if (is.null(fit) || fit$k < 2) return(NA_real_)

  mu1 <- fit$mu[1];     mu2 <- fit$mu[2]
  s1  <- fit$sigma[1];  s2  <- fit$sigma[2]
  l1  <- fit$lambda[1]; l2  <- fit$lambda[2]

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


# Plot the log-interval histogram, the fitted normal components and the
# threshold intersection.
plot_meal_mixture <- function(log_intervals, fit, threshold_log) {
  x <- log_intervals[is.finite(log_intervals)]
  if (length(x) == 0 || is.null(fit)) {
    return(ggplot() + geom_blank() +
             labs(title = "Not enough data to fit a meal-interval mixture") +
             theme_minimal())
  }

  df_hist <- data.frame(LogInterval = x)
  bins <- 50
  xs <- seq(min(x), max(x), length.out = 512)

  comps <- do.call(rbind, lapply(seq_along(fit$mu), function(j) {
    data.frame(x = xs,
               density   = fit$lambda[j] * dnorm(xs, fit$mu[j], fit$sigma[j]),
               Component = paste0("Component ", j,
                                  " (mu=", round(fit$mu[j], 2), ")"))
  }))
  # Combined mixture density (the curve the histogram should follow)
  mix <- data.frame(
    x = xs,
    density = rowSums(sapply(seq_along(fit$mu), function(j)
      fit$lambda[j] * dnorm(xs, fit$mu[j], fit$sigma[j])))
  )

  # A single narrow component can spike far above the data; clip the y-axis to
  # what the histogram actually reaches so the distribution stays readable.
  hist_max <- max(hist(x, breaks = bins, plot = FALSE)$density)
  y_max <- max(hist_max, max(mix$density)) * 1.1

  p <- ggplot() +
    geom_histogram(data = df_hist,
                   aes(x = LogInterval, y = after_stat(density)),
                   bins = bins, fill = "grey85", colour = "white") +
    geom_line(data = comps, aes(x = x, y = density, colour = Component),
              linewidth = 0.9) +
    geom_line(data = mix, aes(x = x, y = density),
              colour = "black", linewidth = 0.7, linetype = "dotted")

  if (is.finite(threshold_log)) {
    p <- p +
      geom_vline(xintercept = threshold_log, linetype = "dashed",
                 colour = "red", linewidth = 0.8) +
      annotate("label", x = threshold_log, y = y_max * 0.95,
               label = paste0("Threshold = ", round(threshold_log, 2),
                              "\n(", round(exp(threshold_log)), " s)"),
               colour = "red", hjust = -0.05, size = 3.2, label.size = 0)
  }

  p +
    coord_cartesian(ylim = c(0, y_max)) +
    labs(title = "Log Meal-Interval Distribution with Fitted Mixture",
         subtitle = "Dotted = combined mixture; coloured = components; dashed red = meal threshold",
         x = "log(interval between meals, seconds)", y = "Density",
         colour = "") +
    theme_minimal()
}


# Per-animal summary: number of meals (visits), days observed, visits per day
# and total mass consumed. Covers every animal in the dataset.
summarize_visits <- function(df) {
  df <- as.data.table(copy(df))
  if (!inherits(df$StartTime, "POSIXct")) df[, StartTime := as.POSIXct(StartTime)]

  out <- df[, .(
    Visits = uniqueN(Visits),
    Days   = uniqueN(as.Date(StartTime)),
    `Total Mass Consumed (Kg)` = round(sum(MassDiffKg, na.rm = TRUE), 3)
  ), by = AnimalTag]

  out[, `Visits/Day` := round(Visits / Days, 2)]
  setcolorder(out, c("AnimalTag", "Visits", "Days", "Visits/Day",
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
