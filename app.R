library(shiny)
#library(DT)
library(data.table)
library(ggplot2)
library(dplyr)
library(lubridate)
library(plotly)
library(readxl)
library(winch)
source('functions.R')
options(shiny.maxRequestSize = 1000*1024^2) #10 GB to upload

ui <- fluidPage(
  # Working indicator ####
  # Large uploads take a while with nothing on screen to say so. Shiny's own
  # busy indicators put a pulse across the top of the page and a spinner on
  # each output that is recalculating; the badge adds one unmissable marker
  # that stays visible while anything at all is still running.
  useBusyIndicators(),
  tags$head(tags$style(HTML("
    #busy_badge {
      display: none; position: fixed; top: 10px; right: 14px; z-index: 2000;
      background: #1f77b4; color: #fff; padding: 6px 14px; border-radius: 14px;
      font-weight: 600; box-shadow: 0 1px 5px rgba(0, 0, 0, 0.35);
    }
    html.shiny-busy #busy_badge { display: block; }
  "))),
  div(id = "busy_badge", icon("spinner", class = "fa-spin"), "Working ..."),

  titlePanel("Animal Feeding Dashboard"),
 # sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV or Excel"),
      uiOutput("sheet_ui"),
      uiOutput("id_col_ui"),
      uiOutput("animal_selector"),
      #dateRangeInput("date_range", "Select Date Range")
      uiOutput("date_ui"),
      numericInput("tolerance_sec", "Overlap Tolerance (seconds)", value = 0, min = 0, step = 1),

      uiOutput("overlap_ui"),

      # Meal settings ####
      # Grouped in their own panel, and worded for someone who has never met a
      # mixture model: every label says what the setting does to the data, not
      # what it does to the statistics.
      wellPanel(
        h4("Meal settings"),
        helpText(paste("A meal is a run of feeding events with only short breaks",
                       "in between. These settings decide where one meal ends",
                       "and the next one starts.")),

        checkboxInput("weight_by_mass",
                      "Let big feedings count more than small ones",
                      value = FALSE),
        helpText("Uses how much the animal actually ate, so a large feeding",
                 "counts for more than a quick nibble."),

        selectInput("mixture_k", "How many kinds of break to look for",
                    choices = c("2 - breaks within a meal, and breaks between meals" = 2,
                                "3 - also separates short pauses inside a meal"      = 3,
                                "4 - for very detailed data"                         = 4),
                    selected = 2),
        helpText("Start with 2. Try 3 if the animals pause in the middle of a",
                 "meal and those pauses are being counted as new meals."),

        selectInput("mixture_family", "Shape of the short-break curve",
                    choices = c("Even on both sides"       = "normal",
                                "Leaning to one side"      = "weibull"),
                    selected = "normal"),
        helpText("Short breaks are often bunched near the low end rather than",
                 "spread evenly around a middle value. The leaning shape can",
                 "follow that better - the fit score on the figure tells you",
                 "which one suits your data."),

        helpText(tags$em(paste("Times recorded only to the whole minute are",
                               "detected and handled automatically.")))
      ),

      downloadButton("download_data", "Download Resolved Dataset"),
      downloadButton("download_visits", "Download Meal Summary"),
      downloadButton("download_meals", "Download One Row Per Meal")


    ),
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Feeding",
          plotlyOutput("feeding_plot"),
          tableOutput("summary_table"),
          plotOutput("accumulated_plot"),
          plotOutput("total_mass_plot")
        ),
        tabPanel(
          "Overlaps",
          #tableOutput("overlap_table"),
          DT::dataTableOutput("overlap_table")
        ),
        tabPanel(
          "Meals",
          h4("How meals were separated"),
          textOutput("meal_criterion"),
          plotOutput("meal_mixture_plot"),
          h4("Daily Meals (selected animal)"),
          plotOutput("animal_visits_plot"),
          h4("Meal Summary (all animals)"),
          tableOutput("visit_summary")
        )
      )
      #tableOutput("debug")

    )
  )
#)


server <- function(input, output, session) {
 
  # Raw upload, read before the animal identifier is known: the ID column is
  # chosen by the user, so reading must not assume a particular column name.
  # Sheet names of an uploaded workbook; empty for a text file.
  excel_sheet_names <- reactive({
    req(input$file)
    if (tolower(tools::file_ext(input$file$name)) %in% c("xlsx", "xls")) {
      excel_sheets(input$file$datapath)
    } else {
      character(0)
    }
  })

  # Only worth asking when the workbook actually holds more than one sheet.
  output$sheet_ui <- renderUI({
    sheets <- excel_sheet_names()
    if (length(sheets) < 2) return(NULL)
    selectInput("sheet", "Which sheet to read",
                choices = sheets, selected = sheets[1])
  })

  raw_data <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name)

    if (tolower(ext) %in% c("xlsx", "xls")) {
      # Read the chosen sheet, falling back to the first one before the
      # selector has rendered (or when there is only one sheet to read).
      sheets <- excel_sheet_names()
      sheet  <- if (!is.null(input$sheet) && input$sheet %in% sheets) input$sheet
                else sheets[1]
      as.data.table(read_excel(input$file$datapath, sheet = sheet))
    } else {
      # Read text-based file and find header line. Files that do not carry an
      # "AnimalTag" column are read from the first line; their identifier
      # column is picked in the dropdown below.
      lines <- readLines(input$file$datapath)
      start_line <- which(grepl("AnimalTag", lines))

      if (length(start_line) == 0 || start_line[1] == 1) {
        fread(input$file$datapath)
      } else {
        fread(input$file$datapath, skip = start_line[1] - 1)
      }
    }
  })

  # Column holding the animal identifier: likely candidates are suggested
  # first, but every uploaded column stays selectable.
  output$id_col_ui <- renderUI({
    req(raw_data())
    ch <- id_column_choices(names(raw_data()))
    choices <- list()
    if (length(ch$suggested)) choices[["Suggested"]]     <- ch$suggested
    if (length(ch$others))    choices[["Other columns"]] <- ch$others

    selectInput("id_col", "Animal Identifier Column",
                choices  = choices,
                selected = c(ch$suggested, ch$others)[1])
  })

  data <- reactive({
    id_col <- req(input$id_col)
    dt <- copy(req(raw_data()))
    validate(need(id_col %in% names(dt),
                  "Selected identifier column is not in the uploaded file."))

    # The rest of the app keys on AnimalTag, so map the chosen column onto it
    # and keep any pre-existing AnimalTag column under a distinct name.
    if (!identical(id_col, "AnimalTag")) {
      if ("AnimalTag" %in% names(dt))
        setnames(dt, "AnimalTag", "AnimalTag_original")
      dt[, AnimalTag := get(id_col)]
    }

    # Parse datetime columns, reconciling any StartTime/EndTime timezone
    # mismatch (e.g. local StartTime vs. UTC "Z" EndTime) via DurationSec.
    dt <- parse_feeding_times(dt)
    dt[, AnimalTag := as.character(AnimalTag)]
    dt[, Feeder := as.character(Feeder)]

    dt
  })
  #Get date to display
  output$date_ui <- renderUI({
    req(data())
    if (nrow(data()) == 0) return(NULL)  # prevents rendering empty table
    min_date <- min(data()$StartTime, na.rm = TRUE)
    max_date <- max(data()$StartTime, na.rm = TRUE)
    
    dateRangeInput("date_range", "Select Date Range",
                   start = min_date,
                   end = max_date,
                   min = min_date,
                   max = max_date)
  })
  
  
  output$animal_selector <- renderUI({
    req(data())
    selectInput("animal_tag", "Animal Tag", choices = unique(data()$AnimalTag))
  })
  
  # filtered_data <- reactive({
  #   req(input$animal_tag, input$date_range)
  #   df <- data()[AnimalTag == input$animal_tag]
  #   df[StartTime >= input$date_range[1] & StartTime <= input$date_range[2]]
  # })
  filtered_data <- reactive({
    req(input$animal_tag, input$date_range)
    
    df <- data()#[AnimalTag == input$animal_tag]
    #Define overlap
    
    df = identify_overlap(df, input)
    
    
    df
  })
  
  
  mass_summary <- reactive({
    df <- filtered_data()[order(StartTime)] %>%
      filter(AnimalTag == input$animal_tag)
    if (nrow(df) == 0) return(NULL)  # prevents rendering empty table
    #df[, overlap := shift(EndTime, type = "lag") > StartTime]
    
    # Merge scenario
    
    # merge_df[, MassDiffKg := ifelse(overlap_flag & MassDiffKg < shift(MassDiffKg), 0, MassDiffKg)]
    # merge_df[, MassDiffKg := MassDiffKg + ifelse(shift(overlap_flag) & shift(MassDiffKg) < MassDiffKg, shift(MassDiffKg), 0)]
    # merge_total <- sum(merge_df$MassDiffKg, na.rm = TRUE)
    # Filter only rows with overlap_id
    merge_df <- df[!is.na(overlap_id)]
    
    # Aggregate by overlap_id
    merged_rows <- merge_df[, .(
      AnimalTag = first(AnimalTag),
      StartTime = min(StartTime),
      EndTime = max(EndTime),
      DurationSec = sum(DurationSec, na.rm = TRUE),
      MassDiffKg = sum(MassDiffKg, na.rm = TRUE),
      #Feeder = Feeder[which.max(MassDiffKg)]  # Feeder with largest MassDiffKg
      Feeder = paste(unique(Feeder), collapse = "|") #Add the all feeders
      
    ), by = overlap_id]
    
    merge_df = bind_rows(df[is.na(overlap_id)],merged_rows)
    merge_total <- sum(merge_df$MassDiffKg, na.rm = TRUE)
    #setorder(merge_df, AnimalTag, StartTime)
    # Delete scenario
    delete_df <- df[!(overlap_flag & MassDiffKg < shift(MassDiffKg))]
    delete_total <- sum(delete_df$MassDiffKg, na.rm = TRUE)
    #setorder(delete_total, AnimalTag, StartTime)
    
    list(merge = merge_total, delete = delete_total)
  })
  
  
  output$overlap_ui <- renderUI({
    req(mass_summary())  # Ensure reactive is ready
    ms <- mass_summary()
    
    radioButtons("overlap_action", "Handle Overlaps:",
                 choices = setNames(
                   c("original","merge", "delete"),
                   c("Original data",
                     paste0("Merge lower mass into higher (", round(ms$merge, 2), " kg)"),
                     paste0("Delete lower mass event (", round(ms$delete, 2), " kg)")
                   )
                 )
    )
  })
  
  
  output$feeding_plot <- renderPlotly({
    #df <- filtered_data()
    df <- req(resolved_data()$df) %>%
      filter(AnimalTag == input$animal_tag) %>%
      mutate(Flags = ifelse(is.na(Flags)| Flags=='','',Flags),
             overlap_id = ifelse(is.na(overlap_id),0,overlap_id))
    
    if (nrow(df) == 0) return(NULL)  # prevents rendering empty table
    overlaps = max(df$overlap_id, na.rm=T)
   #print(overlaps)
    df[,overlap_id := as.factor(overlap_id)]
    
    # Count how many shapes you need
    n_shapes <- length(unique(df$Feeder))
    
    # Generate a vector of shapes long enough
    # Shapes 0–25 are safe, distinct, and printable
    shape_values <- rep(0:25, length.out = n_shapes)
    p1 = ggplot(df, aes(x = StartTime, y = MassDiffKg),
    color='#696666') +
      geom_line() +
      labs(title = "Feed Intake Over Time", x = "Start Time", y = "Mass Diff (Kg)") +
      
      theme_minimal()
    
    if (overlaps>0) {
      p =p1 +geom_point(aes( color = overlap_id,shape=factor(Feeder)), size = 2) +
        scale_shape_manual(values = shape_values) +
        labs(col='Overlap and Feeder',fill='',
             shape = '')
        
    
    } else {
      p = p1 +geom_point(aes(color = Flags), size = 1, show.legend = F) +
        labs(col='Flags')
    }
    ggplotly(p)
  })
  
  
  
  #
  resolved_data <- reactive({
    req(input$overlap_action)
    df <- req(filtered_data())
    df <- df[order(StartTime)]
    if (nrow(df) == 0) return(NULL)  # prevents rendering empty table
    #df[, overlap := shift(EndTime, type = "lag") > StartTime]
    # StartTime and EndTime stay POSIXct here. They used to be formatted to
    # text at this point, which every later step had to parse back: on a large
    # upload the formatting and re-parsing cost far more than the rest of the
    # analysis put together. Text is produced where it is actually shown or
    # written out instead.
    if (input$overlap_action == "merge") {
      merge_df <- df[!is.na(overlap_id)]
      
      # merged_rows <- merge_df %>%
      #   group_by(AnimalTag, overlap_id) %>%
      #   summarise(
      #     overlap_flag = first(overlap_flag),
      #     StartTime = min(StartTime, na.rm = TRUE),
      #     EndTime = max(EndTime, na.rm = TRUE),
      #     DurationSec = sum(DurationSec, na.rm = TRUE),
      #     MassDiffKg = sum(MassDiffKg, na.rm = TRUE),
      #     Feeder = Feeder[which.max(MassDiffKg)]
      #   )
      merged_rows <- merge_df %>%
        group_by(AnimalTag, overlap_id) %>%
        mutate(
          StartTime = min(StartTime, na.rm = TRUE),
          EndTime = max(EndTime, na.rm = TRUE),
          DurationSec = sum(DurationSec, na.rm = TRUE),
          MassDiffKg = sum(MassDiffKg, na.rm = TRUE),
          StartMassKg = min(StartMassKg,na.rm=TRUE),
          EndMassKg = max(EndMassKg,na.rm=TRUE),
          Flags = 'Merged'
        ) %>%
        slice_max(MassDiffKg, n = 1, with_ties = FALSE) %>%
        ungroup()
      
      
      df = bind_rows(df[is.na(overlap_id)],merged_rows)
      setorder(df, AnimalTag, StartTime)
      list(df = df, solved_df = merge_df)
      
      } else if (input$overlap_action == "delete") {
      # df <- df[!(overlap_flag & MassDiffKg < shift(MassDiffKg)),
      #          by = overlap_id]
        
        # Separate overlapping and non-overlapping rows
        non_overlap <- df[is.na(overlap_id)]
        overlap <- df[!is.na(overlap_id)]
        
        # For each overlap group, keep only the row with the largest MassDiffKg
        overlap <- overlap[, .SD[which.max(MassDiffKg)], by = c("AnimalTag","overlap_id")]
        overlap[,Flags:='Deleted of small MassDiffKg']
        # Combine back with non-overlapping rows
        resolved_df <- rbindlist(list(non_overlap, overlap), use.names = TRUE)
        setorder(resolved_df, AnimalTag, StartTime)
        
        
        df <- resolved_df
        list(df = df, solved_df = overlap)
      } else {
        list (df=df, solved_df=df[!is.na(overlap_id)])
      }
    
  
})
  
  
  # Meal-interval analysis ####
  # Builds on the resolved (merged/deleted) dataset: computes the gap between
  # consecutive meals per animal, fits a 2-component Gaussian mixture to the
  # log-intervals (optionally weighted by the mass consumed), derives the meal
  # threshold from the component intersection and assigns per-animal visit
  # (meal) IDs.
  meal_data <- reactive({
    df <- req(resolved_data()$df)
    if (nrow(df) == 0) return(NULL)

    df <- compute_meal_intervals(df)

    # Times stored to the whole minute make every gap a multiple of 60 s, which
    # pins a component onto a zero-width spike at the rounding step. Spreading
    # each gap across the step it was rounded to feeds the fit a continuous
    # distribution; IntervalSec itself is left untouched, so the visits are
    # still assigned from the recorded gaps. The recording step is measured
    # from the gaps themselves, so a file that already carries seconds passes
    # through untouched and nobody has to know the term for any of this.
    model_sec <- dequantise_intervals(df$IntervalSec)
    step <- attr(model_sec, "grid")
    dequantised <- !is.null(step) && is.finite(step) && step > 1

    keep <- is.finite(model_sec) & model_sec > 0
    log_intervals <- log(model_sec[keep])

    # Optionally weight each interval by the mass eaten in the event that
    # closes it, so the mixture describes the gaps between kilograms consumed
    # rather than the gaps between records, and a nibble counts for less than
    # a full meal. Fall back to an unweighted fit if no mass is recorded.
    weights <- NULL
    if (isTRUE(input$weight_by_mass)) {
      weights <- pmax(df$MassDiffKg[keep], 0)
      if (!any(is.finite(weights) & weights > 0)) weights <- NULL
    }

    # More than two components let the long tail split into the several modes
    # it usually has; the meal threshold still comes from the lowest two.
    k <- suppressWarnings(as.integer(input$mixture_k))
    if (!is.finite(k) || k < 2) k <- 2

    family <- if (isTRUE(input$mixture_family == "weibull")) "weibull" else "normal"

    fit <- fit_normal_mixture(log_intervals, k = k, w = weights, family = family)
    threshold_log <- mixture_intersection(fit)

    df <- assign_visits(df, threshold_log)

    list(df = df, fit = fit, threshold_log = threshold_log,
         log_intervals = log_intervals, weights = weights,
         dequantised = dequantised, step = step,
         quality = mixture_fit_quality(log_intervals, weights, fit))
  })


  output$meal_mixture_plot <- renderPlot({
    md <- req(meal_data())
    plot_meal_mixture(md$log_intervals, md$fit, md$threshold_log, md$weights,
                      md$dequantised)
  })

  output$animal_visits_plot <- renderPlot({
    md <- req(meal_data())
    req(input$animal_tag)
    plot_animal_visits(md$df, input$animal_tag)
  })


  # Plain-language statement of the rule the whole Meals tab rests on, so the
  # numbers below it can be read without knowing what a mixture model is.
  output$meal_criterion <- renderText({
    md <- req(meal_data())
    if (is.null(md$fit) || !is.finite(md$threshold_log))
      return("There was not enough data to work out where meals start and end.")

    extras <- c(if (!is.null(md$weights)) "big feedings counted more than small ones",
                if (isTRUE(md$dequantised))
                  paste0("times found to be recorded in steps of ",
                         fmt_interval(md$step), ", so the gaps were spread across that step"))

    fit_note <- if (!is.null(md$quality) && is.finite(md$quality$r2))
      paste0(" The fitted curve follows the shape of the data ",
             round(100 * max(md$quality$r2, 0)),
             "% of the way; if that looks low, try a different number of ",
             "breaks or the other curve shape.") else ""

    paste0("Whenever an animal stopped eating for ",
           fmt_interval(exp(md$threshold_log)),
           " or longer, the next feeding was counted as a new meal. ",
           "Shorter breaks were treated as pauses within the same meal. ",
           "Worked out from ", length(md$fit$mu), " kinds of break",
           if (length(extras)) paste0(", with ", paste(extras, collapse = " and ")) else "",
           ".", fit_note)
  })

  # Meal summary covers every animal in the dataset (not just the selected
  # one); the same all-animal table is used on screen and for download.
  visit_summary_data <- reactive({
    # Averaged from the per-meal table below rather than collapsing the
    # records a second time.
    summarize_meal_averages(req(meal_rows_data()))
  })

  # One row per meal, for anyone who wants the meals themselves rather than
  # the per-animal averages.
  meal_rows_data <- reactive({
    md <- req(meal_data())
    summarize_meals(md$df)
  })

  output$visit_summary <- renderTable({
    req(visit_summary_data())
  })

  output$download_visits <- downloadHandler(
    filename = function() {
      paste0("meal_summary_", Sys.Date(), ".csv")
    },
    content = function(file) {
      fwrite(visit_summary_data(), file)
    }
  )

  output$download_meals <- downloadHandler(
    filename = function() {
      paste0("meals_", Sys.Date(), ".csv")
    },
    content = function(file) {
      fwrite(meal_rows_data(), file)
    }
  )


#Download data ####
  output$download_data <- downloadHandler(
    
    filename = function() {
      paste("resolved_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Prefer the meal-augmented dataset (adds IntervalSec, LogInterval,
      # Visits); fall back to the resolved data if the mixture could not run.
      out <- meal_data()$df
      if (is.null(out)) out <- resolved_data()$df
      fwrite(out %>%
               mutate(ShortDate = format(as.POSIXct(StartTime), "%Y-%m-%d"),
                      StartTime  = format(as.POSIXct(StartTime), "%Y-%m-%d %H:%M:%S"),
                      EndTime    = format(as.POSIXct(EndTime),   "%Y-%m-%d %H:%M:%S")) %>%
               dplyr::select(-overlap_flag,-overlap_id), file)
    }
  )
  
  
  output$debug <- renderTable({
    df <- data()[StartTime >= input$date_range[1] & StartTime <= input$date_range[2]]
    df <- df[order(StartTime)]
    df[, AccumulatedMass := cumsum(MassDiffKg), by = AnimalTag]
    
    summary_stats <- df %>%
      as.data.frame() %>%
      mutate(StartTime = floor_date(StartTime, unit = "2hour")) %>%  # Round down to minute
      group_by(StartTime) %>%
      summarise(mean = mean(AccumulatedMass, na.rm=T), 
                p25 = quantile(AccumulatedMass, 0.05, na.rm=T), 
                p75 = quantile(AccumulatedMass, 0.95, na.rm=T))
    summary_stats
  })
  
  ####3
  output$accumulated_plot <- renderPlot({
    #df <- data()[StartTime >= input$date_range[1] & StartTime <= input$date_range[2]]
    df <- resolved_data()$df
    #if (nrow(df) == 0) return(NULL)  # prevents rendering empty plot
    df[,StartTime:=as.POSIXct(StartTime)]
    df <- df[order(StartTime)]
    if (nrow(df)>0) { 
      df[, AccumulatedMass := cumsum(MassDiffKg), by = AnimalTag]
   
      #all_df <- df#data()[as.Date(StartTime) >= input$date_range[1] & as.Date(StartTime) <= input$date_range[2]] 
      
      #all_df[, AccumulatedMass := cumsum(MassDiffKg), by = AnimalTag]
      summary_stats <- df %>%
        as.data.frame() %>%
        #mutate(StartTime = floor_date(StartTime, unit = "hour")) %>%  # Round down
        mutate(
          StartTime = suppressWarnings(
            if ("StartTime" %in% names(.)) {
              if (inherits(StartTime, "POSIXt")) {
                floor_date(StartTime, unit = "4hour")
              } else {
                warning("StartTime is not a POSIXt object")
                NA
              }
            } else {
              warning("StartTime column not found")
              NA
            }
          )
        ) %>%
      
        group_by(StartTime) %>%
        summarise(mean = mean(AccumulatedMass, na.rm=T), 
                  sd= sd(AccumulatedMass, na.rm=T),
                  p25 = mean-2*sd,#quantile(AccumulatedMass, 0.25, na.rm=T), 
                  p75 = mean+2*sd,#quantile(AccumulatedMass, 0.75, na.rm=T),
                  AnimalTag = 'Peer Mean +-2xsd')
      
      ggplot() +
        geom_line(data = df[AnimalTag %in% input$animal_tag],
                  aes(x = StartTime, y = AccumulatedMass, col=   factor(AnimalTag))) +
        geom_ribbon(data = summary_stats, aes(x = StartTime, ymin = p25, ymax = p75), fill = "grey80", alpha = 0.5) +
        geom_line(data = summary_stats, aes(x=StartTime, y = mean,  col=   factor(AnimalTag)),  alpha=0.8) +
        labs(title = "Cumulative Intake vs Peer Range", x = "Time", y = "Cumulative Intake (Kg)",
             col='')+
        theme_minimal() 
    } else {
      ggplot()+ geom_blank()
    }
  })
  
  
  output$total_mass_plot <- renderPlot({
    
    req(input$animal_tag)
    req(input$date_range)
    df <- req(data())[as.Date(StartTime) >= input$date_range[1] & 
                        as.Date(StartTime) <= input$date_range[2]]
    if (nrow(df) == 0) return(NULL)  # prevents rendering empty table
    
    # Total mass per animal
    summary_df <- df[, .(TotalMass = sum(MassDiffKg)), by = AnimalTag]
    
    # Peer stats (excluding selected animal)
    peer_df <- summary_df[AnimalTag != input$animal_tag]
    stats <- peer_df[, .(
      mean = mean(TotalMass),
      p25  = quantile(TotalMass, 0.25),
      p75  = quantile(TotalMass, 0.75)
    )]
    
    # Selected animal data
    #selected_df <- summary_df[AnimalTag == input$animal_tag]
    selected_df = req(resolved_data()$df) %>%
      as.data.frame() %>%
      filter(AnimalTag == input$animal_tag) %>%
      mutate(AnimalTag = as.character(AnimalTag)) %>%
      group_by(AnimalTag) %>%
      summarize(TotalMass = sum(MassDiffKg, na.rm=T),
                p25 = NA_real_, p75 = NA_real_)
    
    # selected_df[, TotalMass := sum(MassDiffKg),
    #              by = AnimalTag]
    # Create a second row for peer mean
    peer_bar <- data.frame(
      AnimalTag = 'Peer Mean',
      TotalMass = stats$mean,
      p25 = stats$p25,
      p75 = stats$p75
    )
    animaltag = input$animal_tag
    # Combine both for plotting
    plot_df <- rbind(
      selected_df,#[, .(AnimalTag, TotalMass, p25 = NA_real_, p75 = NA_real_)],
      peer_bar
    ) %>%
      mutate(AnimalTag = as.character(AnimalTag))
    ggplot(plot_df, aes(x = (AnimalTag), y = TotalMass, fill = (AnimalTag))) +
      geom_col(width = 0.6) +
      geom_errorbar(aes(ymin = p25, ymax = p75),
        width = 0.2,color = "black",na.rm = TRUE) +
      labs(title = "Total Mass Intake vs Peer Mean",
          x = "Animal Tag",
          y = "Total Intake (Kg)",
          col='AnimalTag',
          fill='AnimalTag'
      ) +
      # scale_fill_manual(values = 
      #                     c("Peer_Mean" = "grey70", animaltag = "#1f77b4"))+
      theme_minimal() 
      
  })
  
  
  
  output$summary_table <- renderTable({
    df <- req(resolved_data()$df) %>%
      filter(AnimalTag == input$animal_tag)
    if (nrow(df) == 0) return(NULL)  # prevents rendering empty table
    
    df %>%
      summarise(
        TotalSessions = .N,
        `Total Time On Feeder (Min)` = sum(DurationSec) / 60,
        `Total Mass Consumed (Kg)` = sum(MassDiffKg),
        FlagsRaised = sum(Flags != "", na.rm = TRUE)
      )
  })
  
  
  
  output$overlap_table <- DT::renderDataTable({
    req(input$overlap_action)
    req(filtered_data())
    req(resolved_data())
    #
    if (input$overlap_action == "original") {
        df <- req(filtered_data())[order(StartTime)] 
        }else {
        df <- resolved_data()$df[order(StartTime)]
        if (nrow(df)==0) {
          df <- resolved_data()$solved_df
        }
        }
    if (nrow(df) == 0) {
      return(NULL)  # prevents rendering empty table
    } else {
      #df[, StartTime := format(StartTime, "%Y-%m-%d %H:%M:%S")]
      #df[, EndTime := format(EndTime, "%Y-%m-%d %H:%M:%S")]
      
      df <- df[overlap_flag == TRUE] %>%
        filter(AnimalTag == input$animal_tag)
      
      # Assign a color per overlap_id
      df[, row_color := as.character(factor(overlap_id))]
      
      # The table is the one place these timestamps are read as text, and by
      # now it holds only the overlapping rows of one animal, so formatting
      # here costs nothing.
      shown <- df[, .(overlap_id, AnimalTag, Feeder, StartTime, EndTime,
                      DurationSec, MassDiffKg, Flags)]
      shown[, `:=`(StartTime = format(StartTime, "%Y-%m-%d %H:%M:%S"),
                   EndTime   = format(EndTime,   "%Y-%m-%d %H:%M:%S"))]

      DT::datatable(shown,
                    options = list(rowCallback = DT::JS(
                      "function(row, data, index) {",
                      "  var colors = ['#f2f2f2', '#d9edf7', '#fcf8e3', '#fbeed5', '#dff0d8'];",
                      "  var id = data[0];",  # overlap_id is the 5th column (0-indexed)
                      "  if (id !== null && id !== '') {",
                      "    var color = colors[parseInt(id) % colors.length];",
                      "    $(row).css('background-color', color);",
                      "  }",
                      "}"
                    ))
      )
    }
    }
    )
}
shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))

