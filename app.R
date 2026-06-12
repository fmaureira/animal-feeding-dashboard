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
options(shiny.maxRequestSize = 1000*1024^2) #a GB to upload

ui <- fluidPage(
  titlePanel("Animal Feeding Dashboard"),
 # sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV or Excel"),
      uiOutput("animal_selector"),
      #dateRangeInput("date_range", "Select Date Range")
      uiOutput("date_ui"),
      numericInput("tolerance_sec", "Overlap Tolerance (seconds)", value = 0, min = 0, step = 1),
      
      uiOutput("overlap_ui"),
      
      
      
      
      downloadButton("download_data", "Download Resolved Dataset"),
      downloadButton("download_visits", "Download Visit Summary")


    ),
    mainPanel(
      plotlyOutput("feeding_plot"),
      tableOutput("summary_table"),
      #tableOutput("overlap_table"),
      DT::dataTableOutput("overlap_table"),
      plotOutput("accumulated_plot"),
      plotOutput("total_mass_plot"),
      h4("Meal-Interval Mixture Model"),
      plotOutput("meal_mixture_plot"),
      h4("Daily Visits (selected animal)"),
      plotOutput("animal_visits_plot"),
      h4("Visit Summary (all animals)"),
      tableOutput("visit_summary")
      #tableOutput("debug")

    )
  )
#)


server <- function(input, output, session) {
 
  data <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    
    if (tolower(ext) %in% c("xlsx", "xls")) {
      # Read Excel file
      dt <- as.data.table(read_excel(input$file$datapath))
    } else {
      # Read text-based file and find header line
      lines <- readLines(input$file$datapath)
      start_line <- which(grepl("AnimalTag", lines))
      
      if (length(start_line) == 0) {
        stop("Header line with 'AnimalTag' not found.")
      }
      
      if (start_line[1] == 1) {
        dt <- fread(input$file$datapath)
      } else {
        dt <- fread(input$file$datapath, skip = start_line[1] - 1)
      }
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
    p1 = ggplot(df, aes(x = StartTime, y = MassDiffKg)) +
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
    df[, StartTime := format(StartTime, "%Y-%m-%d %H:%M:%S")]
    df[, EndTime := format(EndTime, "%Y-%m-%d %H:%M:%S")]
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
  # log-intervals, derives the meal threshold from the component intersection
  # and assigns per-animal visit (meal) IDs.
  meal_data <- reactive({
    df <- req(resolved_data()$df)
    if (nrow(df) == 0) return(NULL)

    df <- compute_meal_intervals(df)

    log_intervals <- df$LogInterval[is.finite(df$LogInterval)]
    fit <- fit_normal_mixture(log_intervals, k = 2)
    threshold_log <- mixture_intersection(fit)

    df <- assign_visits(df, threshold_log)

    list(df = df, fit = fit, threshold_log = threshold_log,
         log_intervals = log_intervals)
  })


  output$meal_mixture_plot <- renderPlot({
    md <- req(meal_data())
    plot_meal_mixture(md$log_intervals, md$fit, md$threshold_log)
  })

  output$animal_visits_plot <- renderPlot({
    md <- req(meal_data())
    req(input$animal_tag)
    plot_animal_visits(md$df, input$animal_tag)
  })


  # Visit summary covers every animal in the dataset (not just the selected
  # one); the same all-animal table is used on screen and for download.
  visit_summary_data <- reactive({
    md <- req(meal_data())
    summarize_visits(md$df)
  })

  output$visit_summary <- renderTable({
    req(visit_summary_data())
  })

  output$download_visits <- downloadHandler(
    filename = function() {
      paste0("visit_summary_", Sys.Date(), ".csv")
    },
    content = function(file) {
      fwrite(visit_summary_data(), file)
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
      
      DT::datatable(df[, .(overlap_id,AnimalTag, Feeder, StartTime, EndTime, DurationSec,MassDiffKg,Flags)],
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
