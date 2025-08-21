library(shiny)
#library(DT)
library(data.table)
library(ggplot2)
library(dplyr)
library(lubridate)
library(plotly)


ui <- fluidPage(
  titlePanel("Animal Feeding Dashboard"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV"),
      uiOutput("animal_selector"),
      #dateRangeInput("date_range", "Select Date Range")
      uiOutput("date_ui"),
      numericInput("tolerance_sec", "Overlap Tolerance (seconds)", value = 0, min = 0, step = 1),
      
      uiOutput("overlap_ui"),
      
      
      
      
      downloadButton("download_data", "Download Resolved Dataset")
      
      
    ),
    mainPanel(
      plotlyOutput("feeding_plot"),
      tableOutput("summary_table"),
      #tableOutput("overlap_table"),
      DT::dataTableOutput("overlap_table"),
      plotOutput("accumulated_plot"),
      plotOutput("total_mass_plot")
      #tableOutput("debug")
      
    )
  )
)


server <- function(input, output, session) {
  data <- reactive({
    req(input$file)
    lines <- readLines(input$file$datapath)
    
    # Step 2: Find line number where "AnimalTag" appears
    start_line <- which(grepl("AnimalTag", lines))  # use [1] to get the first match
    
    # Step 3: Read data from that line onward using fread
    dt <- fread(input$file$datapath, skip = max(start_line) - 1)
    
    #Clean data without tag
    dt = na.omit(dt)
    dt[, StartTime := ymd_hms(StartTime)]
    dt[, EndTime := ymd_hms(EndTime)]
    dt
  })
  
  output$date_ui <- renderUI({
    req(data())
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
    df <- df[as.Date(StartTime) >= (input$date_range[1]) & as.Date(StartTime) <= (input$date_range[2])]
    df <- df[order(StartTime)]
    # Append to existing Flags column
    #This expression compares the end time of the previous row to the start time of the current row. If the previous event ends after the current one starts, then the two events overlap.

    df[, `:=`(overlap_flag = FALSE, overlap_id = NA_integer_)]
    
    df[, c("overlap_flag", "overlap_id") := {
      overlap_cond <- (StartTime - shift(EndTime, type='lag')) <= input$tolerance_sec &
        Feeder != shift(Feeder)
      flag <- rep(FALSE, .N)
      id_vec <- rep(NA_integer_, .N)
      
      # Find indices where overlap occurs
      idx <- which(overlap_cond)
      
      # Assign flags and IDs to both rows in each overlap
      for (i in seq_along(idx)) {
        flag[idx[i]] <- TRUE
        if (idx[i] > 1) flag[idx[i] - 1] <- TRUE
        
        # Assign a unique ID to both rows
        id_val <- i  # or use another scheme if needed
        id_vec[idx[i]] <- id_val
        if (idx[i] > 1) id_vec[idx[i] - 1] <- id_val
      }
      list(flag, id_vec)
    }, by = AnimalTag]
    
      
      
    df[, Flags := ifelse(overlap_flag,
                         paste(Flags, "Overlap", sep = "; "),
                         Flags)]
    
    df
  })
  
  mass_summary <- reactive({
    df <- filtered_data()[order(StartTime)] %>%
      filter(AnimalTag == input$animal_tag)
    
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
      Feeder = Feeder[which.max(MassDiffKg)]  # Feeder with largest MassDiffKg
    ), by = overlap_id]
    
    merge_df = bind_rows(df[is.na(overlap_id)],merged_rows)
    merge_total <- sum(merge_df$MassDiffKg, na.rm = TRUE)
    # Delete scenario
    delete_df <- df[!(overlap_flag & MassDiffKg < shift(MassDiffKg))]
    delete_total <- sum(delete_df$MassDiffKg, na.rm = TRUE)
    
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
    df <- resolved_data() %>%
      filter(AnimalTag == input$animal_tag) %>%
      mutate(Flags = ifelse(is.na(Flags)| Flags=='','',Flags),
             overlap_id = ifelse(is.na(overlap_id),0,overlap_id))
    overlaps = max(df$overlap_id, na.rm=T)
    print(overlaps)
    df[,overlap_id := as.factor(overlap_id)]
    
    p1 = ggplot(df, aes(x = StartTime, y = MassDiffKg)) +
      geom_line() +
      labs(title = "Feed Intake Over Time", x = "Start Time", y = "Mass Diff (Kg)") +
      
      theme_minimal()
    
    if (overlaps>0) {
      p =p1 +geom_point(aes( color = overlap_id,shape=factor(Feeder)), size = 2) +
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
  df <- req(filtered_data())
  df <- df[order(StartTime)]
  #df[, overlap := shift(EndTime, type = "lag") > StartTime]
  
  if (input$overlap_action == "merge") {
    merge_df <- df[!is.na(overlap_id)]
    
    # Aggregate by overlap_id
    # merged_rows <- merge_df[, .(
    #   AnimalTag = first(AnimalTag),
    #   overlap_flag = first(overlap_flag),
    #   StartTime = min(StartTime),
    #   EndTime = max(EndTime),
    #   DurationSec = sum(DurationSec, na.rm = TRUE),
    #   MassDiffKg = sum(MassDiffKg, na.rm = TRUE),
    #   Feeder = Feeder[which.max(MassDiffKg)]  # Feeder with largest MassDiffKg
    # ), by = c(AnimalTag,overlap_id)]
    
    merged_rows <- merge_df %>%
      group_by(AnimalTag, overlap_id) %>%
      summarise(
        overlap_flag = first(overlap_flag),
        StartTime = min(StartTime, na.rm = TRUE),
        EndTime = max(EndTime, na.rm = TRUE),
        DurationSec = sum(DurationSec, na.rm = TRUE),
        MassDiffKg = sum(MassDiffKg, na.rm = TRUE),
        Feeder = Feeder[which.max(MassDiffKg)]
      )
    
    df = bind_rows(df[is.na(overlap_id)],merged_rows)
    
    } else if (input$overlap_action == "delete") {
    # df <- df[!(overlap_flag & MassDiffKg < shift(MassDiffKg)),
    #          by = overlap_id]
      
      # Separate overlapping and non-overlapping rows
      non_overlap <- df[is.na(overlap_id)]
      overlap <- df[!is.na(overlap_id)]
      
      # For each overlap group, keep only the row with the largest MassDiffKg
      overlap_resolved <- overlap[, .SD[which.max(MassDiffKg)], by = c("AnimalTag","overlap_id")]
      
      # Combine back with non-overlapping rows
      resolved_df <- rbindlist(list(non_overlap, overlap_resolved), use.names = TRUE)
      setorder(resolved_df, StartTime)
      df <- resolved_df
  }
  df
})
  
  
#Download data ####
  output$download_data <- downloadHandler(
    filename = function() {
      paste("resolved_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      fwrite(resolved_data(), file)
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
    df <- req(resolved_data())
    df <- df[order(StartTime)]
    df[, AccumulatedMass := cumsum(MassDiffKg), by = AnimalTag]
 
    all_df <- df#data()[as.Date(StartTime) >= input$date_range[1] & as.Date(StartTime) <= input$date_range[2]] 
    
    all_df[, AccumulatedMass := cumsum(MassDiffKg), by = AnimalTag]
    summary_stats <- all_df %>%
      as.data.frame() %>%
      mutate(StartTime = floor_date(StartTime, unit = "4hour")) %>%  # Round down
      group_by(StartTime) %>%
      summarise(mean = mean(AccumulatedMass, na.rm=T), 
                p25 = quantile(AccumulatedMass, 0.25, na.rm=T), 
                p75 = quantile(AccumulatedMass, 0.75, na.rm=T),
                AnimalTag = 'Peer Mean (p25-p75)')
    
    ggplot() +
      geom_line(data = df[AnimalTag == input$animal_tag],
                aes(x = StartTime, y = AccumulatedMass, col=   factor(AnimalTag))) +
      geom_ribbon(data = summary_stats, aes(x = StartTime, ymin = p25, ymax = p75), fill = "grey80", alpha = 0.5) +
      geom_line(data = summary_stats, aes(x=StartTime, y = mean,  col=   factor(AnimalTag)),  alpha=0.8) +
      labs(title = "Cumulative Intake vs Peer Range", x = "Time", y = "Cumulative Intake (Kg)",
           col='')+
      theme_minimal()
  })
  
  
  output$total_mass_plot <- renderPlot({
    
    
    df <- req(data())[as.Date(StartTime) >= input$date_range[1] & as.Date(StartTime) <= input$date_range[2]]
    
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
    selected_df = resolved_data() %>%
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
    df <- req(resolved_data()) %>%
      filter(AnimalTag == input$animal_tag)
    df %>%
      summarise(
        TotalSessions = .N,
        `Total Time On Feeder (Min)` = sum(DurationSec) / 60,
        `Total Mass Consumed (Kg)` = sum(MassDiffKg),
        FlagsRaised = sum(Flags != "", na.rm = TRUE)
      )
  })
  
  
  
  output$overlap_table <- DT::renderDataTable({
    
    #
    if (input$overlap_action == "original") {
      df <- req(filtered_data())[order(StartTime)] 
      }else {
      df <- resolved_data()[order(StartTime)]
      }
    df[, StartTime := format(StartTime, "%Y-%m-%d %H:%M:%S")]
    df[, EndTime := format(EndTime, "%Y-%m-%d %H:%M:%S")]
    
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
  })
}
shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
