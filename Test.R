name= '../SFVisitData_Pen55_Combined(cov2023P_SFVisitData_Pen55).xlsx'
name = "C:/Users/fmaur/Downloads/SF_DMI_Feed_Behavior(Data) (2).csv"
datapath = name
data <- reactive({
  #req(input$file)
  ext <- tools::file_ext(name)
  
  if (tolower(ext) %in% c("xlsx", "xls")) {
    # Read Excel file
    dt <- as.data.table(read_excel(datapath))
  } else {
    # Read text-based file and find header line
    lines <- readLines(datapath)
    start_line <- which(grepl("AnimalTag", lines))
    
    if (length(start_line) == 0) {
      stop("Header line with 'AnimalTag' not found.")
    }
    
    if (start_line[1] == 1) {
      dt <- fread(datapath)
    } else {
      dt <- fread(datapath, skip = start_line[1] - 1)
    }
  }
  
  # Parse datetime columns
  dt[, StartTime := parse_date_time(StartTime, orders = c("mdY HM p", "ymd HMS","ymd"))]
  dt[, EndTime := parse_date_time(EndTime, orders = c("mdY HM p", "ymd HMS","ymd"))]
  print(head(dt))
  dt
  
  
  filtered_data <- 
    {
    df <- dt#[AnimalTag == input$animal_tag]
    #df <- df[as.Date(StartTime) >= (input$date_range[1]) & as.Date(StartTime) <= (input$date_range[2])]
    df <- df[order(StartTime)]
    if (nrow(df) == 0) return(NULL)  # prevents rendering empty table
    # Append to existing Flags column
    #This expression compares the end time of the previous row to the start time of the current row. If the previous event ends after the current one starts, then the two events overlap.
    
    df[, `:=`(overlap_flag = FALSE, overlap_id = NA_integer_)]
    tolerance_sec=0
    df[, c("overlap_flag", "overlap_id") := {
      overlap_cond <- (StartTime - shift(EndTime, type='lag')) <= tolerance_sec &
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
    
    
    
    df[, Flags := fifelse(overlap_flag,
                          ifelse(is.na(Flags), "Overlap", paste(Flags, "Overlap", sep = "; ")),
                          as.character(Flags))]
    
    
    filtered_data=  df
  
  
  
  merge_df <- filtered_data[!is.na(overlap_id)]
  merged_rows <- merge_df %>%
    group_by(AnimalTag, overlap_id) %>%
    mutate(
      StartTime = min(StartTime, na.rm = TRUE),
      EndTime = max(EndTime, na.rm = TRUE),
      DurationSec = sum(DurationSec, na.rm = TRUE),
      MassDiffKg = sum(MassDiffKg, na.rm = TRUE),
      StartMassKg = min(StartMassKg,na.rm=TRUE),
      EndMassKg = max(EndMassKg,na.rm=TRUE)
    ) %>%
    slice_max(MassDiffKg, n = 1, with_ties = FALSE) %>%
    ungroup()
  
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
              AnimalTag = 'Peer Mean (p25-p75)')
  animal_tag = df$AnimalTag[1]
  ggplot() +
    geom_line(data = df[AnimalTag %in% animal_tag],
              aes(x = StartTime, y = AccumulatedMass, col=   factor(AnimalTag))) +
    geom_ribbon(data = summary_stats, aes(x = StartTime, ymin = p25, ymax = p75), fill = "grey80", alpha = 0.5) +
    geom_line(data = summary_stats, aes(x=StartTime, y = mean,  col=   factor(AnimalTag)),  alpha=0.8) +
    labs(title = "Cumulative Intake vs Peer Range", x = "Time", y = "Cumulative Intake (Kg)",
         col='')+
    theme_minimal()
  all_df %>%
    write.csv('C:/Users/fmaur/Downloads/SF_DMI_Feed_Behavior(Data)_merged.csv',row.names = F)
  