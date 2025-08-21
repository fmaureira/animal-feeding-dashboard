# 🐄 Animal Feeding Dashboard

**Author:** Fidel  
**Date:** August 2025

##  Overview

The **Animal Feeding Dashboard** is an interactive Shiny application designed to visualize, clean, and resolve feeding log data for animals. It supports overlap detection, benchmarking, and cumulative intake analysis, making it ideal for researchers, nutritionists, and farm managers.

##  Features

- Upload feeding logs in CSV format  
- Select animals dynamically from uploaded data  
- Filter by date range (customizable via UI)  
- Set overlap tolerance in seconds  
- Detect and resolve overlapping feeding events  
- Visualize feeding patterns with:
  - Interactive feeding timeline (`plotly`)
  - Accumulated intake plot
  - Total mass per animal  
- View summary and overlap tables  
- Download cleaned and resolved dataset  

##  Technologies Used

- `shiny` – UI and server logic  
- `data.table` – Fast data manipulation  
- `ggplot2` and `plotly` – Static and interactive plots  
- `dplyr` and `lubridate` – Data wrangling and time handling  
- `DT` – Interactive data tables  

##  Installation

Ensure R is installed and the following packages are available:

```r
install.packages(c("shiny", "data.table", "ggplot2", "dplyr", "lubridate", "plotly", "DT"))

## Running the App
You can launch the app using R or Rscript:


# From R console
shiny::runApp("path/to/app.R", launch.browser = TRUE)

# Or via terminal (Windows/macOS/Linux):
Rscript launch.R


Use the provided run_launch.bat or run_launch.sh scripts to auto-detect R, install packages, and launch the app.
📄 CSV Format Requirements


###Uploaded CSV files should include at least the following columns:
- AnimalTag – Unique identifier
- StartTime, EndTime – Feeding event timestamps
- MassDiffKg – Amount of feed consumed
- DurationSec – Duration in seconds
- Feeder – Feeder ID
- Flags – Flags

### Notes
- Overlap resolution logic is customizable via tolerance_sec
- UI elements like date range and overlap filters are dynamically generated

### Contributing
Pull requests are welcome! If you have ideas for new visualizations, benchmarking logic, or data cleaning modules, feel free to fork and submit.
📜 License
MIT License. See the LICENSE file for details.

Built with care by Fidel Maureira, for better science and better feed.
