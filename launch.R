# 📦 List of required packages
required_packages <- c("shiny", "DT", "data.table", "ggplot2", "dplyr", "lubridate", "plotly")

# 🔍 Function to check and install missing packages
check_and_install <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(sprintf("Installing missing package: %s", pkg))
    install.packages(pkg, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE)
}

# 🚀 Apply function to all required packages
invisible(lapply(required_packages, check_and_install))




library(shiny)
runApp("./app.R")