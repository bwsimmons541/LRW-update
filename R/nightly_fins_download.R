# nightly_fins_download.R

# ---- Load Dependencies ----
library(httr)
library(jsonlite)
library(lubridate)
library(readr)

# ---- Source API Function ----
source("R/get_fins_data.R")

# ---- Log Utility Function ----
log_message <- function(msg, type = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  full_msg <- paste0("[", type, "] ", timestamp, " - ", msg, "\n")
  
  # Ensure logs/ directory exists
  if (!dir.exists("logs")) dir.create("logs")
  
  # Append to log file
  cat(full_msg, file = "./logs/fins_download.log", append = TRUE)
}

# ---- Run Download Logic ----
tryCatch({
  log_message("Starting nightly FINS data download")
  
  fins_data <- get_fins_data()
  
  if (!is.data.frame(fins_data) || nrow(fins_data) == 0) {
    stop("Download returned no data.")
  }
  
  # Build filename and save
  file_name <- paste0("data/TrappingData_FINS_", format(Sys.Date(), "%Y%m%d"), ".csv")
  write_csv(fins_data, file = file_name)
  
  log_message(paste("Successfully saved file:", file_name))
  
}, error = function(e) {
  log_message(paste("Download failed:", conditionMessage(e)), type = "ERROR")
})
