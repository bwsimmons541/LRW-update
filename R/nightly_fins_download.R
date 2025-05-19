# scripts/nightly_fins_download.R
library(httr)
library(jsonlite)
library(dplyr)
library(readr)
library(lubridate)
source("./R/get_fins_data.R")  # contains get_fins_data()

# Set download window for today
today <- Sys.Date()

# Safe wrapper
try({
  fins_data <- get_fins_data()
  if (!is.data.frame(fins_data)) stop("No data returned from FINS API")
  
  # Save with timestamped filename
  fname <- paste0("data/TrappingData_FINS_", format(today, "%Y%m%d"), ".csv")
  write_csv(fins_data, file = fname)
  
  message("FINS data successfully saved to: ", fname)
}, silent = TRUE)