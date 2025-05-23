# nightly_fins_download.R - Fixed version

# ---- Load Dependencies ----
library(httr)
library(jsonlite)
library(lubridate)
library(readr)
library(dplyr)

# ---- Source API Function ----
source("R/get_fins_data.R")

# ---- Log Utility Function ----
log_message <- function(msg, type = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC")
  pt_time <- format(Sys.time(), tz = "America/Los_Angeles", "%Y-%m-%d %H:%M:%S PT")
  full_msg <- paste0("[", type, "] ", timestamp, " (", pt_time, ") - ", msg, "\n")
  
  # Ensure logs/ directory exists
  if (!dir.exists("logs")) dir.create("logs")
  
  # Print to console for GitHub Actions logs
  cat(full_msg)
  
  # Append to log file
  cat(full_msg, file = "./logs/fins_download.log", append = TRUE)
}

# ---- Ensure data directory exists ----
if (!dir.exists("data")) dir.create("data")

# ---- Run Download Logic ----
tryCatch({
  log_message("Starting nightly FINS data download")
  
  # Check API key
  api_key <- Sys.getenv("FINS_API_KEY")
  if (api_key == "") {
    stop("FINS_API_KEY environment variable not set")
  }
  log_message("API key found")
  
  # Get current time info
  current_utc <- Sys.time()
  current_pt <- format(current_utc, tz = "America/Los_Angeles", "%Y-%m-%d %H:%M:%S")
  log_message(paste("Current PT time:", current_pt))
  
  # Download data - get ALL historical data for NPT GRSME Program only
  end_date <- format(Sys.Date(), "%m/%d/%Y")
  
  log_message(paste("Requesting ALL historical trapping data through", end_date, "for NPT GRSME Program only"))
  # log_message(paste("Requesting ALL historical trapping data through", end_date, "for entire domain"))
  
  fins_data <- get_fins_data(
    start_date = NULL,  # NULL = get all historical data
    end_date = end_date,
    module = "Trapping",
    scope = NULL,  # NULL = only your facility (NPT GRSME Program), "domain" = all facilities in domain
    use_post = FALSE  # Use GET method for simplicity
  )
  
  # Validate data
  if (!is.data.frame(fins_data)) {
    stop("API returned data that is not a data frame")
  }
  
  if (nrow(fins_data) == 0) {
    log_message("API returned empty data set - this may be normal if no recent trapping occurred")
  } else {
    log_message(paste("Downloaded", nrow(fins_data), "rows of data"))
    log_message(paste("Columns:", paste(names(fins_data), collapse = ", ")))
  }
  
  # Build filename and save
  file_name <- "data/TrappingData.csv"
  write_csv(fins_data, file = file_name)
  
  if (!file.exists(file_name)) {
    stop("CSV file was not created successfully")
  }
  
  file_info <- file.info(file_name)
  log_message(paste("Successfully saved file:", file_name, "- Size:", file_info$size, "bytes"))
  
  
  # Add summary info
  if (nrow(fins_data) > 0) {
    log_message("Data summary:")
    date_col <- NULL
    if ("Trapped Date" %in% names(fins_data)) {
      date_col <- "Trapped Date"
    } else if ("TrapDate" %in% names(fins_data)) {
      date_col <- "TrapDate"
    }
    
    if (!is.null(date_col)) {
      date_range <- range(fins_data[[date_col]], na.rm = TRUE)
      log_message(paste("  Date range:", paste(date_range, collapse = " to ")))
    }
    log_message(paste("  Total records:", nrow(fins_data)))
  }
  

log_message("Download completed successfully")

}, error = function(e) {
  error_msg <- conditionMessage(e)
  log_message(paste("Download failed:", error_msg), type = "ERROR")
  
  # Print error to console for GitHub Actions
  cat("ERROR:", error_msg, "\n")
  
  # Exit with error code so GitHub Actions shows failure
  quit(status = 1)
})