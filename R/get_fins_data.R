# get_fins_data.R - Fixed version for FINS API

get_fins_data <- function(
    start_date = format(Sys.Date() - 7, "%m/%d/%Y"),  # Default to last 7 days
    end_date = format(Sys.Date(), "%m/%d/%Y"),
    module = "Trapping",
    scope = "domain",  # Use "domain" for all facilities in your domain
    apikey = Sys.getenv("FINS_API_KEY"),
    use_post = FALSE  # Set to TRUE if you want to use POST method
) {
  
  # Validate API key
  if (apikey == "" || is.null(apikey)) {
    stop("API key not found. Make sure FINS_API_KEY is set as an environment variable.")
  }
  
  # Check if current time is within allowed hours (6pm - 6am PT)
  # Convert current UTC time to PT
  current_utc <- Sys.time()
  current_pt <- format(current_utc, tz = "America/Los_Angeles")
  current_hour_pt <- as.numeric(format(as.POSIXct(current_pt), "%H"))
  
  # Check if we're in the maintenance window (11pm - 3am PT)
  if (current_hour_pt >= 23 || current_hour_pt <= 3) {
    warning("Current time is in maintenance window (11pm-3am PT). API call may fail.")
  }
  
  # Check if we're in business hours (6am - 6pm PT)
  if (current_hour_pt >= 6 && current_hour_pt < 18) {
    stop("API calls are only allowed outside business hours (6pm - 6am PT). Current PT time: ", current_pt)
  }
  
  # Build the correct URL based on method
  if (use_post) {
    base_url <- "https://www.finsnet.org/fins/chp"
  } else {
    base_url <- "https://www.finsnet.org/fins/ncg"
  }
  
  url <- paste0(base_url, "/", module, "/")
  
  # Build query parameters
  query_params <- list(
    apikey = apikey,
    startDate = start_date,
    endDate = end_date,
    scope = scope
  )
  
  # Make the API call
  if (use_post) {
    # POST method
    res <- httr::POST(
      url,
      query = query_params,
      body = list(mode = "post"),
      encode = "json",
      httr::add_headers("Content-Type" = "application/json")
    )
  } else {
    # GET method (recommended for simple CSV download)
    res <- httr::GET(
      url,
      query = query_params,
      httr::add_headers("Content-Type" = "application/json")
    )
  }
  
  # Check response status
  if (httr::status_code(res) != 200) {
    error_msg <- paste("API request failed with status:", httr::status_code(res))
    if (httr::status_code(res) == 429) {
      error_msg <- paste(error_msg, "- Rate limit exceeded. Wait 1 hour between calls to same module.")
    }
    stop(error_msg)
  }
  
  # Process response
  if (use_post) {
    # POST returns JSON with ReturnValue containing CSV
    content <- httr::content(res, as = "text", encoding = "UTF-8")
    json_response <- jsonlite::fromJSON(content)
    
    if (!is.null(json_response$Error) && json_response$Error != "") {
      stop("API returned error: ", json_response$Error)
    }
    
    csv_data <- json_response$ReturnValue
  } else {
    # GET returns CSV directly
    csv_data <- httr::content(res, as = "text", encoding = "UTF-8")
  }
  
  # Parse CSV data
  if (is.null(csv_data) || csv_data == "" || length(csv_data) == 0) {
    stop("API returned empty data")
  }
  
  # Convert CSV string to data frame
  tryCatch({
    data <- readr::read_csv(csv_data, show_col_types = FALSE)
    return(data)
  }, error = function(e) {
    stop("Failed to parse CSV data: ", conditionMessage(e))
  })
}