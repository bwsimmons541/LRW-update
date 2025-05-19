
# function to get FINS data through API

get_fins_data <- function(
    start_date = "2020-05-01",
    end_date = Sys.Date(),
    module = "Adult Weir",
    facility = "NPT GRSME Program",
    apikey = "fXegxGXlJdWU0WaTsSh1qh9u7v9kn3ZR"
) {
  url <- "https://fins.psmfc.org/api/v1/data"
  
  res <- httr::GET(url, query = list(
    apikey = apikey,
    Module = module,
    Facility = facility,
    StartDate = start_date,
    EndDate = end_date
  ))
  
  if (res$status_code != 200) {
    stop("API request failed with status: ", res$status_code)
  }
  
  data <- httr::content(res, as = "text", encoding = "UTF-8") |>
    jsonlite::fromJSON(flatten = TRUE)
  
  return(data)
}