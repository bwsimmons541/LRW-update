# deploy.R - Deployment script for LRW Chinook Summary

library(rsconnect)

# Check if environment variables are set
if (Sys.getenv("SHINYAPPS_NAME") == "" || 
    Sys.getenv("SHINYAPPS_TOKEN") == "" || 
    Sys.getenv("SHINYAPPS_SECRET") == "") {
  
  cat("Error: Shinyapps.io credentials not found in environment variables.\n")
  cat("Please create a .Renviron file with:\n")
  cat("SHINYAPPS_NAME=nptfisheries\n")
  cat("SHINYAPPS_TOKEN=your_token_here\n")
  cat("SHINYAPPS_SECRET=your_secret_here\n")
  cat("\nThen restart R and try again.\n")
  stop("Missing credentials")
}

# Set up account credentials
cat("Setting up shinyapps.io account credentials...\n")
rsconnect::setAccountInfo(
  name = Sys.getenv("SHINYAPPS_NAME"),
  token = Sys.getenv("SHINYAPPS_TOKEN"),
  secret = Sys.getenv("SHINYAPPS_SECRET")
)

# Verify account is set up
accounts <- rsconnect::accounts()
cat("Available accounts:\n")
print(accounts)

# Deploy the app
cat("Deploying LRW Chinook Summary to shinyapps.io...\n")

rsconnect::deployApp(
  appName = "LRW-Chinook-Summary",
  appTitle = "Lostine River Weir - Chinook Summary",
  appFiles = c(
    "app.R",
    "R/",
    "www/",
    "data/TrappingData.csv",
    "data/yearly_estimates.csv"
  ),
  forceUpdate = TRUE,
  launch.browser = TRUE
)

cat("Deployment complete!\n")
cat("App URL: https://nptfisheries.shinyapps.io/LRW-Chinook-Summary/\n")