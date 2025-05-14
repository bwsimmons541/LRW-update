# R/report_helpers.R

library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(flextable)

# ---- Load Yearly Estimates ----
load_yearly_estimates <- function(year, path = "data/yearly_estimates.csv") {
  read_csv(path) %>%
    filter(year == !!year) %>%
    slice(1)
}

# ---- Load and Clean Weir Data ----
load_weir_data <- function(path = "data/AdultWeirData.rda") {
  load(path)  # loads object: AdultWeirData
  AdultWeirData
}

clean_weir_data <- function(weir_data) {
  weir_data %>%
    mutate(
      MonthDay = format(as.Date(trapped_date), '%m/%d'),
      count = as.double(count),
      trapped_date = ymd(trapped_date)
    )
}

# ---- Make Trap Date ----
make_trap_date <- function(year, month_day) {
  ymd(paste0(year, "-", month_day))
}

# ---- Prepare Flow Data ----
prepare_flow_data <- function(trap_year) {
  start_date <- paste0(trap_year, '-05-30')
  end_date <- paste0(trap_year, '-09-30')
  
  req_url <- paste0(
    "https://apps.wrd.state.or.us/apps/sw/hydro_near_real_time/hydro_download.aspx?station_nbr=13330000",
    "&start_date=", start_date, "%2012:00:00%20AM",
    "&end_date=", end_date, "%2012:00:00%20AM",
    "&dataset=MDF&format=csv"
  )
  
  read.delim(req_url, sep = '\t') %>%
    mutate(
      record_date = mdy(record_date),
      MonthDay = format(as.Date(record_date), '%m/%d'),
      facet = paste(trap_year)
    ) %>%
    select(MonthDay, MeanDailyFlow = mean_daily_flow_cfs, facet)
}

# ---- Prepare Catch Data ----
prepare_catch_data <- function(grsme_df, trap_year) {
  grsme_df %>%
    filter(
      species == 'Chinook',
      recap == FALSE,
      trap_year == !!trap_year,
      age_designation == 'Adult'
    ) %>%
    group_by(trapped_date, MonthDay, origin) %>%
    summarize(Catch = sum(count, na.rm = TRUE), .groups = "drop") %>%
    mutate(facet = paste(trap_year))
}

# ---- Generate Plot ----
generate_lrw_megaplot <- function(flow_df, catch_df, trap_year) {
  flow_df <- flow_df %>%
    mutate(trapped_date = make_trap_date(trap_year, MonthDay))
  
  merged <- full_join(catch_df, flow_df, by = c("MonthDay", "facet", "trapped_date"))
  
  scale_factor <- max(merged$Catch, na.rm = TRUE) / max(merged$MeanDailyFlow, na.rm = TRUE)
  
  ggplot(merged, aes(x = trapped_date)) +
    geom_bar(aes(y = Catch, fill = origin), stat = "identity", color = "black", width = 1) +
    geom_line(aes(y = MeanDailyFlow * scale_factor), color = "blue", size = 1) +
    scale_y_continuous(
      name = "Number of Chinook Adults",
      sec.axis = sec_axis(~./scale_factor, name = "Discharge (ft³/s)")
    ) +
    scale_fill_manual(values = c("Natural" = '#FDE735FF', "Hatchery" = '#482677FF')) +
    theme_bw() +
    theme(legend.position = "top")
}
