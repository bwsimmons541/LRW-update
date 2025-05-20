# R/report_helpers.R

library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(flextable)
# library(cdmsR)
library(cuyem) 

# ---- CDMS login ----

# ---- Source GRSME functions ----
source("../R/sumGRSMEdisp.R")  # FINS Disposition Summary
source("../R/sumGRSMEbrood.R") # Brood Collection Summary
# source("./R/cdms_login.R")


# ---- Load Yearly Estimates ----
load_yearly_estimates <- function(year, path = "../data/yearly_estimates.csv") {
  read_csv(path) %>%
    filter(year == !!year) %>%
    slice(1)
}


# ---- Load and Clean Weir Data from CDMS or .rda and TrappingData.xlsx ----
get_trap_data <- function(download_cdms = TRUE, trap_year = NULL) {
  if (download_cdms) { # == TRUE
    # Download new data
    AdultWeirData <- get_WeirData(Facility = "NPT GRSME Program")
    
    # Save to disk for reuse
    save(AdultWeirData, file = "../data/AdultWeirData.rda")
    
    # Clean weir data
    AdultWeirData_clean <- clean_weirData(AdultWeirData) |>
      mutate(
        MonthDay = format(as.Date(trapped_date), "%m/%d"),
        count = as.double(count)
      )
    
    # Assign to GRSME_df (potentially redundant filter)
    grsme_df <- AdultWeirData_clean
    
  } else {
    # Load existing data
    load(file = "../data/AdultWeirData.rda")
    
    # Clean historic weir data
    AdultWeirData_clean <- clean_weirData(AdultWeirData) |>
      mutate(
        MonthDay = format(as.Date(trapped_date), "%m/%d"),
        count = as.double(count)
      )
    
    # Load current data directly from FINS trapping XLSX
    grsme_df <- readxl::read_xlsx("../data/TrappingData.xlsx")
    
    grsme_df <- clean_weirData(grsme_df) |>
      mutate(
        MonthDay = format(as.Date(trapped_date), "%m/%d"),
        count = as.double(count)
      )
      
  }
  
  if (!is.null(trap_year)) {
    grsme_df <- grsme_df |> filter(trap_year == !!trap_year)
  }
  
  # ✅ Return both cleaned datasets in a named list
  list(
    AdultWeirData_clean = AdultWeirData_clean,
    grsme_df = grsme_df
  )
}

# ---- Make Trap Date ----

make_trap_date <- function(month_day, year) {
  ymd(paste(year, month_day, sep = "-"))
}



#---- Calculate Dispositions ----

calculate_dispositions <- function(data, trap_year) {
  # Summary tables
  h_df <- sumGRSMEdisp(data = data, origin_ = "Hatchery", trap.year = trap_year)
  n_df <- sumGRSMEdisp(data = data, origin_ = "Natural", trap.year = trap_year)

  # Extract counts
  hat_up <- as.numeric(stringr::str_extract(h_df[[1, 5]], "^\\d+"))
  nat_up <- as.numeric(stringr::str_extract(n_df[[1, 5]], "^\\d+"))
  h_upstream_calc <- round((hat_up / (hat_up + nat_up)) * 100, 0)

  hat_bs <- as.numeric(stringr::str_extract(h_df[[2, 5]], "^\\d+"))
  nat_bs <- as.numeric(stringr::str_extract(n_df[[2, 5]], "^\\d+"))
  n_brood_calc <- round((nat_bs / (hat_bs + nat_bs)) * 100, 0)

  list(
    h_df = h_df,
    n_df = n_df,
    h_upstream_calc = h_upstream_calc,
    n_brood_calc = n_brood_calc
  )
}


# ---- Plot Data Prep Function Prepare Mega DF ----

prepare_megadf <- function(trap_year, grsme_df, weir_data_clean) {
 
  # ---- Flow Data: Current Year ----
  start_date <- paste0(trap.year, "-05-30") #changed trap_year to trap.year
  end_date <- paste0(trap.year, "-09-30") #changed trap_year to trap.year
  
  req_url <- paste0(
    "https://apps.wrd.state.or.us/apps/sw/hydro_near_real_time/hydro_download.aspx?station_nbr=13330000",
    "&start_date=", start_date, "%2012:00:00%20AM",
    "&end_date=", end_date, "%2012:00:00%20AM",
    "&dataset=MDF&format=csv"
  )
  
  flow_df <- read.delim(req_url, sep = "\t") |>
    mutate(
      record_date = lubridate::mdy(record_date),
      MonthDay = format(record_date, "%m/%d"),
      facet = as.character(trap.year) #changed trap_year to trap.year
    ) |>
    select(MonthDay, MeanDailyFlow = mean_daily_flow_cfs, facet)
  
  # ---- Flow Data: Historic (5-year average) ----
  start_date_h <- paste0(trap.year - 5, "-05-30") #changed trap_year to trap.year
  end_date_h <- paste0(trap.year - 1, "-09-21") #changed trap_year to trap.year
  
  req_url2 <- paste0(
    "https://apps.wrd.state.or.us/apps/sw/hydro_near_real_time/hydro_download.aspx?station_nbr=13330000",
    "&start_date=", start_date_h, "%2012:00:00%20AM",
    "&end_date=", end_date_h, "%2012:00:00%20AM",
    "&dataset=MDF&format=csv"
  )

  
  flow_df_h <- read.delim(req_url2, sep = "\t") |>
    mutate(
      record_date = mdy(record_date),
      legend = paste(Sys.Date() - 1, "Discharge"),
      MonthDay = format(as.Date(record_date), "%m/%d")
    ) |>
    group_by(MonthDay) |>
    summarise(MeanDailyFlow = mean(mean_daily_flow_cfs, na.rm = TRUE)) |>
    mutate(
      facet = paste0(trap.year - 5, "-", trap.year - 1, " Average")
    )
  
  # ---- Combine Flow----
  
  flow_all <- bind_rows(flow_df, flow_df_h) |>
    mutate(
      trapped_date = make_trap_date(MonthDay, trap.year)
    ) |>
    filter( # This filter is klling this section when imported from helpers.
      between(
        trapped_date,
        ymd(paste0(trap.year, "-05-30")),
        ymd(paste0(trap.year, "-09-21"))
      )
    )

  
  lrw_catch <- grsme_df |>
    filter(
      species == "Chinook",
      recap == FALSE,
      trap_year == trap.year,
      age_designation == "Adult"
    ) |>
    group_by(trapped_date, MonthDay, origin) |>
    summarise(Catch = sum(count, na.rm = TRUE), .groups = "drop") |>
    mutate(facet = as.character(trap.year))
  
  # ---- Catch: Historic Mean ----
  
  lrw_historic <- weir_data_clean |>
    filter(
      facility == "NPT GRSME Program",
      species == "Chinook",
      recap == FALSE,
      !trap_year %in% c(1997:(trap.year - 6), trap.year),
      age_designation == "Adult"
    ) |>
    group_by(MonthDay, origin) |>
    summarise(AllCatch = sum(count, na.rm = TRUE), .groups = "drop") |>
    mutate(
      Catch = AllCatch / 5,
      trapped_date = make_trap_date(MonthDay, trap.year),
      facet = paste0(trap.year - 5, "-", trap.year - 1, " Average")
    )
  
  # Combine Catch
  lrw_all <- bind_rows(lrw_catch, lrw_historic)
  
  # Merge with Flow
  lrw_megadf <- full_join(
    lrw_all, flow_all, 
    by = c("trapped_date", "facet", "MonthDay"))
  
  # Order facet
  lrw_megadf$facet <- factor(
    lrw_megadf$facet,
    levels = c(
      as.character(trap.year), #changed from trap_year
      paste0(trap.year - 5, "-", trap.year - 1, " Average")) #changed trap_year to trap.year
  )
  
  list(lrw_megadf = lrw_megadf, 
       lrw_catch = lrw_catch)
}




# ---- Generate Plot ----

generate_lrw_megaplot <- function(megadf,
                                  lrw_catch,
                                  save_plot = FALSE,
                                  output_path = "../LRW_megaplot.jpg") {
  # ---- Compute Y-Axis Max ---
  plot_max_df <- lrw_catch |>
    group_by(trapped_date) |>
    summarise(Count = sum(Catch), .groups = "drop")
  
  plot_max_df2 <- megadf |>
    group_by(trapped_date) |>
    summarise(Count = sum(Catch, na.rm = TRUE), .groups = "drop")
  
  plot_max <- if (max(plot_max_df$Count, na.rm = TRUE) > max(plot_max_df2$Count, na.rm = TRUE)) {
    round(max(plot_max_df$Count, na.rm = TRUE) + 2, 0)
  } else {
    round(max(plot_max_df2$Count, na.rm = TRUE) + 2, 0)
  }
  
  # ---- Scale Factor for Dual Axis ---
  scale_factor <- round(
    max(megadf$Catch, na.rm = TRUE) / max(megadf$MeanDailyFlow, na.rm = TRUE),
    3
  )
  
  # ---- Create Plot ---
  p <- ggplot(megadf, aes(x = trapped_date)) +
    geom_bar(
      aes(y = Catch, fill = origin),
      color = "black",
      stat = "identity",
      position = "stack",
      width = 1
    ) +
    geom_line(
      aes(y = MeanDailyFlow * scale_factor, linetype = "Discharge"),
      color = "blue",
      size = 1
    ) +
    scale_y_continuous(
      name = "Number of Chinook Adults",
      breaks = scales::breaks_pretty(7),
      limits = c(0, max(0, plot_max)),
      expand = c(0, 0),
      sec.axis = sec_axis(
        ~ . / scale_factor,
        name = expression(paste("Discharge (" * ft^3 * "/s)")),
        breaks = scales::breaks_pretty(7)
      )
    ) +
    scale_x_date(
      name = "",
      labels = scales::label_date("%m/%d"),
      breaks = scales::breaks_pretty(7),
      expand = c(0.001, 0.001)
    ) +
    scale_fill_manual(values = c("Natural" = "#FDE735FF", "Hatchery" = "#482677FF")) +
    facet_grid(rows = vars(facet)) +
    guides(color = FALSE) +
    theme_bw() +
    theme(
      axis.text.x = element_text(hjust = 1, angle = 45, size = 14),
      axis.ticks.length.x = unit(0.15, "cm"),
      axis.title.y.left = element_text(size = 16),
      axis.text.y.left = element_text(size = 14),
      axis.title.y.right = element_text(color = "blue", size = 16),
      axis.text.y.right = element_text(color = "blue", size = 14),
      legend.position = "top",
      legend.title = element_blank(),
      legend.box.background = element_blank(),
      panel.grid.minor = element_blank(),
      panel.spacing = unit(2, "lines")
    )
  
  # ---- Optionally Save Plot ---
  if (save_plot) {
    ggsave(
      filename = output_path,
      plot = p,
      device = "jpeg",
      width = 10,
      height = 7,
      units = "in"
    )
  }
  
  # ---- Return Plot Object ---
  return(p)
  
}

# ---- Prepare Captions ----

prepare_caption_table1 <- function(trap_year) {
  paste0(
    "Return year ", trap_year, " capture and disposition summary of Hatchery Chinook Salmon ",
    "(numbers in parentheses exclude recaptures)."
  )
}

prepare_caption_table2 <- function(trap_year) {
  paste0(
    "Return year ", trap_year, " capture and disposition summary of Natural Chinook Salmon ",
    "(numbers in parentheses exclude recaptures)."
  )
}

prepare_caption_table3 <- function(trap_year) {
  paste0(
    "Return year ", trap_year, " weekly summary of captured adult Chinook Salmon and Bull Trout, ",
    "excluding recaptures. Broodstock collection for Chinook Salmon is shown in parentheses. ",
    "*Asterisk indicates an incomplete week."
  )
}

prepare_caption_plot <- function(trap_year) {
  paste0(
    "Return year ", trap_year, " (top panel) and five-year average (bottom panel) of mean daily discharge ",
    "(cubic feet per second) and daily captures of hatchery- and natural-origin adult Chinook salmon ",
    "at the Lostine River Weir. Discharge recorded at USGS station 1333000 located upstream of the town of Lostine."
  )
}
