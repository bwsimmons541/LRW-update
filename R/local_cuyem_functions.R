# R/local_cuyem_functions.R
# Local copy of cuyem::clean_weirData function to avoid package dependency

# Required packages for this function
library(dplyr)
library(lubridate)
library(stringr)

clean_weirData <- function(data) {
  # Validation
  if (is.null(data)) 
    stop("weir data must be supplied")
  
  # Standardize column names (lowercase with underscores)
  names(data) <- gsub(" ", "_", tolower(names(data)))
  
  # Handle date formatting - check if ISO format (with T) or regular format
  if (grepl("T", data$trapped_date[1])) {
    data$trapped_date = gsub("T\\d{2}:\\d{2}:\\d{2}", "", data$trapped_date)
    data$trapped_date = lubridate::ymd(data$trapped_date)
  } else {
    data$trapped_date = lubridate::mdy(data$trapped_date)
  }
  
  # Main data processing
  trap_df <- data %>% 
    mutate(
      trapped_date = lubridate::ymd(trapped_date), 
      trap_year = lubridate::year(trapped_date)
    ) %>% 
    mutate(
      weir = str_split(trap, " - ", simplify = TRUE)[, 1]
    ) %>% 
    mutate(
      stream = str_replace(weir, " Weir", ""), 
      stream = str_replace(stream, "Upper ", "")
    ) %>% 
    mutate(
      release_up = case_when(
        disposition == "Released" & grepl("Above|Upstream|Lostine River: Acclimation Facility", release_site) ~ TRUE, 
        TRUE ~ FALSE
      )
    ) %>% 
    mutate(
      release_dwn = case_when(
        disposition == "Released" & grepl("Below|Downstream", release_site) ~ TRUE, 
        living_status %in% c("DOA", "TrapMort") & grepl("Below|Downstream", moved_to) ~ TRUE, 
        TRUE ~ FALSE
      )
    ) %>% 
    mutate(
      marked = case_when(
        release_up & grepl("RT", applied_tags) & trap_year == 2011 ~ FALSE, 
        release_up & grepl("OP", applied_marks) ~ TRUE, 
        release_up & grepl("OP", applied_tags) ~ TRUE, 
        TRUE ~ FALSE
      ), 
      recapped = case_when(
        release_dwn & grepl("OP", existing_marks) ~ TRUE, 
        release_dwn & grepl("OP", existing_tags) ~ TRUE, 
        TRUE ~ FALSE
      ), 
      recap = as.logical(recap)
    ) %>% 
    mutate(
      existing_tags = ifelse(is.na(existing_tags), "None", existing_tags), 
      tmp_id = 1:n(), 
      pit = ifelse(!is.na(existing_pit), existing_pit, applied_pit), 
      op = ifelse(
        str_detect(existing_tags, "OP"), 
        str_extract(existing_tags, "OP \\d+"), 
        str_extract(applied_tags, "OP \\d+")
      ), 
      op = str_extract(op, "\\d+"), 
      tmp_fish_id = ifelse(
        !is.na(pit), 
        pit, 
        ifelse(!is.na(op), paste0(trap_year, "-", op), tmp_id)
      )
    ) %>% 
    arrange(tmp_fish_id, trapped_date) %>% 
    group_by(tmp_fish_id) %>% 
    mutate(
      current_location = ifelse(
        grepl("Upstream|Above", release_site), 
        "Upstream", 
        ifelse(grepl("Downstream|Below", release_site), "Downstream", NA)
      )
    )
  
  # Final processing with joins
  trap_df <- trap_df %>% 
    left_join(
      trap_df %>% 
        group_by(tmp_fish_id) %>% 
        slice(which.max(trapped_date)) %>% 
        select(tmp_fish_id, final_location = current_location), 
      by = "tmp_fish_id"
    ) %>% 
    select(trap_year, trapped_date, tmp_fish_id, current_location, final_location, everything()) %>% 
    select(-tmp_id, -pit, -op) %>% 
    ungroup()
  
  return(trap_df)
}