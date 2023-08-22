library(jsonlite)
library(dplyr)
library(tidyr)
library(readxl)

create_path <- function(newpath_child) {
  parent_dir <- 'data'
  newpath <- file.path(parent_dir, newpath_child)
  if (!dir.exists(newpath)) {
    cat(" > Creating new folder: data/", newpath_child, "\n")
    dir.create(newpath)
    cat(" > New folder created.\n")
  }
}

clean_path <- function(folder) {
  create_path(folder)
  cat(" > Deleting any pre-existing plots from data/cleaning_log/...\n")
  parent_dir <- 'data'
  path <- file.path(parent_dir, folder)
  files <- list.files(path)
  for (f in files) {
    full_path <- file.path(path, f)
    if (file.info(full_path)$isdir) {
      subfiles <- list.files(full_path)
      for (subf in subfiles) {
        sub_full_path <- file.path(full_path, subf)
        file.remove(sub_full_path)
      }
      dir.remove(full_path)
    } else {
      file.remove(full_path)
    }
  }
}

import_data <- function(throughput_data) {
  cat(" > Loading dataset...\n")
  # who <- read.csv("data/input/supply_data/analysis_vx_throughput_data.csv")
  who <- throughput_data
  iso_mapping <- read_excel("data/input/static/base_entitydetails.xlsx")
  colnames(iso_mapping) <- c("country_name", "iso_code")
  return(list(who = who, iso_mapping = iso_mapping))
}

convert_data_types <- function(who) {
  cat(" > Converting data types\n") # These rows are technically obsolete because data is read in as float automatically
  who$total_doses <- as.numeric(who$total_doses)
  who$at_least_one_dose <- as.numeric(who$at_least_one_dose)
  who$fully_vaccinated <- as.numeric(who$fully_vaccinated)
  who$persons_booster_add_dose <- as.numeric(who$persons_booster_add_dose)
  return(who)
}

cleaning <- function(who) {
  cat(" > Remove NA...\n")
  who <- who[complete.cases(who['total_doses']), ]
  who <- who[who$total_doses >= 0, ]
  cat(" > Selecting relevant columns...\n")
  df1 <- who %>%
    select(country_name, date, total_doses, at_least_one_dose, fully_vaccinated, persons_booster_add_dose, date_accessed)
  return(df1)
}

date_to_date_week <- function(df1) {
  cat(" > Converting date to date_week...\n")
  df1$date <- as.Date(df1$date, format = '%Y-%m-%d')
  df1$date_week <- df1$date + lubridate::days((4 - lubridate::wday(df1$date)) %% 7)
  cat(" > Dropping duplicates...\n")
  df1 <- distinct(df1)
  return(df1)
}

map_iso_codes <- function(df1, iso_mapping) {
  cat(" > Mapping ISO codes...\n")
  iso_mapping$country_name <- tools::toTitleCase(iso_mapping$country_name)
  df1 <- df1 %>%
    left_join(iso_mapping %>% select(country_name, iso_code), by = 'country_name') %>%
    mutate(
      iso_code = ifelse(country_name == 'Bonaire, Sint Eustatius And Saba/Saba', 'SAB',
                        ifelse(country_name == 'Bonaire, Sint Eustatius And Saba/Sint Eustatius', 'STA',
                               ifelse(country_name == 'Bonaire, Sint Eustatius And Saba/Bonaire', 'BON',
                                      ifelse(country_name == 'Sint Maarten', 'SXM',
                                             ifelse(country_name == 'Pitcairn Islands', 'PCN',
                                                    ifelse(country_name == 'Northern Mariana Islands (Commonwealth Of The)', 'MNP',
                                                           ifelse(country_name == 'The United Kingdom', 'GBR',
                                                                  ifelse(country_name == 'Côte D’Ivoire', 'CIV',
                                                                         ifelse(country_name == 'Falkland Islands (Malvinas)', 'FLK',
                                                                                ifelse(country_name == 'Liechtenstein', 'LIE',
                                                                                       ifelse(country_name == 'Kosovo', 'XKX', iso_code)))))))))),
                        is_latest_week_reported = ifelse(date_week == max(date_week), 1, 0)
      ) %>%
        mutate_at(vars(starts_with('total_doses'), starts_with('at_least_one_dose'), starts_with('fully_vaccinated'), starts_with('persons_booster_add_dose')), ~ replace_na(., 0))
      return(df1)
}

automized_cleaning <- function(df2, uncleaned_df, var_to_clean, delete_errors) {
  cat(" > Starting the automized cleaning process...\n")
  cat(" > Initializing variables...\n")
  log <- data.frame(country = character(0), date = character(0), stringsAsFactors = FALSE)
  options(dplyr.summarise.inform = FALSE)
  df2$to_delete_automized_clean <- 0
  var_to_clean_iloc <- match(var_to_clean, colnames(df2))
  cat(" > Looping through all countries to check for decreases in", var_to_clean, "...\n")
  countries <- unique(df2$iso_code)
  countries <- sort(countries)
  for (country in countries) {
    country_data <- df2 %>%
      filter(iso_code == country) %>%
      arrange(desc(date))
    if (!all(diff(country_data[, var_to_clean]) >= 0)) {
      while (!all(diff(country_data[, var_to_clean]) >= 0)) {
        row <- 1
        result <- row_check(country_data, row, df2, log, var_to_clean_iloc)
        country_data <- result[[1]]
        df2 <- result[[2]]
        log <- result[[3]]
      }
      cat(" > Changes made for country:", country, "\n")
      export_plots_of_changes(df2, uncleaned_df, country, log, var_to_clean, var_to_clean)
    }
  }
  cat(" > Saving plots of cleaned changes to data/cleaning_log...\n")
  if (delete_errors) {
    df2 <- df2 %>%
      filter(to_delete_automized_clean == 0)
  } else {
    df2 <- df2 %>%
      mutate_at(vars(starts_with(var_to_clean)), ~ ifelse(to_delete_automized_clean == 1, NA, .))
  }
  df2 <- df2 %>%
    select(-to_delete_automized_clean)
  cat(" > Saving logged_changes to csv...\n")
  log <- log %>%
    rename("deleted dates" = date) %>%
    arrange(iso_code, `deleted dates`) %>%
    select(-country)
  write.csv(log, file = paste0("data/cleaning_log/", var_to_clean, "/decrease_cleaning/logged_changes.csv"), row.names = FALSE)
  return(df2)
}

row_check <- function(country_data, row, df2, log, var_to_clean_iloc) {
  if (nrow(country_data) > row) {
    result <- row_check(country_data, row + 1, df2, log, var_to_clean_iloc)
    country_data <- result[[1]]
    df2 <- result[[2]]
    log <- result[[3]]
  } else {
    return(list(country_data, df2, log))
  }
  
  previous_value <- country_data[min(row + 1, nrow(country_data)), var_to_clean_iloc]
  current_value <- country_data[min(row, nrow(country_data)), var_to_clean_iloc]
  next_value <- country_data[max(row - 1, 1), var_to_clean_iloc]
  
  if (current_value > next_value) {
    if (previous_value > next_value) {
      result <- deep_clean(country_data, row, df2, log, var_to_clean_iloc)
      country_data <- result[[1]]
      df2 <- result[[2]]
      log <- result[[3]]
    } else {
      result <- delete_row(country_data, df2, row, log)
      country_data <- result[[1]]
      df2 <- result[[2]]
      log <- result[[3]]
    }
  }
  
  return(list(country_data, df2, log))
}

deep_clean <- function(country_data, row, df2, log, var_to_clean_iloc) {
  count_previous_larger <- 0
  count_after_smaller <- 0
  
  row_backwards_check <- row
  row_forward_check <- row - 1
  not_exhausted <- TRUE
  next_value <- country_data[min(row - 1, nrow(country_data)), var_to_clean_iloc]
  current_value <- country_data[min(row, nrow(country_data)), var_to_clean_iloc]
  
  while (next_value < country_data[min(row_backwards_check, nrow(country_data)), var_to_clean_iloc] && not_exhausted) {
    count_previous_larger <- count_previous_larger + 1
    row_backwards_check <- row_backwards_check + 1
    if (row_backwards_check > nrow(country_data)) {
      not_exhausted <- FALSE
    }
  }
  
  not_exhausted <- TRUE
  while (current_value > country_data[max(row_forward_check, 1), var_to_clean_iloc] && not_exhausted) {
    count_after_smaller <- count_after_smaller + 1
    row_forward_check <- row_forward_check - 1
    if (row_forward_check < 1) {
      not_exhausted <- FALSE
    }
  }
  
  row_to_delete <- row
  if (count_previous_larger > count_after_smaller) {
    row_to_delete <- row_to_delete - 1
  }
  
  result <- delete_row(country_data, df2, row_to_delete, log)
  country_data <- result[[1]]
  df2 <- result[[2]]
  log <- result[[3]]
  
  return(list(country_data, df2, log))
}

delete_row <- function(country_data, df2, row, log) {
  country_code <- country_data[row, 'iso_code']
  date <- country_data[row, 'date']
  
  df2 <- df2 %>%
    mutate(to_delete_automized_clean = ifelse(iso_code == country_code & date == date, 1, to_delete_automized_clean))
  
  country_data <- country_data[-row, , drop = FALSE]
  addition <- data.frame(iso_code = country_code, date = date)
  log <- bind_rows(log, addition)
  
  return(list(country_data, df2, log))
}

export_plots_of_changes <- function(df2, uncleaned_df, country, log, var_to_clean, folder) {
  create_path(file.path("cleaning_log", folder))
  
  country_data <- df2 %>%
    filter(iso_code == country) %>%
    arrange(desc(date))
  
  uncleaned_c <- uncleaned_df %>%
    filter(iso_code == country) %>%
    arrange(desc(date))
  
  country_data <- country_data %>%
    filter(to_delete_automized_clean == 0)
  
  country_data$type_line <- '_cleaned'
  uncleaned_c$type_line <- '_original'
  
  background_data <- uncleaned_c %>%
    select(date, total_doses, at_least_one_dose, fully_vaccinated, persons_booster_add_dose, type_line)
  
  background_data1 <- background_data %>%
    select(date, total_doses, type_line) %>%
    rename(y = total_doses)
  
  background_data2 <- background_data %>%
    select(date, at_least_one_dose, type_line) %>%
    rename(y = at_least_one_dose)
  
  background_data3 <- background_data %>%
    select(date, fully_vaccinated, type_line) %>%
    rename(y = fully_vaccinated)
  
  background_data4 <- background_data %>%
    select(date, persons_booster_add_dose, type_line) %>%
    rename(y = persons_booster_add_dose)
  
  background_data1$type_col <- 'Total Doses'
  background_data2$type_col <- 'At Least One Dose'
  background_data3$type_col <- 'Fully Vaccinated'
  background_data4$type_col <- 'Persons Booster Add Dose'
  
  background_data <- bind_rows(background_data1, background_data2, background_data3, background_data4)
  
  background_data <- background_data %>%
    filter(type_col != var_to_clean)
  
  country_data$type_col <- paste0(var_to_clean, ' cleaned')
  uncleaned_c$type_col <- paste0(var_to_clean, ' original')
  
  plot_data <- bind_rows(
    background_data,
    uncleaned_c %>%
      select(date, var_to_clean, type_line, type_col),
    country_data %>%
      select(date, var_to_clean, type_line, type_col)
  )
  
  plot_data$y <- plot_data$var_to_clean / 1000000
  yaxis <- paste0(var_to_clean, ' (in million)')
  
  customPalette <- c("#AAAAAA", "#C1C1C1", "#D3D3D3", "#6495ED", "#FFA500")
  
  changes <- log %>%
    filter(iso_code == country) %>%
    arrange(date) %>%
    pull(date)
  
  uncleaned_c <- uncleaned_c %>%
    arrange(desc(date)) %>%
    mutate(row_number = row_number())
  
  min_date <- min(uncleaned_c$date)
  max_date <- max(uncleaned_c$date)
  group_together <- FALSE
  count <- 0
  y_jump_list <- vector("numeric")
  y_to_show <- vector("numeric")
  
  for (date_change in 1:length(changes)) {
    y_original <- uncleaned_c %>%
      filter(date == changes[date_change]) %>%
      pull(var_to_clean)
    
    row_low <- max(which(uncleaned_c$date == changes[date_change]) - 1, 1)
    row_high <- min(which(uncleaned_c$date == changes[date_change]) + 1, nrow(uncleaned_c))
    
    y_cleaned <- (uncleaned_c[row_low, var_to_clean] + uncleaned_c[row_high, var_to_clean]) / 2
    
    y_jump <- abs(y_original - y_cleaned)
    y_jump_list <- append(y_jump_list, y_jump)
    y_to_show <- append(y_to_show, c(y_original, uncleaned_c[row_low, var_to_clean], uncleaned_c[row_high, var_to_clean]))
    
    date_to <- uncleaned_c[min(which(uncleaned_c$date == changes[date_change]) - 15, nrow(uncleaned_c)), 'date']
    
    if (group_together) {
      date_from <- date_from_grouped
    } else {
      date_from <- uncleaned_c[max(which(uncleaned_c$date == changes[date_change]) + 16, 1), 'date']
    }
    
    if (date_change < length(changes)) {
      date_from_next_change <- uncleaned_c[max(which(uncleaned_c$date == changes[date_change + 1]) + 16, 1), 'date']
      
      if (date_to > (date_from_next_change - as.difftime(2, units = 'days'))) {
        group_together <- TRUE
        date_from_grouped <- date_from
      } else {
        group_together <- FALSE
      }
    } else {
      group_together <- FALSE
    }
    
    if (!group_together) {
      y_jump_max <- max(y_jump_list) / 1000000
      y_jump_list <- vector("numeric")
      count <- count + 1
      
      plot_data_range <- plot_data %>%
        filter(date >= date_from)
      
      plot_data_range <- plot_data_range %>%
        filter(date <= date_to)
      
      if (min_date + as.difftime(15, units = 'days') < date_from) {
        zoom_lower_bound <- as.difftime(2, units = 'days')
      } else {
        zoom_lower_bound <- as.difftime(-2, units = 'days')
      }
      
      if (max_date - as.difftime(15, units = 'days') > date_to) {
        zoom_upper_bound <- as.difftime(2, units = 'days')
      } else {
        zoom_upper_bound <- as.difftime(-2, units = 'days')
      }
      
      plot <- plot_data_range %>%
        ggplot(aes(x = date, y = y, color = type_col, linetype = type_line)) +
        geom_line() +
        scale_color_manual(values = customPalette) +
        scale_linetype_manual(values = c('_cleaned' = 'solid', '_original' = 'dotted')) +
        labs(
          title = paste(country, ": Change ", count, sep = ""),
          x = NULL,
          y = yaxis
        ) +
        theme_minimal() +
        theme(legend.position = 'top', axis.text.x = element_text(angle = 25, hjust = 1)) +
        coord_cartesian(xlim = c(date_from + zoom_lower_bound, date_to - zoom_upper_bound)) +
        guides(color = guide_legend(override.aes = list(linetype = c('_cleaned' = 'solid', '_original' = 'dotted'))))
      
      path <- file.path("cleaning_log", folder, paste0("cleaning_", country))
      if (count > 1) {
        path <- paste0(path, "_", count)
      }
      ggsave(path = paste0(path, ".png"), plot = plot, dpi = 300)
    }
  }
}

automized_cleaning <- function(df2, uncleaned_df, var_to_clean, delete_errors) {
  print(" > Starting the automized cleaning process...")
  print(" > Initializing variables...")
  
  log <- data.frame(country = character(0), date = as.Date(character(0)), stringsAsFactors = FALSE)
  options(dplyr.summarise.inform = FALSE)
  df2$to_delete_automized_clean <- 0
  var_to_clean_iloc <- which(names(df2) == var_to_clean)
  
  print(" > Looping through all countries to check for decreases in", var_to_clean,"...")
  
  countries <- df2$iso_code %>%
    unique() %>%
    sort()
  
  for (country in countries) {
    country_data <- df2 %>%
      filter(iso_code == country) %>%
      select(country_name, date, iso_code, total_doses, at_least_one_dose, fully_vaccinated, persons_booster_add_dose, date_accessed)
    
    if (!monotonic(country_data[[var_to_clean]])) {
      while (!monotonic(country_data[[var_to_clean]])) {
        row <- 1
        result <- row_check(country_data, row, df2, log, var_to_clean_iloc)
        country_data <- result[[1]]
        df2 <- result[[2]]
        log <- result[[3]]
      }
      
      printing_log(country_data, log)
      export_plots_of_changes(df2, uncleaned_df, country, log, var_to_clean, file.path(var_to_clean, "decrease_cleaning"))
    }
  }
  
  print(" > Saving plots of cleaned changes to data/cleaning_log...")
  
  if (delete_errors) {
    df2 <- df2 %>%
      filter(to_delete_automized_clean == 0)
  } else {
    df2 %>%
      mutate(!!sym(var_to_clean) := if_else(to_delete_automized_clean == 1, NA, !!sym(var_to_clean)))
  }
  
  df2 <- df2 %>%
    select(-to_delete_automized_clean)
  
  print(" > Saving logged_changes to csv...")
  log %>%
    filter(iso_code %in% countries) %>%
    select(-country) %>%
    arrange(iso_code, date) %>%
    rename(`deleted dates` = date) %>%
    write_csv(file.path(var_to_clean, "decrease_cleaning", "logged_changes.csv"))
  
  return(df2)
}

main <- function(auto_cleaning, throughput_data) {
  who_iso_mapping <- import_data(throughput_data)
  who <- who_iso_mapping$who
  iso_mapping <- who_iso_mapping$iso_mapping
  who <- convert_data_types(who)
  df1 <- cleaning(who)
  df1 <- date_to_date_week(df1)
  result <- map_iso_codes(df1, iso_mapping)
  df1 <- result$df1
  df2 <- result$df2
  
  if (auto_cleaning) {
    clean_path("cleaning_log")
    uncleaned_df <- df2
    df2 <- automized_cleaning(df2, uncleaned_df, "total_doses", FALSE)
    df2 <- automized_cleaning(df2, uncleaned_df, "at_least_one_dose", TRUE)
    df2 <- automized_cleaning(df2, uncleaned_df, "fully_vaccinated", TRUE)
    df2 <- automized_cleaning(df2, uncleaned_df, "persons_booster_add_dose", TRUE)
  }
  
  # Rest of the code...
}

# Call the main function with auto_cleaning set to TRUE and the throughput_data as a parameter
main(TRUE, throughput_data)
