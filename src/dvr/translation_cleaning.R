
create_path <- function(newpath_child) {
  parent_dir <- 'data'
  newpath <- file.path(parent_dir, newpath_child)
  if (!dir.exists(newpath)) {
    cat(" > Creating new folder: data/", newpath_child, "\n")
    dir.create(newpath, recursive = TRUE)
    cat(" > New folder created.\n")
  }
}

clean_path <- function(folder) {
  create_path(folder)
  cat(" > Deleting any pre-existing plots from data/cleaning_log/...\n")
  parent_dir <- 'data'
  path <- file.path(parent_dir, folder)
  files <- list.files(path, recursive = TRUE, full.names = TRUE)
  for (file in files) {
    tryCatch({
      file.remove(file)
    }, error = function(e) {
      sub_files <- list.files(file, recursive = TRUE, full.names = TRUE)
      for (sub_file in sub_files) {
        tryCatch({
          file.remove(sub_file)
        }, error = function(e) {
          sub_sub_files <- list.files(sub_file, recursive = TRUE, full.names = TRUE)
          file.remove(sub_sub_files)
        })
      }
    })
  }
}

import_data <- function(throughput_data) {
  cat(" > Loading dataset...\n")
  # who <- read.csv("data/input/supply_data/analysis_vx_throughput_data.csv")
  who <- throughput_data
  iso_mapping <- read.xlsx("data/input/static/base_entitydetails.xlsx")
  colnames(iso_mapping) <- c("country_name", "iso_code")
  iso_mapping <- iso_mapping[, c("country_name", "iso_code")]
  return(list(who = who, iso_mapping = iso_mapping))
}

convert_data_types <- function(who) {
  cat(" > Converting data types\n")
  who$total_doses <- as.numeric(who$total_doses)
  who$at_least_one_dose <- as.numeric(who$at_least_one_dose)
  who$fully_vaccinated <- as.numeric(who$fully_vaccinated)
  who$persons_booster_add_dose <- as.numeric(who$persons_booster_add_dose)
  return(who)
}

cleaning <- function(who) {
  cat(" > Remove NA...\n")
  who <- who[!is.na(who$total_doses), ]
  who <- who[who$total_doses >= 0, ]
  
  cat(" > Selecting relevant columns...\n")
  df1 <- who[, c('country_name', 'date', 'total_doses', 'at_least_one_dose', 'fully_vaccinated', 'persons_booster_add_dose', 'date_accessed')]
  return(df1)
}

date_to_date_week <- function(df1) {
  cat(" > Converting date to date_week...\n")
  df1$date <- as.Date(df1$date, format = "%Y-%m-%d")
  df1$date_week <- df1$date + lubridate::days((4 - lubridate::wday(df1$date)) %% 7)
  
  cat(" > Dropping duplicates...\n")
  df1 <- unique(df1)
  return(df1)
}

map_iso_codes <- function(df1, iso_mapping) {
  cat(" > Mapping ISO codes...\n")
  iso_mapping$country_name <- tools::toTitleCase(iso_mapping$country_name)
  df1 <- merge(df1, iso_mapping, by = "country_name", all.x = TRUE)
  
  df1[df1$country_name == "Bonaire, Sint Eustatius And Saba/Saba", "iso_code"] <- "SAB"
  df1[df1$country_name == "Bonaire, Sint Eustatius And Saba/Sint Eustatius", "iso_code"] <- "STA"
  df1[df1$country_name == "Bonaire, Sint Eustatius And Saba/Bonaire", "iso_code"] <- "BON"
  df1[df1$country_name == "Sint Maarten", "iso_code"] <- "SXM"
  df1[df1$country_name == "Pitcairn Islands", "iso_code"] <- "PCN"
  df1[df1$country_name == "Northern Mariana Islands (Commonwealth Of The)", "iso_code"] <- "MNP"
  df1[df1$country_name == "The United Kingdom", "iso_code"] <- "GBR"
  df1[df1$country_name == "Côte D’Ivoire", "iso_code"] <- "CIV"
  df1[df1$country_name == "Falkland Islands (Malvinas)", "iso_code"] <- "FLK"
  df1[df1$country_name == "Liechtenstein", "iso_code"] <- "LIE"
  df1[df1$country_name == "Kosovo", "iso_code"] <- "XKX"
  
  cat(" > Identifying countries that have not reported for the latest week...\n")
  max_date_week <- max(df1$date_week)
  df1$max_date_week <- ave(df1$date_week, df1$iso_code, FUN = max)
  df1$is_latest_week_reported <- ifelse(df1$max_date_week == max_date_week, 1, 0)
  df1$max_date_week <- NULL
  
  cat(" > Remove missing values...\n")
  df1[is.na(df1)] <- 0
  df2 <- df1
  
  return(list(df1 = df1, df2 = df2))
}

monotonic <- function(series) {
  if (length(series) <= 1) {
    return(TRUE)
  } else {
    if (series[1] >= series[2]) {
      series <- series[-1]
      return(monotonic(series))
    } else {
      return(FALSE)
    }
  }
}

filter_country_data <- function(df2, country) {
  country_data <- df2[df2$iso_code == country, ]
  country_data <- country_data[order(country_data$date, decreasing = TRUE), ]
  return(country_data)
}

printing_log <- function(country_data, log) {
  country_data <- country_data[order(country_data$date), ]
  country_code <- country_data[1, 'iso_code']
  country_name <- country_data[1, 'country_name']
  n_changes <- sum(log$iso_code == country_code)
  cat(sprintf(" > %3d obs. removed from %s (%s)\n", n_changes, country_code, country_name))
}

delete_row <- function(country_data, df, row, log, reset_index = TRUE) {
  if (reset_index) {
    country_data <- country_data[-row, ]
    country_name <- country_data[row, 'iso_code']
  }
  country_code <- country_data[row, 'iso_code']
  date <- country_data[row, 'date']
  df$to_delete_automized_clean[df$iso_code == country_code & df$date == date] <- 1
  country_data <- country_data[-row, ]
  addition <- data.frame(iso_code = country_code, date = date)
  log <- rbind(log, addition)
  return(list(country_data, df, log))
}

deep_clean <- function(country_data, row, df, log, var_to_clean_iloc) {
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
  return(delete_row(country_data, df, row_to_delete, log))
}

row_check <- function(country_data, row, df, log, var_to_clean_iloc) {
  if (nrow(country_data) > row) {
    result <- Recall(country_data, row + 1, df, log, var_to_clean_iloc)
    country_data <- result[[1]]
    df <- result[[2]]
    log <- result[[3]]
  } else {
    return(list(country_data, df, log))
  }
  
  previous_value <- country_data[min(row + 1, nrow(country_data)), var_to_clean_iloc]
  current_value <- country_data[min(row, nrow(country_data)), var_to_clean_iloc]
  next_value <- country_data[max(row - 1, 1), var_to_clean_iloc]
  
  if (current_value > next_value) {
    if (previous_value > next_value) {
      result <- deep_clean(country_data, row, df, log, var_to_clean_iloc)
      country_data <- result[[1]]
      df <- result[[2]]
      log <- result[[3]]
    } else {
      result <- delete_row(country_data, df, row, log)
      country_data <- result[[1]]
      df <- result[[2]]
      log <- result[[3]]
    }
  }
  return(list(country_data, df, log))
}

export_plots_of_changes <- function(df2, uncleaned, country, log, var_to_clean, folder) {
  # Create directory if it doesn't exist
  dir.create(paste0("cleaning_log/", folder), showWarnings = FALSE)
  
  # Filter data for the respective country
  country_data <- subset(df2, iso_code == country)
  uncleaned_c <- subset(uncleaned, iso_code == country)
  
  # Sort data by date in descending order and remove rows marked for deletion
  country_data <- subset(country_data, to_delete_automized_clean == 0)
  country_data$type_line <- "_cleaned"
  uncleaned_c$type_line <- "_original"
  
  # Prepare background data for plotting
  background_data <- subset(uncleaned_c, select = c("date", "total_doses", "at_least_one_dose",
                                                    "fully_vaccinated", "persons_booster_add_dose", "type_line"))
  background_data1 <- subset(background_data, select = c("date", "total_doses", "type_line"))
  background_data2 <- subset(background_data, select = c("date", "at_least_one_dose", "type_line"))
  background_data3 <- subset(background_data, select = c("date", "fully_vaccinated", "type_line"))
  background_data4 <- subset(background_data, select = c("date", "persons_booster_add_dose", "type_line"))
  background_data1$type_col <- "Total Doses"
  background_data2$type_col <- "At Least One Dose"
  background_data3$type_col <- "Fully Vaccinated"
  background_data4$type_col <- "Persons Booster Add Dose"
  names(background_data1) <- c("date", var_to_clean, "type_line", "type_col")
  names(background_data2) <- c("date", var_to_clean, "type_line", "type_col")
  names(background_data3) <- c("date", var_to_clean, "type_line", "type_col")
  names(background_data4) <- c("date", var_to_clean, "type_line", "type_col")
  background_data <- rbind(background_data1, background_data2, background_data3, background_data4)
  background_data <- subset(background_data, !(type_col == tolower(gsub("_", " ", var_to_clean))))
  
  # Prepare plot data
  country_data$type_col <- paste0(tolower(gsub("_", " ", var_to_clean)), " cleaned")
  uncleaned_c$type_col <- paste0(tolower(gsub("_", " ", var_to_clean)), " original")
  plot_data <- rbind(background_data,
                     subset(uncleaned_c, select = c("date", var_to_clean, "type_line", "type_col")),
                     subset(country_data, select = c("date", var_to_clean, "type_line", "type_col")))
  names(plot_data)[names(plot_data) == var_to_clean] <- "y"
  plot_data$y <- plot_data$y/1000000
  yaxis <- paste0(tolower(gsub("_", " ", var_to_clean)), " (in million)")
  
  # Set custom color palette
  customPalette <- c("#AAAAAA", "#C1C1C1", "#D3D3D3", "#6495ED", "#FFA500")
  
  # Extract changes from the log
  changes <- subset(log, iso_code == country)$date
  changes <- changes[order(changes)]
  uncleaned_c <- uncleaned_c[order(uncleaned_c$date, decreasing = TRUE), ]
  min_date <- min(uncleaned_c$date)
  max_date <- max(uncleaned_c$date)
  group_together <- FALSE
  count <- 0
  y_jump_list <- c()
  y_to_show <- c()
  
  for (date_change in 1:length(changes)) {
    y_original <- as.numeric(subset(uncleaned_c, date == changes[date_change])[var_to_clean])
    row_low <- max(which(uncleaned_c$date == changes[date_change]) - 1, 1)
    row_high <- min(which(uncleaned_c$date == changes[date_change]) + 1, nrow(uncleaned_c))
    y_cleaned <- as.numeric((uncleaned_c[row_low, var_to_clean] + uncleaned_c[row_high, var_to_clean])/2)
    y_jump <- abs(y_original - y_cleaned)
    y_jump_list <- c(y_jump_list, y_jump)
    y_to_show <- c(y_to_show, y_original,
                   as.numeric(uncleaned_c[row_low, var_to_clean]),
                   as.numeric(uncleaned_c[row_high, var_to_clean]))
    date_to <- uncleaned_c[min(which(uncleaned_c$date == changes[date_change]) - 15, nrow(uncleaned_c)), "date"]
    
    if (group_together) {
      date_from <- date_from_grouped
    } else {
      date_from <- uncleaned_c[max(which(uncleaned_c$date == changes[date_change]) + 16, 1), "date"]
    }
    
    if (date_change < length(changes)) {
      date_from_next_change <- uncleaned_c[max(which(uncleaned_c$date == changes[date_change + 1]) + 16, 1), "date"]
      
      if (date_to > (date_from_next_change - as.difftime(2, units = "days"))) {
        group_together <- TRUE
        date_from_grouped <- date_from
      } else {
        group_together <- FALSE
      }
    } else {
      group_together <- FALSE
    }
    
    if (!group_together) {
      y_jump_max <- max(y_jump_list)/1000000
      y_jump_list <- c()
      count <- count + 1
      plot_data_range <- subset(plot_data, date >= date_from)
      plot_data_range <- subset(plot_data_range, date <= date_to)
      
      # Zooming in where possible to give the impression of continuous data
      zoom_lower_bound <- ifelse(min_date + as.difftime(15, units = "days") < date_from, as.difftime(2, units = "days"), as.difftime(-2, units = "days"))
      zoom_upper_bound <- ifelse(max_date - as.difftime(15, units = "days") > date_to, as.difftime(2, units = "days"), as.difftime(-2, units = "days"))
      
      # Clear previous plot
      graphics.off()
      
      # Create a new plot
      plt <- ggplot(data = plot_data_range, aes(x = date, y = y, color = type_col, linetype = type_line)) +
        geom_line() +
        scale_color_manual(values = customPalette) +
        scale_linetype_manual(values = c(1, 1, 1, 1, 1)) +
        labs(title = paste(country, ": Change ", count),
             x = NULL,
             y = yaxis) +
        theme(legend.title = element_blank(),
              legend.position = "bottom") +
        ylim(c(min(y_to_show) / 1000000, max(y_to_show) / 1000000))
      
      plt <- plt + scale_x_datetime(limits = as.POSIXct(c(date_from, date_to)),
                                    date_labels = "%Y-%m-%d",
                                    date_breaks = "1 week",
                                    expand = c(0, 0))
      
      plt <- plt + theme(axis.text.x = element_text(angle = 25, hjust = 1))
      
      # Adjust plot margins
      plt <- plt + theme(plot.margin = margin(20, 20, 50, 20))
      
      # Save the plot
      path <- paste("data/cleaning_log/", folder, "/cleaning_", country, sep = "")
      if (count > 1) {
        path <- paste(path, "_", count, sep = "")
      }
      
      ggsave(filename = paste0(path, ".png"), plot = plt, dpi = 300)
    }
  }
}
  

automized_cleaning <- function(df2, uncleaned_df, var_to_clean, delete_errors) {
  # Starting the automized cleaning process
  cat(" > Starting the automized cleaning process...\n")
  
  # Initializing variables
  log <- data.frame(country = character(), date = character(), stringsAsFactors = FALSE)
  options(mode.chained_assignment = NULL)
  df2$to_delete_automized_clean <- 0
  var_to_clean_iloc <- which(names(df2) == var_to_clean)
  
  # Looping through all countries to check for decreases in var_to_clean
  cat(" > Looping through all countries to check for decreases in", var_to_clean, "...\n")
  countries <- unique(df2$iso_code)
  countries <- sort(countries)
  for (country in countries) {
    country_data <- filter_country_data(df2, country)
    if (!is.monotonic(country_data[[var_to_clean]])) {
      while (!is.monotonic(country_data[[var_to_clean]])) {
        row <- 0
        result <- row_check(country_data, row, df2, log, var_to_clean_iloc)
        country_data <- result$country_data
        df2 <- result$df2
        log <- result$log
      }
      printing_log(copy(country_data), copy(log))
      export_plots_of_changes(df2, uncleaned_df, country, log, var_to_clean, paste(var_to_clean, "/decrease_cleaning", sep = "/"))
    }
  }
  
  # Saving plots of cleaned changes to data/cleaning_log
  cat(" > Saving plots of cleaned changes to data/cleaning_log...\n")
  if (delete_errors) {
    df2 <- df2[df2$to_delete_automized_clean == 0, ]
  } else {
    df2[df2$to_delete_automized_clean == 1, var_to_clean] <- NULL
  }
  df2$to_delete_automized_clean <- NULL
  
  # Saving logged_changes to csv
  cat(" > Saving logged_changes to csv...\n")
  log <- rename(log, "deleted dates" = "date")
  log <- log[order(log$iso_code, log$deleted dates), ]
  log$country <- NULL
  write.csv(log, file = paste("data/cleaning_log/", var_to_clean, "/decrease_cleaning/logged_changes.csv", sep = ""), row.names = FALSE)
  
  return(df2)
}


main <- function(auto_cleaning, throughput_data) {
  who <- import_data(throughput_data)
  iso_mapping <- who$iso_mapping
  who <- convert_data_types(who$who)
  df1 <- cleaning(who)
  df1 <- date_to_date_week(df1)
  result <- map_iso_codes(df1, iso_mapping)
  df1 <- result$df1
  df2 <- result$df2
  if (auto_cleaning) {
    clean_path(folder = "cleaning_log")
    uncleaned_df <- copy(df2)
    df2 <- automized_cleaning(df2, uncleaned_df, var_to_clean = "total_doses", delete_errors = FALSE)
    df2 <- automized_cleaning(df2, uncleaned_df, var_to_clean = "at_least_one_dose", delete_errors = FALSE)
    df2 <- automized_cleaning(df2, uncleaned_df, var_to_clean = "fully_vaccinated", delete_errors = FALSE)
    df2 <- automized_cleaning(df2, uncleaned_df, var_to_clean = "persons_booster_add_dose", delete_errors = FALSE)
  }
  return(df2)
}


  

  

