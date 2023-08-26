library(plyr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(readxl)
library(ggplot2)
library(scales)
library(gridExtra)
library(purrr)
library(writexl)

create_path <- function(newpath_child) {
  parent_dir <- 'data'
  newpath <- file.path(parent_dir, newpath_child)
  if (!file.exists(newpath)) {
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
  files <- list.files(path, full.names = TRUE)
  for (f in files) {
    tryCatch({
      file.remove(f)
    }, error = function(e) {
      subfiles <- list.files(f, full.names = TRUE)
      for (j in subfiles) {
        tryCatch({
          file.remove(j)
        }, error = function(e) {
          subsubfiles <- list.files(j, full.names = TRUE)
          for (k in subsubfiles) {
            file.remove(k)
          }
        })
      }
    })
  }
}

import_data <- function(throughput_data) {
  cat(" > Loading dataset...\n")
  who <- throughput_data
  iso_mapping <- read_excel("data/input/static/base_entitydetails.xlsx")
  colnames(iso_mapping) <- c('country_name', 'iso_code')
  return(list(who = who, iso_mapping = iso_mapping))
}

convert_data_types <- function(who) {
  cat(" > Converting data types\n") # these rows are technically obsolete because data is read in as float automatically
  who$total_doses <- as.numeric(who$total_doses)
  who$at_least_one_dose <- as.numeric(who$at_least_one_dose)
  who$fully_vaccinated <- as.numeric(who$fully_vaccinated)
  who$persons_booster_add_dose <- as.numeric(who$persons_booster_add_dose)
  return(who)
}

cleaning <- function(who) {
  cat(" > Remove NA...\n")
  who <- who[complete.cases(who$total_doses), ]
  who <- who[who$total_doses >= 0, ]
  
  cat(" > Selecting relevant columns...\n")
  df1 <- who[, c('country_name', 'date', 'total_doses', 'at_least_one_dose', 'fully_vaccinated', 'persons_booster_add_dose', 'date_accessed')]
  return(df1)
}

date_to_date_week <- function(df1) {
  cat(" > Converting date to date_week...\n")
  df1$date <- as.Date(df1$date, format = '%Y-%m-%d')
  df1$date_week <- df1$date + weeks(4 - wday(df1$date, label = TRUE) %% 7)
  
  cat(" > Dropping duplicates...\n")
  df1 <- unique(df1)
  return(df1)
}

map_iso_codes <- function(df1, iso_mapping) {
  cat(" > Mapping ISO codes...\n")
  iso_mapping$country_name <- str_to_title(iso_mapping$country_name)
  df1 <- merge(df1, iso_mapping, by = 'country_name', all.x = TRUE)
  df1[df1$country_name == 'Bonaire, Sint Eustatius And Saba/Saba', 'iso_code'] <- 'SAB' # changed from BES1
  df1[df1$country_name == 'Bonaire, Sint Eustatius And Saba/Sint Eustatius', 'iso_code'] <- 'STA' # changed from BES1
  df1[df1$country_name == 'Bonaire, Sint Eustatius And Saba/Bonaire', 'iso_code'] <- 'BON' # changed from XAA
  df1[df1$country_name == 'Sint Maarten', 'iso_code'] <- 'SXM'
  df1[df1$country_name == 'Pitcairn Islands', 'iso_code'] <- 'PCN'
  df1[df1$country_name == 'Northern Mariana Islands (Commonwealth Of The)', 'iso_code'] <- 'MNP'
  df1[df1$country_name == 'The United Kingdom', 'iso_code'] <- 'GBR'
  df1[df1$country_name == 'Côte D’Ivoire', 'iso_code'] <- 'CIV'
  df1[df1$country_name == 'Falkland Islands (Malvinas)', 'iso_code'] <- 'FLK'
  df1[df1$country_name == 'Liechtenstein', 'iso_code'] <- 'LIE'
  df1[df1$country_name == 'Kosovo', 'iso_code'] <- 'XKX'
  
  cat(" > Identifying countries that have not reported for the latest week...\n")
  max_date_week <- max(df1$date_week)
  df1$max_date_week <- ave(df1$date_week, df1$iso_code, FUN = max)
  df1$is_latest_week_reported <- ifelse(df1$max_date_week == max_date_week, 1, 0)
  df1$max_date_week <- NULL
  
  cat(" > Remove missing values...\n")
  df1[is.na(df1)] <- 0
  df2 <- copy(df1)
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
  country_data <- df2[df2$iso_code == country, , drop = FALSE]
  country_data <- country_data[order(country_data$date, decreasing = TRUE), ]
  return(country_data)
}

printing_log <- function(country_data, log) {
  country_data <- country_data[order(country_data$iso_code), ]
  country_code <- country_data[1, 'iso_code']
  country_name <- country_data[1, 'country_name']
  n_changes <- nrow(log[log$iso_code == country_code, , drop = FALSE])
  cat(" >", sprintf("%3d", n_changes), "obs. removed from", country_code, "(", country_name, ")\n")
}

delete_row <- function(country_data, df, row, log, reset_index = TRUE) {
  if (reset_index) {
    country_data <- country_data[-row, , drop = FALSE]
  }
  country_code <- country_data[row, 'iso_code']
  date <- country_data[row, 'date']
  df[df$iso_code == country_code & df$date == date, 'to_delete_automized_clean'] <- 1
  country_data <- country_data[-row, , drop = FALSE]
  addition <- data.frame(iso_code = country_code, date = date)
  log <- rbind(log, addition)
  return(list(country_data = country_data, df = df, log = log))
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
    res <- Recall(country_data, row + 1, df, log, var_to_clean_iloc)
    country_data <- res$country_data
    df <- res$df
    log <- res$log
  } else {
    return(list(country_data = country_data, df = df, log = log))
  }
  
  previous_value <- country_data[min(row + 1, nrow(country_data)), var_to_clean_iloc]
  current_value <- country_data[min(row, nrow(country_data)), var_to_clean_iloc]
  next_value <- country_data[max(row - 1, 1), var_to_clean_iloc]
  
  if (current_value > next_value) {
    if (previous_value > next_value) {
      res <- deep_clean(country_data, row, df, log, var_to_clean_iloc)
      country_data <- res$country_data
      df <- res$df
      log <- res$log
    } else {
      res <- delete_row(country_data, df, row, log)
      country_data <- res$country_data
      df <- res$df
      log <- res$log
    }
  }
  return(list(country_data = country_data, df = df, log = log))
}

automized_cleaning <- function(df2, uncleaned_df, var_to_clean, delete_errors) {
  cat(" > Starting the automized cleaning process...\n")
  cat(" > Initializing variables...\n")
  log <- data.frame(country = character(), date = character(), stringsAsFactors = FALSE)
  df2$to_delete_automized_clean <- 0
  var_to_clean_iloc <- which(names(df2) == var_to_clean)
  
  cat(" > Looping through all countries to check for decreases in", var_to_clean, "...\n")
  countries <- unique(df2$iso_code)
  countries <- sort(countries)
  for (country in countries) {
    country_data <- filter_country_data(df2, country)
    if (!monotonic(country_data[[var_to_clean]])) {
      while (!monotonic(country_data[[var_to_clean]])) {
        row <- 1
        res <- row_check(country_data, row, df2, log, var_to_clean_iloc)
        country_data <- res$country_data
        df2 <- res$df
        log <- res$log
      }
      printing_log(copy(country_data), copy(log))
      cat(" > Saving plots of cleaned changes to data/cleaning_log...\n")
      if (delete_errors) {
        df2 <- df2[df2$to_delete_automized_clean == 0, , drop = FALSE]
      } else {
        df2[df2$to_delete_automized_clean == 1, var_to_clean] <- NULL
      }
      df2$to_delete_automized_clean <- NULL
      cat(" > Saving logged_changes to csv...\n")
      colnames(log) <- c("deleted dates")
      log <- log[order(log$iso_code, log$deleted.dates), ]
      log$country <- NULL
      write.csv(log, paste0('data/cleaning_log/', var_to_clean, '/decrease_cleaning/logged_changes.csv'), row.names = FALSE)
    }
  }
  return(df2)
}

export_plots_of_changes <- function(df2, uncleaned, country, log, var_to_clean, folder) {
  create_path(paste0("cleaning_log/", folder))
  country_data <- df2[df2$iso_code == country, ]
  uncleaned_c <- uncleaned[uncleaned$iso_code == country, ]
  country_data <- country_data[order(country_data$date, decreasing = TRUE), ]
  country_data <- country_data[country_data$to_delete_automized_clean == 0, ]
  country_data$type_line <- "_cleaned"
  uncleaned_c$type_line <- "_original"
  background_data <- uncleaned_c[c("date", "total_doses", "at_least_one_dose", "fully_vaccinated", "persons_booster_add_dose", "type_line")]
  background_data1 <- background_data[c("date", "total_doses", "type_line")]
  background_data2 <- background_data[c("date", "at_least_one_dose", "type_line")]
  background_data3 <- background_data[c("date", "fully_vaccinated", "type_line")]
  background_data4 <- background_data[c("date", "persons_booster_add_dose", "type_line")]
  background_data1$type_col <- "Total Doses"
  background_data2$type_col <- "At Least One Dose"
  background_data3$type_col <- "Fully Vaccinated"
  background_data4$type_col <- "Persons Booster Add Dose"
  names(background_data1) <- c("date", var_to_clean, "type_line", "type_col")
  names(background_data2) <- c("date", var_to_clean, "type_line", "type_col")
  names(background_data3) <- c("date", var_to_clean, "type_line", "type_col")
  names(background_data4) <- c("date", var_to_clean, "type_line", "type_col")
  background_data <- rbind(background_data1, background_data2, background_data3, background_data4)
  background_data <- background_data[!(background_data$type_col == gsub("_", " ", var_to_clean, fixed = TRUE)), ]
  country_data$type_col <- paste0(gsub("_", " ", var_to_clean, fixed = TRUE), " cleaned")
  uncleaned_c$type_col <- paste0(gsub("_", " ", var_to_clean, fixed = TRUE), " original")
  plot_data <- rbind(background_data,
                     uncleaned_c[c("date", var_to_clean, "type_line", "type_col")],
                     country_data[c("date", var_to_clean, "type_line", "type_col")])
  names(plot_data)[names(plot_data) == var_to_clean] <- "y"
  plot_data$y <- plot_data$y / 1000000
  yaxis <- paste0(gsub("_", " ", var_to_clean, fixed = TRUE), " (in million)")
  customPalette <- c("#AAAAAA", "#C1C1C1", "#D3D3D3", "#6495ED", "#FFA500")
  
  changes <- log[log$iso_code == country, "date"]
  changes <- changes[order(changes)]
  uncleaned_c <- uncleaned_c[order(uncleaned_c$date, decreasing = TRUE), ]
  uncleaned_c <- uncleaned_c[order(uncleaned_c$date), ]
  min_date <- min(uncleaned_c$date)
  max_date <- max(uncleaned_c$date)
  group_together <- FALSE
  count <- 0
  y_jump_list <- vector()
  y_to_show <- vector()
  for (date_change in 1:length(changes)) {
    y_original <- as.numeric(uncleaned_c[uncleaned_c$date == changes[date_change], var_to_clean])
    row_low <- max(which(uncleaned_c$date == changes[date_change]) - 1, 0)
    row_high <- min(which(uncleaned_c$date == changes[date_change]) + 1, length(uncleaned_c$date) - 1)
    y_cleaned <- as.numeric((uncleaned_c[row_low, var_to_clean] + uncleaned_c[row_high, var_to_clean]) / 2)
    y_jump <- abs(y_original - y_cleaned)
    y_jump_list <- c(y_jump_list, y_jump)
    y_to_show <- c(y_to_show,
                   y_original,
                   as.numeric(uncleaned_c[row_low, var_to_clean]),
                   as.numeric(uncleaned_c[row_high, var_to_clean]))
    date_to <- uncleaned_c[max(which(uncleaned_c$date == changes[date_change]) - 15, 0), "date"]
    if (group_together) {
      date_from <- date_from_grouped
    } else {
      date_from <- uncleaned_c[min(which(uncleaned_c$date == changes[date_change]) + 16, length(uncleaned_c$date) - 1), "date"]
    }
    if (date_change < length(changes)) {
      date_from_next_change <- uncleaned_c[min(which(uncleaned_c$date == changes[date_change + 1]) + 16, length(uncleaned_c$date) - 1), "date"]
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
      y_jump_max <- max(y_jump_list) / 1000000
      y_jump_list <- vector()
      count <- count + 1
      plot_data_range <- plot_data[plot_data$date >= date_from, ]
      plot_data_range <- plot_data_range[plot_data$date <= date_to, ]
      
      # zooming in where possible to give the impression of continuous data
      zoom_lower_bound <- as.difftime(-2, units = "days")
      zoom_upper_bound <- as.difftime(-2, units = "days")
      if (min_date + as.difftime(15, units = "days") < date_from) {
        zoom_lower_bound <- as.difftime(2, units = "days")
      }
      if (max_date - as.difftime(15, units = "days") > date_to) {
        zoom_upper_bound <- as.difftime(2, units = "days")
      }
      
      plot(1, type = "n", xlab = "", ylab = yaxis,
           main = paste0(country, ": Change ", count),
           xlim = c(date_from + zoom_lower_bound, date_to - zoom_upper_bound))
      lines(y ~ date, data = plot_data_range, col = customPalette,
            lty = ifelse(plot_data_range$type_line == "_cleaned", "solid", "dashed"),
            pch = ifelse(plot_data_range$type_line == "_cleaned", NA, 1),
            type = "l")
      legend("topright", legend = unique(plot_data_range$type_col), 
             col = customPalette[2:6], lty = c("solid", "dashed"), pch = c(NA, 1),
             inset = 0.02, cex = 0.8)
      ylim <- c(min(y_to_show) / 1000000, max(y_to_show) / 1000000)
      zoom <- TRUE
      if ((diff(ylim) > y_jump_max * 80) & zoom) {
        y_low <- min(y_to_show) / 1000000
        y_high <- max(y_to_show) / 1000000
        y_diff <- y_high - y_low
        y_high <- y_high + y_diff * 5
        y_low <- y_low - y_diff * 5
        y_low <- max(y_low, -0.5)
        y_to_show <- vector()
        if (y_low < 0) {
          y_low <- y_high / (-10)
        }
        ylim <- c(y_low, y_high)
      }
      axis(1, at = pretty(c(date_from + zoom_lower_bound, date_to - zoom_upper_bound)))
      axis(2)
      box()
      dev.copy(png, paste0("data/cleaning_log/", folder, "/cleaning_", country, ifelse(count > 1, paste0("_", count), ""), ".png"))
      dev.off()
    }
  }
}

main <- function(auto_cleaning, throughput_data) {
  # Importing data
  result <- import_data(throughput_data)
  who <- result$who
  iso_mapping <- result$iso_mapping
  
  who <- convert_data_types(who)
  df1 <- cleaning(who)
  df1 <- date_to_date_week(df1)
  result <- map_iso_codes(df1, iso_mapping)
  df1 <- result$df1
  df2 <- result$df2
  
  if (auto_cleaning) {
    clean_path(folder = "cleaning_log")
    uncleaned_df <- df2
    df2 <- automized_cleaning(df2, uncleaned_df, var_to_clean = "total_doses", delete_errors = FALSE)
    df2 <- automized_cleaning(df2, uncleaned_df, var_to_clean = "at_least_one_dose", delete_errors = FALSE)
    df2 <- automized_cleaning(df2, uncleaned_df, var_to_clean = "fully_vaccinated", delete_errors = FALSE)
    df2 <- automized_cleaning(df2, uncleaned_df, var_to_clean = "persons_booster_add_dose", delete_errors = FALSE)
  }
  
  return(df2)
}
