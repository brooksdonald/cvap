library(tidyverse)
library(lubridate)
library(writexl)
library(readxl)
library(stringr)

create_path <- function(newpath_child) {
  parent_dir <- "data"
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
  parent_dir <- "data"
  path <- file.path(parent_dir, folder)
  files <- list.files(path)
  for (f in files) {
    file_path <- file.path(path, f)
    if (file.info(file_path)$isdir) {
      subfiles <- list.files(file.path(path, f))
      for (j in subfiles) {
        subfile_path <- file.path(file.path(path, f), j)
        if (file.info(subfile_path)$isdir) {
          subsubfiles <- list.files(subfile_path)
          for (k in subsubfiles) {
            subsubfile_path <- file.path(subfile_path, k)
            file.remove(subsubfile_path)
          }
        } else {
          file.remove(subfile_path)
        }
      }
    } else {
      file.remove(file_path)
    }
  }
}

import_data <- function(throughput_data) {
  cat(" > Loading dataset...\n")
  # who <- read.csv("data/input/supply_data/analysis_vx_throughput_data.csv")
  who <- throughput_data
  iso_mapping <- read_excel("data/input/static/base_entitydetails.xlsx")
  iso_mapping <- rename(iso_mapping, country_name = NAMEWORKEN, iso_code = CODE)
  iso_mapping <- iso_mapping[c("country_name", "iso_code")]
  return(list(who, iso_mapping))
}

convert_data_types <- function(who) {
  cat(" > Converting data types\n") ## these rows are technically obsolete because data is read in as float automatically
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
  df1 <- who %>% select(country_name, date, total_doses, at_least_one_dose, fully_vaccinated, persons_booster_add_dose, date_accessed)
  return(df1)
}

date_to_date_week <- function(df1) {
  cat(" > Converting date to date_week...\n")
  df1$date <- as.Date(df1$date, format = "%Y-%m-%d")
  df1$date_week <- df1$date + lubridate::days((4 - lubridate::wday(df1$date)) %% 7)
  
  cat(" > Dropping duplicates...\n")
  df1 <- df1[!duplicated(df1), ]
  return(df1)
}

map_iso_codes <- function(df1, iso_mapping) {
  cat(" > Mapping ISO codes...\n")
  iso_mapping$country_name <- str_to_title(iso_mapping$country_name)
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
  df1 <- df1[, !names(df1) %in% c("max_date_week")]
  
  cat(" > Remove missing values...\n")
  df1[is.na(df1)] <- 0
  df2 <- df1
  return(list(df1, df2))
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
  country_data <- country_data[order(country_data$date, decreasing = TRUE), ]
  country_code <- country_data[1, "iso_code"]
  country_name <- country_data[1, "country_name"]
  n_changes <- nrow(log[log$iso_code == country_code, ])
  cat(sprintf(" > %3d obs. removed from %s (%s)\n", n_changes, country_code, country_name))
}

delete_row <- function(country_data, df, row, log, reset_index = TRUE) {
  if (reset_index) {
    country_data <- country_data[-row, ]
    country_name <- country_data[1, "iso_code"]
  }
  country_code <- country_data[row, "iso_code"]
  date <- country_data[row, "date"]
  df[to_delete_automized_clean] <- ifelse(df$iso_code == country_code & df$date == date, 1, df[to_delete_automized_clean])
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
  
  if (current_value > next_value) { # is it larger than next one?
    if (previous_value > next_value) { # is previous larger than next?
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
  create_path(paste0("cleaning_log/", folder))
  country_data <- df2[df2$iso_code == country, ]
  uncleaned_c <- uncleaned[uncleaned$iso_code == country, ]
  country_data <- country_data[order(country_data$date, decreasing = TRUE), ]
  country_data <- country_data[country_data$to_delete_automized_clean == 0, ]
  country_data$type_line <- "_cleaned"
  uncleaned_c$type_line <- "_original"
  background_data <- uncleaned_c[, c("date", "total_doses", "at_least_one_dose", "fully_vaccinated", "persons_booster_add_dose", "type_line")]
  background_data1 <- background_data[, c("date", "total_doses", "type_line")]
  background_data2 <- background_data[, c("date", "at_least_one_dose", "type_line")]
  background_data3 <- background_data[, c("date", "fully_vaccinated", "type_line")]
  background_data4 <- background_data[, c("date", "persons_booster_add_dose", "type_line")]
  background_data1$type_col <- "Total Doses"
  background_data2$type_col <- "At Least One Dose"
  background_data3$type_col <- "Fully Vaccinated"
  background_data4$type_col <- "Persons Booster Add Dose"
  colnames(background_data1) <- c("date", var_to_clean, "type_line", "type_col")
  colnames(background_data2) <- c("date", var_to_clean, "type_line", "type_col")
  colnames(background_data3) <- c("date", var_to_clean, "type_line", "type_col")
  colnames(background_data4) <- c("date", var_to_clean, "type_line", "type_col")
  background_data <- rbind(background_data1, background_data2, background_data3, background_data4)
  background_data <- background_data[!(background_data$type_col == gsub("_", " ", var_to_clean)), ]
  country_data$type_col <- paste0(gsub("_", " ", var_to_clean), " cleaned")
  uncleaned_c$type_col <- paste0(gsub("_", " ", var_to_clean), " original")
  plot_data <- rbind(background_data,
                     uncleaned_c[, c("date", var_to_clean, "type_line", "type_col")],
                     country_data[, c("date", var_to_clean, "type_line", "type_col")])
  colnames(plot_data)[which(names(plot_data) == var_to_clean)] <- "y"
  plot_data$y <- plot_data$y / 1000000
  yaxis <- paste0(gsub("_", " ", var_to_clean), " (in million)")
  customPalette <- c("#AAAAAA", "#C1C1C1", "#D3D3D3", "#6495ED", "#FFA500")
  
  changes <- log$log[log$iso_code == country, "date"]
  changes <- changes[order(changes)]
  uncleaned_c <- uncleaned_c[order(uncleaned_c$date, decreasing = TRUE), ]
  min_date <- min(uncleaned_c$date)
  max_date <- max(uncleaned_c$date)
  group_together <- FALSE
  count <- 0
  y_jump_list <- c()
  y_to_show <- c()
  for (date_change in 1:length(changes)) {
    y_original <- as.numeric(uncleaned_c[uncleaned_c$date == changes[date_change], var_to_clean])
    row_low <- max(which(uncleaned_c$date == changes[date_change]) - 1, 1)
    row_high <- min(which(uncleaned_c$date == changes[date_change]) + 1, nrow(uncleaned_c))
    y_cleaned <- (uncleaned_c[row_low, var_to_clean] + uncleaned_c[row_high, var_to_clean]) / 2
    y_jump <- abs(y_original - y_cleaned)
    y_jump_list <- c(y_jump_list, y_jump)
    y_to_show <- c(y_to_show,
                   y_original,
                   uncleaned_c[row_low, var_to_clean],
                   uncleaned_c[row_high, var_to_clean])
    date_to <- uncleaned_c[which(uncleaned_c$date == changes[date_change]) - 15, "date"]
    if (group_together) {
      date_from <- date_from_grouped
    } else {
      date_from <- uncleaned_c[which(uncleaned_c$date == changes[date_change]) + 16, "date"]
    }
    if (date_change < length(changes)) {
      date_from_next_change <- uncleaned_c[which(uncleaned_c$date == changes[date_change + 1]) + 16, "date"]
      if (date_to > (date_from_next_change - days(2))) {
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
      y_jump_list <- c()
      count <- count + 1
      plot_data_range <- plot_data[plot_data$date >= date_from, ]
      plot_data_range <- plot_data_range[plot_data_range$date <= date_to, ]
      zoom_lower_bound <- days(-2)
      zoom_upper_bound <- days(-2)
      if (min_date + days(15) < date_from) {
        zoom_lower_bound <- days(2)
      }
      if (max_date - days(15) > date_to) {
        zoom_upper_bound <- days(2)
      }
      plot.new()
      plot.window(xlim = as.numeric(range(plot_data_range$date)),
                  ylim = c(min(y_to_show) / 1000000, max(y_to_show) / 1000000))
      box()
      axis(1, at = as.numeric(plot_data_range$date),
           labels = format(plot_data_range$date, "%Y-%m-%d"), las = 2)
      axis(2, las = 1)
      title(main = paste(country, ": Change ", count), xlab = NA, ylab = yaxis)
      lines(plot_data_range$date, plot_data_range$y,
            col = customPalette[as.numeric(plot_data_range$type_col)],
            lty = as.numeric(plot_data_range$type_line))
      legend("topright", legend = unique(plot_data_range$type_col),
             col = customPalette[1:5], lty = c(1, 1, 1, 1, 1), cex = 0.8)
      if ((max(y_to_show) - min(y_to_show) > y_jump_max * 80) & zoom) {
        y_low <- min(y_to_show) / 1000000
        y_high <- max(y_to_show) / 1000000
        y_diff <- y_high - y_low
        y_high <- y_high + y_diff * 5
        y_low <- y_low - y_diff * 5
        y_low <- max(y_low, -0.5)
        y_to_show <- c()
        if (y_low < 0) {
          y_low <- y_high / (-10)
        }
        par(usr = c(as.numeric(range(plot_data_range$date)), c(y_low, y_high)))
        plot.window(xlim = as.numeric(range(plot_data_range$date)),
                    ylim = c(y_low, y_high))
        box()
        axis(1, at = as.numeric(plot_data_range$date),
             labels = format(plot_data_range$date, "%Y-%m-%d"), las = 2)
        axis(2, las = 1)
        title(main = paste(country, ": Change ", count), xlab = NA, ylab = yaxis)
        lines(plot_data_range$date, plot_data_range$y,
              col = customPalette[as.numeric(plot_data_range$type_col)],
              lty = as.numeric(plot_data_range$type_line))
        legend("topright", legend = unique(plot_data_range$type_col),
               col = customPalette[1:5], lty = c(1, 1, 1, 1, 1), cex = 0.8)
      }
      path <- paste0("data/cleaning_log/", folder, "/cleaning_", country)
      if (count > 1) {
        path <- paste0(path, "_", count)
      }
      path <- paste0(path, ".png")
      dev.copy(png, path)
      dev.off()
    }
  }
}

automized_cleaning <- function(df2, uncleaned_df, var_to_clean, delete_errors) {
  print(" > Starting the automized cleaning process...")
  print(" > Initializing variables...")
  log <- data.frame(country = character(), date = character(), stringsAsFactors = FALSE)
  options(warn = -1)
  df2$to_delete_automized_clean <- 0
  var_to_clean_iloc <- which(names(df2) == var_to_clean)
  
  print(paste(" > Looping through all countries to check for decreases in", var_to_clean, "..."))
  countries <- unique(df2$iso_code)
  countries <- sort(countries)
  for (country in countries) {
    country_data <- filter_country_data(df2, country)
    if (!is.monotonic(country_data[[var_to_clean]])) {
      while (!is.monotonic(country_data[[var_to_clean]])) {
        row <- 0
        country_data <- row_check(country_data, row, df2, log, var_to_clean_iloc)
      }
      printing_log(copy(country_data), copy(log))
      export_plots_of_changes(df2, uncleaned_df, country, log, var_to_clean, paste0(var_to_clean, "/decrease_cleaning"))
    }
  }
  print(" > Saving plots of cleaned changes to data/cleaning_log...")
  if (delete_errors) {
    df2 <- df2[df2$to_delete_automized_clean == 0, ]
  } else {
    df2[df2$to_delete_automized_clean == 1, var_to_clean] <- NULL
  }
  df2$to_delete_automized_clean <- NULL
  print(" > Saving logged_changes to csv...")
  names(log) <- c("deleted_dates")
  log <- log[order(log$iso_code, log$deleted_dates), ]
  log$country <- NULL
  write.csv(log, file = paste0("data/cleaning_log/", var_to_clean, "/decrease_cleaning/logged_changes.csv"), row.names = FALSE)
  return(df2)
}

main <- function(auto_cleaning, throughput_data) {
  who <- iso_mapping <- import_data(throughput_data)
  who <- convert_data_types(who)
  df1 <- cleaning(who)
  df1 <- date_to_date_week(df1)
  df1 <- df2 <- map_iso_codes(df1, iso_mapping)
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

  
