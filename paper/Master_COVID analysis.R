# COVID-19 Vaccination Analysis
# Author: BROOKS, Donald J.

# Setup -------------------------------------------------------------------

# Clear environment
rm(list = ls())
gc()

# Set working directory
setwd("C:/Users/brooksd/OneDrive - World Health Organization/Documents/GitHub/cvap")

# Load packages
library("tidyverse")
library("readxl")
library("writexl")
library("lubridate")
library("countrycode")
library("data.table")
library("httr")
library("scales")
library("jsonlite")
library("ggpattern")
library("ggrepel")
library("scales")
library("treemap")
library("ggtext")
library("ggforce")
library("tibble")
library("countrycode")
library("kableExtra")
library("knitr")


# Process variables --------------------------------------------------------

path_save <- "C:/Users/brooksd/OneDrive - World Health Organization/Desktop"


# Extract data ------------------------------------------------------------

# Load base data
raw_data_time <- data.frame(
  read_excel("data/output/output_master.xlsx",
             sheet = "1_stock"))

raw_data_end <- data.frame(
  read_excel("data/output/output_master.xlsx",
             sheet = "0_base_data"))



# Load analyses -----------------------------------------------------------

source("paper/1_static values.R")
source("paper/2_reporting analysis.R")
source("paper/3_daily vaccination rate analysis.R")




# Figure - reporting over time, all ---------------------------------------

combo <- count_time %>%
  filter(date < as.Date("2024-01-01")) %>%
  mutate(group = factor(group, levels = c("total", "old", "hcw"))) %>%
  ggplot(aes(x = date, y = count_month, 
             fill = factor(type, levels = c("Complete primary series", "Boosters")))) +
  geom_col() +
  geom_line(aes(y = count_cumul, 
                color = factor(type, levels = c("Complete primary series", "Boosters"))), 
            linewidth = 1.25) +
  scale_color_manual(values = c("#D86422", "#000066"), 
                     labels = c("Complete primary series only", "Complete primary series & boosters")) +
  scale_fill_manual(values = c("#A2D2F0", "#1E7FB8"), 
                    labels = c("Complete primary series only", "Complete primary series & boosters")) +
  scale_x_date(date_labels = "%b %Y") +
  coord_cartesian(ylim = c(0, 200)) +
  # geom_hline(aes(yintercept = 194), linetype = "dashed", color = "black") +
  # annotate("text", x = as.Date("2023-04-01"), y = 194,
  #          label = "194 (Total number of WHO Member States)",
  #          color = "black", hjust = 1, vjust = -1, size = 3, fontface = "italic") +
  theme_test() + 
  labs(title = "Member State reporting by population group, cumulative and monthly",
       y = "Count of reporting Member States ",
       caption = "Data source: eJRF and WHO regional office reporting systems") + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(vjust = 4, size = 11),
        axis.text.y = element_text(size = 10),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.justification = "left",
        plot.title = element_text(face = "bold", vjust = 3, size = 14),
        plot.subtitle = element_text(size = 8, face = "italic", vjust = 7),
        plot.caption = element_text(hjust = 0, size = 8, vjust = -3),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) +
  facet_wrap(~group)

combo

ggsave(
  path = path_save,
  filename = "fig_rep_disag_time.jpg",
  plot = combo,
  width = 10,
  height = 6,
  dpi = 300
)



# Figure - reporting % over time, by income group -------------------------

fig_report_cumul_inc <- count_time_inc %>%
  mutate(population = factor(population, levels = c("Total population",
                                                    "Older adults",
                                                    "Health and care workers")),
         type = factor(type, levels = c("Complete primary series",
                                        "Booster")),
         WBINCOME_LONG_STATUS = factor(WBINCOME_LONG_STATUS, levels = c("High income",
                                                                        "Upper middle income",
                                                                        "Lower middle income",
                                                                        "Low income"))) %>%
  ggplot(aes(x = DATE, y = count_per, color = WBINCOME_LONG_STATUS)) +
  geom_line(size = 1.5) +
  # scale_color_manual(values = c("#FFBB30", "#5200AE", "#C8D65B", "#00AE8F")) +
  # scale_x_date(date_labels = "%b %Y") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_test() + 
  labs(title = "Coverage in health and care workers, global",
       y = "Percentage of health and care workers",
       caption = "Data source: WHO-UNICEF electronic Joint Reporting Form COVID-19 module",
       color = "Legend") + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(vjust = 4, size = 11),
        axis.text.y = element_text(size = 10),
        legend.position = "bottom",
        legend.justification = "left",
        plot.title = element_text(face = "bold", vjust = 3, size = 14),
        plot.subtitle = element_text(size = 8, face = "italic", vjust = 7),
        plot.caption = element_text(hjust = 0, size = 8, vjust = -3),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) +
  facet_grid(type ~ population, scales = "free", space = "free") 
  # facet_wrap(~group, ncol = 1) + 
  # guides(color = guide_legend(ncol = 6, override.aes = list(size = 3)))

fig_report_cumul_inc

ggsave(
  path = path_save,
  filename = "fig_report_cumul_inc.jpg",
  plot = fig_report_cumul_inc,
  width = 10,
  height = 6,
  dpi = 300
)



# Figure - reporting over time, older adults ------------------------------

combo_old <- sum_old_cps %>%
  ggplot(aes(x = DATE, y = count_month, 
             fill = factor(type_cumul, levels = c("Complete primary series", "Boosters")))) +
  geom_col() +
  geom_line(aes(y = count_cumul, 
                color = factor(type_cumul, levels = c("Complete primary series", "Boosters"))), 
            size = 1.25) +
  scale_color_manual(values = c("#D86422", "#000066"), 
                     labels = c("Complete primary series only", "Complete primary series & boosters")) +
  scale_fill_manual(values = c("#A2D2F0", "#1E7FB8"), 
                    labels = c("Complete primary series only", "Complete primary series & boosters")) +
  scale_x_date(date_labels = "%b %Y") +
  coord_cartesian(ylim = c(0, 200)) +
  geom_hline(aes(yintercept = 194), linetype = "dashed", color = "black") +
  annotate("text", x = as.Date("2023-04-01"), y = 194,
           label = "194 (Total number of WHO Member States)",
           color = "black", hjust = 1, vjust = -1, size = 3, fontface = "italic") +
  theme_test() + 
  labs(title = "Older adults",
       y = "Count of reporting Member States ",
       caption = "Data source: eJRF and WHO regional office reporting systems") + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(vjust = 4, size = 11),
        axis.text.y = element_text(size = 10),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.justification = "left",
        plot.title = element_text(face = "bold", vjust = 3, size = 14),
        plot.subtitle = element_text(size = 8, face = "italic", vjust = 7),
        plot.caption = element_text(hjust = 0, size = 8, vjust = -3),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))


# Figure - reporting over time, hcw ---------------------------------------

combo_hcw <- sum_hcw_cps %>%
  ggplot(aes(x = DATE, y = count_month, 
             fill = factor(type_cumul, levels = c("Complete primary series", "Boosters")))) +
  geom_col() +
  geom_line(aes(y = count_cumul, 
                color = factor(type_cumul, levels = c("Complete primary series", "Boosters"))), 
            size = 1.25) +
  scale_color_manual(values = c("#D86422", "#000066"), 
                     labels = c("Complete primary series only", "Complete primary series & boosters")) +
  scale_fill_manual(values = c("#A2D2F0", "#1E7FB8"), 
                    labels = c("Complete primary series only", "Complete primary series & boosters")) +
  scale_x_date(date_labels = "%b %Y") +
  coord_cartesian(ylim = c(0, 200)) +
  geom_hline(aes(yintercept = 194), linetype = "dashed", color = "black") +
  annotate("text", x = as.Date("2023-04-01"), y = 194,
           label = "194 (Total number of WHO Member States)",
           color = "black", hjust = 1, vjust = -1, size = 3, fontface = "italic") +
  theme_test() + 
  labs(title = "Healthcare workers",
       y = "Count of reporting Member States ",
       caption = "Data source: eJRF and WHO regional office reporting systems") + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(vjust = 4, size = 11),
        axis.text.y = element_text(size = 10),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.justification = "left",
        plot.title = element_text(face = "bold", vjust = 3, size = 14),
        plot.subtitle = element_text(size = 8, face = "italic", vjust = 7),
        plot.caption = element_text(hjust = 0, size = 8, vjust = -3),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))


# Figure - daily vaccination rate over time, income/region-----------------

fig_dvr_time <- dvr %>%
  ggplot(aes(x = adm_date, y = dvr_per, 
             color = type)) +
  geom_line(size = 1.5) +
  geom_point(data = dvr %>% 
               group_by(type, group) %>% 
               filter(dvr_per == max(dvr_per)), 
             aes(x = adm_date, y = dvr_per, color = type), 
             size = 3, shape = 21, fill = "black", stroke = 0) +
  # scale_color_manual(values = c("#FFBB30", "#5200AE", "#C8D65B", "#00AE8F")) +
  # scale_x_date(date_labels = "%b %Y") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_test() + 
  labs(title = "Coverage in health and care workers, global",
       y = "Percentage of health and care workers",
       caption = "Data source: WHO-UNICEF electronic Joint Reporting Form COVID-19 module",
       color = "Legend") + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(vjust = 4, size = 11),
        axis.text.y = element_text(size = 10),
        legend.position = "bottom",
        legend.justification = "left",
        plot.title = element_text(face = "bold", vjust = 3, size = 14),
        plot.subtitle = element_text(size = 8, face = "italic", vjust = 7),
        plot.caption = element_text(hjust = 0, size = 8, vjust = -3),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) + 
  facet_wrap(~group, ncol = 1) + 
  guides(color = guide_legend(ncol = 6, override.aes = list(size = 3)))

fig_dvr_time

ggsave(
  path = path_save,
  filename = "fig_dvr_time_group.jpg",
  plot = fig_dvr_time,
  width = 10,
  height = 6,
  dpi = 300
)


# Figure - spread daily vaccination rate, 75/25-----------------------------

test<- data_time_mmm %>%
  select(-max_rate, -min_rate, -perc_10, -perc_90) %>%
  filter(a_income_group != "Other") %>%
  mutate(a_income_group = factor(a_income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))) %>%
  ggplot(aes(x = adm_date, y = avg_rate, color = a_income_group)) +
  geom_ribbon(aes(ymin = perc_25, ymax = perc_75, fill = a_income_group), alpha = 0.2) +  # Shaded 10th-90th percentile
  geom_line(size = 1.2) +  # Line for the average rate
  scale_y_continuous(labels = scales::percent_format()) +  # Format y-axis as percentage
  theme_test() +
  labs(title = "4-Week Vaccination Rate Trend by Income Group",
       y = "Vaccination Rate",
       x = "Date",
       fill = "Income Group",
       color = "Income Group",
       caption = "Shaded area represents the 10th-90th percentile range\nData source: WHO-UNICEF electronic Joint Reporting Form COVID-19 module") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(vjust = 4, size = 11),
        axis.text.y = element_text(size = 10),
        legend.position = "bottom",
        legend.justification = "left",
        plot.title = element_text(face = "bold", vjust = 3, size = 14),
        plot.subtitle = element_text(size = 8, face = "italic", vjust = 7),
        plot.caption = element_text(hjust = 0, size = 8, vjust = -3),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) +
  facet_wrap(~a_income_group, scales = "free_y")  # Separate panels for each income group

test


# Figure - spread daily vaccination rate, sd ------------------------------

test_sd<- data_time_mmm %>%
  filter(a_income_group != "Other") %>%
  mutate(a_income_group = factor(a_income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))) %>%
  ggplot(aes(x = adm_date, y = avg_rate, color = a_income_group)) +
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound, fill = a_income_group), alpha = 0.2) +  # Shaded 10th-90th percentile
  geom_line(size = 1.2) +  # Line for the average rate
  scale_y_continuous(labels = scales::percent_format()) +  # Format y-axis as percentage
  theme_test() +
  labs(title = "Daily vaccination rate (4-week rolling average) trend by income group",
       y = "Daily vaccination rate, as percent of total population",
       x = "Date",
       fill = "Income group",
       color = "Income group",
       caption = "Shaded area represents ±1 Standard Deviation\nData source: WHO-UNICEF electronic Joint Reporting Form COVID-19 module") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(vjust = 4, size = 11),
        axis.text.y = element_text(size = 10),
        legend.position = "bottom",
        legend.justification = "left",
        plot.title = element_text(face = "bold", vjust = 3, size = 14),
        plot.subtitle = element_text(size = 8, face = "italic", vjust = 7),
        plot.caption = element_text(hjust = 0, size = 8, vjust = -3),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) +
  facet_wrap(~a_income_group, scales = "free_y")  # Separate panels for each income group

test_sd


# Figure - spread daily vaccination rate, 75/25-----------------------------

test<- data_time_mmm %>%
  select(-max_rate, -min_rate, -perc_10, -perc_90) %>%
  filter(a_income_group != "Other") %>%
  mutate(a_income_group = factor(a_income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))) %>%
  ggplot(aes(x = adm_date, y = avg_rate, color = a_income_group)) +
  geom_ribbon(aes(ymin = perc_25, ymax = perc_75, fill = a_income_group), alpha = 0.2) +  # Shaded 10th-90th percentile
  geom_line(size = 1.2) +  # Line for the average rate
  scale_y_continuous(labels = scales::percent_format()) +  # Format y-axis as percentage
  theme_test() +
  labs(title = "Daily vaccination rate (4-week rolling average) trend by income group",
       y = "Daily vaccination rate, as percent of total population",
       x = "Date",
       fill = "Income group",
       color = "Income group",
       caption = "Shaded area represents the 25th-75th percentile range\nData source: WHO-UNICEF electronic Joint Reporting Form COVID-19 module") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(vjust = 4, size = 11),
        axis.text.y = element_text(size = 10),
        legend.position = "bottom",
        legend.justification = "left",
        plot.title = element_text(face = "bold", vjust = 3, size = 14),
        plot.subtitle = element_text(size = 8, face = "italic", vjust = 7),
        plot.caption = element_text(hjust = 0, size = 8, vjust = -3),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) +
  facet_wrap(~a_income_group, scales = "free_y")  # Separate panels for each income group

test

ggsave(
  path = path_save,
  filename = "fig_dvr_time_inc.jpg",
  plot = test,
  width = 10,
  height = 6,
  dpi = 300
)

# Figure - coverage over time, all ----------------------------------------

dat_cov_time <- raw_data_time %>%
  select(a_iso,
         month_name,
         a_status_who,
         a_income_group,
         adm_tot_td,
         adm_tot_a1d,
         adm_tot_cps,
         adm_tot_boost,
         adm_a1d_old_cap,
         adm_fv_old_cap,
         adm_booster_old_cap,
         adm_hcw_a1d,
         adm_hcw_fv,
         adm_booster_hcw_cap) %>%
  filter(a_status_who == "Member State") %>%
  select(-a_status_who) %>%
  group_by(month_name) %>%
  summarize(adm_td_tot = sum(adm_tot_td, na.rm = TRUE),
            adm_a1d_tot = sum(adm_tot_a1d, na.rm = TRUE),
            adm_fv_tot = sum(adm_tot_cps, na.rm = TRUE),
            adm_booster_tot = sum(adm_tot_boost, na.rm = TRUE),
            adm_a1d_old = sum(adm_a1d_old_cap, na.rm = TRUE),
            adm_fv_old = sum(adm_fv_old_cap, na.rm = TRUE),
            adm_booster_old = sum(adm_booster_old_cap, na.rm = TRUE),
            adm_a1d_hcw = sum(adm_hcw_a1d, na.rm = TRUE),
            adm_fv_hcw = sum(adm_hcw_fv, na.rm = TRUE),
            adm_booster_hcw = sum(adm_booster_hcw_cap, na.rm = TRUE)) %>%
  gather(key = "measure", value = "admin", -month_name) %>%
  mutate(group = if_else(str_detect(measure, "tot"), "Total population", 
                         if_else(str_detect(measure, "old"), "Older adults",
                                 if_else(str_detect(measure, "hcw"), "Health and care workers", NA))),
         measure = if_else(str_detect(measure, "a1d"), "At least one dose", 
                           if_else(str_detect(measure, "fv"), "Complete primary series", 
                                   if_else(str_detect(measure, "boost"), "At least one booster", 
                                           if_else(str_detect(measure, "td"), "Total doses",NA))))) %>%
  mutate(coverage = if_else(group == "Total population", admin / pop_world_total,
                            if_else(group == "Older adults", admin / pop_world_old,
                                    if_else(group == "Health and care workers", admin / pop_world_hcw, NA))))

fig_cov_time_world <- dat_cov_time %>%
  filter(measure != "Total doses") %>%
  mutate(measure = factor(measure, levels = c("At least one dose",
                                              "Complete primary series",
                                              "At least one booster")),
         group = factor(group, levels = c("Total population", 
                                          "Older adults",
                                          "Health and care workers"))) %>%
  ggplot(aes(x = month_name, y = coverage, 
             color = measure)) +
  geom_line(size = 1.5) +
  scale_color_manual(values = c("#FFBB30", "#5200AE", "#C8D65B")) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  theme_test() + 
  labs(title = "COVID-19 vaccination coverage, per group",
       y = "Percentage of population",
       caption = "Data source: WHO-UNICEF electronic Joint Reporting Form COVID-19 module",
       color = "Legend") + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(vjust = 4, size = 11),
        axis.text.y = element_text(size = 10),
        legend.position = "bottom",
        legend.justification = "left",
        plot.title = element_text(face = "bold", vjust = 3, size = 14),
        plot.subtitle = element_text(size = 8, face = "italic", vjust = 7),
        plot.caption = element_text(hjust = 0, size = 8, vjust = -3),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) +
  facet_wrap(~ group, ncol = 3)

fig_cov_time_world

ggsave(
  path = path_save,
  filename = "fig_cov_time_world.jpg",
  plot = fig_cov_time_world,
  width = 10,
  height = 6,
  dpi = 300
)


# Figure - coverage over time, total --------------------------------------

fig_cov_time_total_world <- dat_cov_time %>%
  filter(group == "Total population",
         measure != "Total doses") %>%
  ggplot(aes(x = month_name, y = coverage, 
             color = measure)) +
  geom_line(size = 1.5) +
  scale_color_manual(values = c("#FFBB30", "#5200AE", "#C8D65B")) +
  # scale_x_date(date_labels = "%b %Y") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_test() + 
  labs(title = "Coverage in total population, global",
       y = "Percentage of total population",
       caption = "Data source: WHO-UNICEF electronic Joint Reporting Form COVID-19 module",
       color = "Legend") + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(vjust = 4, size = 11),
        axis.text.y = element_text(size = 10),
        legend.position = "bottom",
        legend.justification = "left",
        plot.title = element_text(face = "bold", vjust = 3, size = 14),
        plot.subtitle = element_text(size = 8, face = "italic", vjust = 7),
        plot.caption = element_text(hjust = 0, size = 8, vjust = -3),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))

fig_cov_time_total_world


# Figure - coverage over time, old ---------------------------------------------

fig_cov_time_old_world <- dat_cov_time %>%
  filter(group == "Older adults",
         measure != "Total doses") %>%
  ggplot(aes(x = month_name, y = coverage, 
             color = measure)) +
  geom_line(size = 1.5) +
  scale_color_manual(values = c("#FFBB30", "#5200AE", "#C8D65B")) +
  # scale_x_date(date_labels = "%b %Y") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_test() + 
  labs(title = "Coverage in older adults, global",
       y = "Percentage of older adults",
       caption = "Data source: WHO-UNICEF electronic Joint Reporting Form COVID-19 module",
       color = "Legend") + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(vjust = 4, size = 11),
        axis.text.y = element_text(size = 10),
        legend.position = "bottom",
        legend.justification = "left",
        plot.title = element_text(face = "bold", vjust = 3, size = 14),
        plot.subtitle = element_text(size = 8, face = "italic", vjust = 7),
        plot.caption = element_text(hjust = 0, size = 8, vjust = -3),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))

fig_cov_time_old_world


# Figure - coverage over time, hcw ----------------------------------------

fig_cov_time_hcw_world <- dat_cov_time %>%
  filter(group == "Health and care workers",
         measure != "Total doses") %>%
  ggplot(aes(x = month_name, y = coverage, 
             color = measure)) +
  geom_line(size = 1.5) +
  scale_color_manual(values = c("#FFBB30", "#5200AE", "#C8D65B")) +
  # scale_x_date(date_labels = "%b %Y") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_test() + 
  labs(title = "Coverage in health and care workers, global",
       y = "Percentage of health and care workers",
       caption = "Data source: WHO-UNICEF electronic Joint Reporting Form COVID-19 module",
       color = "Legend") + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(vjust = 4, size = 11),
        axis.text.y = element_text(size = 10),
        legend.position = "bottom",
        legend.justification = "left",
        plot.title = element_text(face = "bold", vjust = 3, size = 14),
        plot.subtitle = element_text(size = 8, face = "italic", vjust = 7),
        plot.caption = element_text(hjust = 0, size = 8, vjust = -3),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))

fig_cov_time_hcw_world


# Figure - uptake over time per pop, by income group and region-------------

dat_cov_time_income <- raw_data_time %>%
  select(a_iso,
         month_name,
         a_status_who,
         a_income_group,
         adm_tot_td) %>%
  filter(a_status_who == "Member State",
         a_income_group != "Other") %>%
  select(-a_status_who) %>%
  group_by(month_name, a_income_group) %>%
  summarize(adm_td_tot = sum(adm_tot_td, na.rm = TRUE)) %>%
  mutate(coverage = if_else(a_income_group == "HIC", adm_td_tot / pop_total_hic * 100,
                            if_else(a_income_group == "UMIC", adm_td_tot / pop_total_umic * 100,
                                    if_else(a_income_group == "LMIC", adm_td_tot / pop_total_lmic * 100, 
                                            if_else(a_income_group == "LIC", adm_td_tot / pop_total_lic * 100, NA)))),
         category = "Income group") %>%
  rename(dimension = a_income_group) %>%
  ungroup()

dat_cov_time_covax <- raw_data_time %>%
  select(a_iso,
         month_name,
         a_status_covax,
         adm_tot_td) %>%
  filter(is.na(a_status_covax) == FALSE) %>%
  group_by(month_name, a_status_covax) %>%
  summarize(adm_td_tot = sum(adm_tot_td, na.rm = TRUE)) %>%
  mutate(coverage = if_else(a_status_covax == "AMC", adm_td_tot / pop_total_amc * 100,
                            if_else(a_status_covax == "Self-financing", adm_td_tot / pop_total_sfp * 100,
                                    if_else(a_status_covax == "No", adm_td_tot / pop_total_noncovax * 100, NA))),
         category = "COVAX") %>%
  rename(dimension = a_status_covax) %>%
  ungroup()

dat_cov_time_group <- raw_data_time %>%
  select(a_iso,
         month_name,
         a_status_who,
         a_region_who,
         adm_tot_td) %>%
  filter(a_status_who == "Member State") %>%
  select(-a_status_who) %>%
  group_by(month_name, a_region_who) %>%
  summarize(adm_td_tot = sum(adm_tot_td, na.rm = TRUE)) %>%
  mutate(coverage = if_else(a_region_who == "AFR", adm_td_tot / pop_total_afr * 100,
                            if_else(a_region_who == "AMR", adm_td_tot / pop_total_amr * 100,
                                    if_else(a_region_who == "EMR", adm_td_tot / pop_total_emr * 100, 
                                            if_else(a_region_who == "EUR", adm_td_tot / pop_total_eur * 100, 
                                                    if_else(a_region_who == "SEAR", adm_td_tot / pop_total_sear * 100,
                                                            if_else(a_region_who == "WPR", adm_td_tot / pop_total_wpr * 100, NA)))))),
         category = "Region") %>%
  rename(dimension = a_region_who) %>%
  ungroup() %>%
  rbind(., dat_cov_time_income, dat_cov_time_covax)

fig_cov_time_total_group <- dat_cov_time_group %>%
  mutate(category = factor(category, levels = c("Income group",
                                              "Region",
                                              "COVAX"))) %>%
  ggplot(aes(x = month_name, y = coverage, 
             color = dimension)) +
  geom_line(size = 1.5) +
  geom_point(size = 2.5) +
  # scale_color_manual(values = c("#FFBB30", "#5200AE", "#C8D65B")) +
  # scale_x_date(date_labels = "%b %Y") +
  # scale_y_continuous(labels = scales::percent_format()) +
  theme_test() + 
  labs(title = "Uptake in total population, global, by income group, by region",
       y = "Total doses administered per 100 population",
       caption = "Data source: WHO-UNICEF electronic Joint Reporting Form COVID-19 module",
       color = "Legend") + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(vjust = 4, size = 11),
        axis.text.y = element_text(size = 10),
        legend.position = "bottom",
        legend.justification = "left",
        plot.title = element_text(face = "bold", vjust = 3, size = 14),
        plot.subtitle = element_text(size = 8, face = "italic", vjust = 7),
        plot.caption = element_text(hjust = 0, size = 8, vjust = -3),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) +
  facet_wrap(~ category, ncol = 3)

fig_cov_time_total_group

ggsave(
  path = path_save,
  filename = "fig_upt_time_total_reg_inc.jpg",
  plot = fig_cov_time_total_group,
  width = 10,
  height = 6,
  dpi = 300
)

# Figure - final coverage, violins, income group ---------------------------

dat_cov_end_inc <- raw_data_end %>%
  select(a_iso,
         a_status_who,
         a_income_group,
         cov_total_a1d,
         cov_total_fv,
         cov_total_booster,
         cov_60p_a1d_adjust,
         cov_60p_fv,
         cov_60p_booster,
         cov_hcw_a1d_adjust,
         cov_hcw_fv,
         cov_hcw_booster) %>%
  filter(a_status_who == "Member State",
         a_income_group != "Other") %>%
  select(-a_status_who) %>%
  gather(key = "measure", value = "coverage", -a_iso, -a_income_group) %>%
  mutate(group = if_else(str_detect(measure, "cov_total"), "Total population", 
                         if_else(str_detect(measure, "cov_60p"), "Older adults",
                                 if_else(str_detect(measure, "cov_hcw"), "Health and care workers", NA))),
         measure = if_else(str_detect(measure, "a1d"), "At least one dose", 
                           if_else(str_detect(measure, "fv"), "Complete primary series", 
                                   if_else(str_detect(measure, "booster"), "At least one booster", NA))))

# Faceted violins
fig_cov_end_inc <- dat_cov_end_inc %>%
  mutate(a_income_group = factor(a_income_group, 
                                 levels = c("LIC", 
                                            "LMIC", 
                                            "UMIC", 
                                            "HIC")),
         measure = factor(measure, levels = c("At least one dose", 
                                              "Complete primary series", 
                                              "At least one booster")),
         group = factor(group, levels = c("Total population", 
                                          "Older adults", 
                                          "Health and care workers"))) %>%
  ggplot(aes(x = a_income_group, 
             y = coverage, 
             fill = a_income_group)) + 
  geom_violin(trim = TRUE) + 
  geom_boxplot(width = 0.1, color = "black") +
  # geom_jitter(width = 0.05, size = 3, alpha = 0.7) +
  facet_grid(group ~ measure, scales = "free", space = "free") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_fill_manual(values = c("#FFBB30", "#0A71D5", "#C8D65B", "#00AE8F")) +
  theme_bw() +
  labs(title = "COVID-19 vaccination coverage, by measure, by group, by income group",
       y = "Percentage of population") + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(vjust = 4, size = 12),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(face = "bold", vjust = 3, size = 14),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.justification = "left") +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))

fig_cov_end_inc

ggsave(
  path = path_save,
  filename = "fig_cov_end_inc.jpg",
  plot = fig_cov_end_inc,
  width = 10,
  height = 8,
  dpi = 300
)





# Recycling ---------------------------------------------------------------

# Grouped violins
fig_cov_end_inc <- dat_cov_end_inc %>%
  mutate(a_income_group = factor(a_income_group, 
                                 levels = c("LIC", 
                                            "LMIC", 
                                            "UMIC", 
                                            "HIC")),
         measure = factor(measure, levels = c("cov_total_a1d", 
                                              "cov_total_fv", 
                                              "cov_total_booster"))) %>%
  ggplot(aes(x = measure, 
             y = coverage, 
             fill = a_income_group)) + 
  geom_violin(position = position_dodge(0.7), scale = "width", width = 0.7, trim = TRUE) + 
  geom_boxplot(position = position_dodge(0.7), width = 0.1, color = "black") +
  # geom_jitter(position = position_dodge(0.7), width = 0.05, size = 3, alpha = 0.7) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("#FFBB30", "#0A71D5", "#C8D65B", "#00AE8F")) +
  theme_bw() +
  labs(title = "Coverage, by income group",
       y = "Coverage with a complete primary series (% of total pop.)") + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(),
        axis.title.y = element_text(vjust = 4, size = 12),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(face = "bold", vjust = 3, size = 14),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.justification = "left") +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))

fig_cov_end_inc

