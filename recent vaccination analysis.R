# Individuals recently vaccinated for COVID-19
# Date: 22 January 2024


# Setup -------------------------------------------------------------------

# Set working directory
setwd("C:/Users/brooksd/OneDrive - World Health Organization/Documents/GitHub/covid19_vaccination_analysis")

# Clear environment
rm(list = ls())
gc()

# Load packages
library("tidyverse")
library("readxl")
library("writexl")
library("lubridate")
library("countrycode")


# Load --------------------------------------------------------------------

base <- data.frame(
  read_excel("data/output/output_master.xlsx",
             sheet = "1_stock")
)

latest <- base %>%
  filter(month_name == as.Date("2023-11-01")) %>%
  select(iso,
         a_pop,
         a_region_who,
         a_status_who,
         a_income_group,
         adm_tot_td,
         adm_tot_cps,
         adm_tot_a1d,
         adm_tot_boost)

six_month <- base %>%
  filter(month_name == as.Date("2023-05-01")) %>%
  select(iso,
         adm_tot_td,
         adm_tot_cps,
         adm_tot_a1d,
         adm_tot_boost) %>%
  rename(adm_tot_td_6m = adm_tot_td,
         adm_tot_cps_6m = adm_tot_cps,
         adm_tot_a1d_6m = adm_tot_a1d,
         adm_tot_boost_6m = adm_tot_boost)

merge <- latest %>%
  left_join(., six_month, by = c("iso")) %>%
  mutate(adm_tot_td_rec = round(adm_tot_td - adm_tot_td_6m),
         adm_tot_cps_rec = round(adm_tot_cps - adm_tot_cps_6m),
         adm_tot_a1d_rec = round(adm_tot_a1d - adm_tot_a1d_6m),
         adm_tot_boost_rec = round(adm_tot_boost - adm_tot_boost_6m)) %>%
  filter(a_status_who == "Member State")

merge <- merge %>%
  mutate(adm_tot_first_rec = rowSums(merge[, c("adm_tot_cps_rec", 
                                  "adm_tot_a1d_rec", 
                                  "adm_tot_boost_rec")], na.rm = TRUE),
         adm_tot_boost_other_rec = pmax(adm_tot_td_rec - adm_tot_first_rec, 0)) %>%
  select(iso,
         a_pop,
         a_region_who,
         a_income_group,
         adm_tot_td_rec,
         adm_tot_cps_rec,
         adm_tot_a1d_rec,
         adm_tot_boost_rec,
         adm_tot_first_rec,
         adm_tot_boost_other_rec) %>%
  mutate(cov_tot_rec = (adm_tot_td_rec / a_pop) * 100)

region <- merge %>%
  group_by(a_region_who) %>%
  summarize(adm_tot_td_rec = sum(adm_tot_td_rec, na.rm = TRUE),
            a_pop = sum(a_pop, na.rm = TRUE)) %>% 
  mutate(cov_tot_rec = (adm_tot_td_rec / a_pop)) %>%
  ungroup()

income <- merge %>%
  group_by(a_income_group) %>%
  summarize(adm_tot_td_rec = sum(adm_tot_td_rec, na.rm = TRUE),
            a_pop = sum(a_pop, na.rm = TRUE)) %>% 
  mutate(cov_tot_rec = (adm_tot_td_rec / a_pop)) %>%
  ungroup()

bar_last_cps_perpop_csc_hor <- region %>%
  select(a_region_who,
         cov_tot_rec) %>%
  ggplot(aes(x = reorder(a_region_who, cov_tot_rec), y = cov_tot_rec)) + 
  geom_col(position = position_stack(), color = "white", width = 0.9) +
  geom_text(aes(y = cov_tot_rec + 0.003, label = scales::percent(cov_tot_rec, accuracy = 0.1)), position = position_stack(), size = 4) +
  scale_fill_manual(values = c("#FFBB30", "#5200AE", "#C8D65B", "#00AE8F", 
                               "#0A71D5", "#C12592"),
                    labels = c("Change in coverage since Jan 2021", 
                               "Coverage in Jan 2021")) +
  coord_flip(ylim = c(0, 0.06)) + 
  scale_y_continuous(labels = percent_format()) +
  theme_test() +
  labs(x = "WHO region",
       y = "% of total population vaccinated in the past 6 months",
       caption = "<b>Data source:</b> WHO COVID-19 vaccination data reporting mechanism<br>
       <b>Visual:</b>WHO Immunization Analysis & Insights Unit") + 
  theme(axis.title.x = element_text(vjust = 1, size = 11),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(face = "bold", vjust = 3, size = 14),
        plot.subtitle = element_text(size = 8, face = "italic", vjust = 7),
        plot.caption = element_markdown(hjust = 0, size = 8, vjust = -3),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.justification = "left") +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))

bar_last_cps_perpop_csc_hor

ggsave(
  path = path_save,
  filename = "test_region.jpg",
  plot = bar_last_cps_perpop_csc_hor,
  width = 7,
  height = 6,
  dpi = 300
)

bar_last_cps_perpop_csc_hor_ig <- income %>%
  filter(a_income_group != "Other") %>%
  select(a_income_group,
         cov_tot_rec) %>%
  ggplot(aes(x = reorder(a_income_group, cov_tot_rec), y = cov_tot_rec)) + 
  geom_text(aes(y = cov_tot_rec + 0.003, label = scales::percent(cov_tot_rec, accuracy = 0.1)), position = position_stack(), size = 4) +
  geom_col(position = position_stack(), color = "white", width = 0.9) +
  scale_fill_manual(values = c("#FFBB30", "#5200AE", "#C8D65B", "#00AE8F", 
                               "#0A71D5", "#C12592"),
                    labels = c("Change in coverage since Jan 2021", 
                               "Coverage in Jan 2021")) +
  coord_flip(ylim = c(0, 0.06)) + 
  scale_y_continuous(labels = percent_format()) +
  theme_test() +
  labs(x = "Income group",
       y = "% of total population vaccinated in the past 6 months",
       caption = "<b>Data source:</b> WHO COVID-19 vaccination data reporting mechanism<br>
       <b>Visual:</b>WHO Immunization Analysis & Insights Unit") + 
  theme(axis.title.x = element_text(vjust = 1, size = 11),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(face = "bold", vjust = 3, size = 14),
        plot.subtitle = element_text(size = 8, face = "italic", vjust = 7),
        plot.caption = element_markdown(hjust = 0, size = 8, vjust = -3),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.justification = "left") +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))

bar_last_cps_perpop_csc_hor_ig

ggsave(
  path = path_save,
  filename = "test_ig.jpg",
  plot = bar_last_cps_perpop_csc_hor_ig,
  width = 7,
  height = 6,
  dpi = 300
)

