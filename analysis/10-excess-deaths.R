library(fs)
library(readr)
library(dplyr)
library(lubridate)
library(stringr)

inpath <- path("data", "interim")

sinadef <- 
  read_csv(
    path(inpath, "sinadef-20210719.csv"), 
    locale = locale(encoding = "utf-8")
  )

pop <- 
  read_csv(
    path(inpath, "population-2017.csv"), 
    locale = locale(encoding = "utf-8")
  )

ubigeo_sinadef <- 
  pop %>%
  select(ubigeo, region, province, district) %>% 
  left_join(sinadef, by = c("region", "province", "district"))

dates <- seq(ymd("2017-01-01"), ymd("2021-07-18"), by = "day")
years <- year(dates)
months <- str_pad(month(dates), 2, "left", "0")
weeks <- week(dates)

dates_df <- 
  unique(tibble(year = years, month = months, week = weeks)) %>%  # 241
  slice(rep(1:n(), each = 1874))

ubigeo <- 
  unique(pop$ubigeo) %>% 
  rep(286)

weekly_report_dates <- tibble(ubigeo, dates_df)

weekly_deaths <- 
  ubigeo_sinadef %>%
  group_by(ubigeo, year, week) %>%
  summarise(deaths = n(), .groups = "drop") 

weekly_base <- 
  weekly_deaths %>% 
  filter(year < 2020) %>% 
  group_by(ubigeo, week) %>% 
  summarise(base = mean(deaths), .groups = "drop")

weekly_excess_deaths <- 
  weekly_report_dates %>% 
  left_join(weekly_deaths, by = c("ubigeo", "year", "week")) %>% 
  arrange(ubigeo, year, week) %>% 
  filter(year >= 2020) %>% 
  left_join(weekly_base, by = c("ubigeo", "week")) %>% 
  left_join(pop, by = "ubigeo") %>% 
  mutate_all(~ replace(., is.na(.), 0)) %>%
  mutate(excess = 10000 * (deaths - base) / population)

outpath <- path("data", "processed")
report_date <- "20210719"
outfile_name <- paste0("weekly-excess-deaths-", report_date, ".csv")
outfile <- path(outpath, outfile_name)
write.csv(weekly_excess_deaths, outfile, row.names = FALSE, 
          fileEncoding = "utf-8")

monthly_report_dates <- 
  weekly_report_dates %>% 
  select(-week) %>% 
  unique()

monthly_deaths <- 
  ubigeo_sinadef %>%
  group_by(ubigeo, year, month) %>%
  summarise(deaths = n(), .groups = "drop") 

monthly_base <- 
  monthly_deaths %>% 
  filter(year < 2020) %>% 
  group_by(ubigeo, month) %>% 
  summarise(base = mean(deaths), .groups = "drop")

monthly_excess_deaths <- 
  monthly_report_dates %>% 
  left_join(monthly_deaths, by = c("ubigeo", "year", "month")) %>% 
  arrange(ubigeo, year, month) %>% 
  filter(year >= 2020) %>% 
  left_join(monthly_base, by = c("ubigeo", "month")) %>% 
  left_join(pop, by = "ubigeo") %>% 
  mutate_all(~ replace(., is.na(.), 0)) %>% 
  mutate(excess = 10000 * (deaths - base) / population)

outfile_name <- paste0("monthly-excess-deaths-", report_date, ".csv")
outfile <- path(outpath, outfile_name)
write.csv(monthly_excess_deaths, outfile, row.names = FALSE, 
          fileEncoding = "utf-8")