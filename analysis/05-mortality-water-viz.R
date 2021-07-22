library(fs)
library(readr)
library(dplyr)
library(lubridate)
library(zoo)
library(tidyr)
library(stringr)
library(ggplot2)
theme_set(theme_bw())

inpath <- path("data", "processed")
infile <- path(inpath, "mortality-water.csv")
dat_raw <- read_csv(infile)

dates <- seq(ymd("2017-01-01"), ymd("2021-07-18"), by = "day")
years <- year(dates)
weeks <- week(dates)

year_week <- 
  unique(tibble(year = years, week = weeks)) %>%  # 241
  slice(rep(1:n(), each = 1874))

ubigeo <- 
  unique(dat_raw$ubigeo) %>% 
  rep(241)

times <- tibble(ubigeo, year_week)

water <- unique(select(dat_raw, ubigeo, population, water_247_yes, water_247_no))
mortality <- select(dat_raw, ubigeo, dated, year, week)

rm(dat_raw, dates, years, weeks, year_week, ubigeo)
gc()

mortality_sum <-
  mortality %>%
  group_by(ubigeo, year, week) %>%
  summarise(deaths = n(), .groups = "drop") 

mortality_water <- 
  times %>% 
  left_join(water, by = c("ubigeo")) %>% 
  left_join(mortality_sum, by = c("ubigeo", "year", "week")) %>% 
  filter(!(is.na(water_247_yes) & is.na(water_247_no))) %>% 
  mutate_all(~ replace(., is.na(.), 0)) %>% 
  arrange(ubigeo, year, week)

mortality_water_base <- 
  mortality_water %>% 
  filter(year < 2020) %>% 
  group_by(ubigeo, week) %>% 
  summarise(base = mean(deaths), .groups = "drop")

mortality_water_covid <- 
  mortality_water %>% 
  filter(year >= 2020) %>% 
  left_join(mortality_water_base, by = c("ubigeo", "week")) %>% 
  mutate(
    excess = 10000 * (deaths - base) / population,
    water_prop = 100 * water_247_yes / (water_247_yes + water_247_no)
  ) %>% 
  mutate(
    water_cat = cut(water_prop, breaks = seq(0, 100, 20), 
                    include.lowest = TRUE)
  )

mortality_water_covid_cat <- 
  mortality_water_covid %>% 
  group_by(year, week, water_cat) %>% 
  summarise(excess_mean = mean(excess), .groups = "drop") %>% 
  mutate(week_str = str_pad(as.character(week), 2, "left", "0")) %>% 
  mutate(year_week = paste(year, week_str, sep = "-")) %>% 
  mutate(
    year_week = factor(year_week, ordered = TRUE),
    water_cat = factor(water_cat, ordered = TRUE)
  ) %>% 
  select(-week_str)

plt_line <- 
  mortality_water_covid_cat %>% 
  ggplot(
    aes(
      x = as.numeric(year_week), 
      y = excess_mean, 
      group = water_cat, 
      color = water_cat
    )
  ) +
  geom_line(aes(x = as.numeric(year_week), y = excess_mean), lwd = 0.8) +
  geom_hline(yintercept = 0, lty = "dashed")

show(plt_line)

plt_smooth <- 
  mortality_water_covid_cat %>% 
  ggplot(
    aes(
      x = as.numeric(year_week), 
      y = excess_mean, 
      group = water_cat, 
      color = water_cat
    )
  ) +
  geom_smooth(
    aes(
      x = as.numeric(year_week), 
      y = excess_mean, 
      fill = water_cat
    ), 
    alpha = 0.05
  ) +
  geom_hline(yintercept = 0, lty = "dashed")

show(plt_smooth)  
