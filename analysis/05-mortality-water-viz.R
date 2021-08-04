library(fs)
library(readr)
library(dplyr)
library(lubridate)
library(zoo)
library(tidyr)
library(stringr)
library(ggplot2)
theme_set(theme_bw())
library(biscale)
library(lis)
library(cowplot)

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

#----------------------------------------------------------

data("Peru")

mortality_water_covid_all <- 
  mortality_water %>% 
  filter(year >= 2020) %>% 
  left_join(mortality_water_base, by = c("ubigeo", "week")) %>% 
  group_by(ubigeo) %>% 
  summarise(
    base = sum(base),
    deaths = sum(deaths),
    yes = mean(water_247_yes),
    no = mean(water_247_no), 
    pop = mean(population)
  ) %>% 
  mutate(
    excess = 10000 * (deaths - base) / pop,
    water_prop = 100 * yes / (yes + no)
  )

mortality_water_covid_bi <- 
  Peru %>% 
  right_join(mortality_water_covid_all, by = "ubigeo")

bidata <- 
  bi_class(
    mortality_water_covid_bi, 
    x = water_prop, 
    y = excess, 
    style = "quantile", 
    dim = 3
  )

plt_bimap <- 
  ggplot() +
  geom_sf(bidata, mapping = aes(fill = bi_class), color = "white", size = 0.1, 
          show.legend = FALSE) +
  bi_scale_fill(pal = "DkBlue", dim = 3) +
  bi_theme()

legend <- bi_legend(pal = "DkBlue", dim = 3, xlab = "Higher % Water Supply", 
                    ylab = "Higher Excess Death", size = 8)

plt_bimap_water_excess <- 
  ggdraw() +
  draw_plot(plt_bimap, 0, 0, 1, 1) +
  draw_plot(legend, 0.15, 0.1, 0.2, 0.2)

show(plt_bimap_water_excess)

png("analysis/figs/bimap-water-excess.png", width = 22, height = 32, 
    units = "cm", res = 300)
show(plt_bimap_water_excess)
dev.off()

bimap_reg <- function(region = "LIMA", x.map = 0, y.map = 0, width.map = 1, 
                      height.map = 1, scale.map = 1, x.legend = 0, y.legend = 0, 
                      width.legend = 1, height.legend = 1, scale.legend = 1) {
  dat <- 
    mortality_water_covid_bi %>% 
    filter(reg == region)
  
  bidat <- 
    bi_class(
      dat, 
      x = water_prop, 
      y = excess, 
      style = "quantile", 
      dim = 3
    )
  
  bimap <- 
    ggplot() +
    geom_sf(bidat, mapping = aes(fill = bi_class), color = "white", size = 0.1, 
            show.legend = FALSE) +
    bi_scale_fill(pal = "DkBlue", dim = 3) +
    bi_theme()
  
  legend <- bi_legend(pal = "DkBlue", dim = 3, xlab = "Higher % Water Supply ", 
                      ylab = "Higher Excess Death ", size = 8)
  
  full_bimap <- 
    ggdraw() +
    draw_plot(bimap, x.map, y.map, width.map, height.map, scale.map) +
    draw_plot(legend, x.legend, y.legend, width.legend, height.legend, 
              scale.legend)
  
  full_bimap
}

bimap_lima <- bimap_reg(x.legend = 0.15, y.legend = 0.1, width.legend = 0.2, 
                        height.legend = 0.2)

png("analysis/figs/bimap-lima.png", width = 22, height = 32, units = "cm", 
    res = 300)
show(bimap_lima)
dev.off()

