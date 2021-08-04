library(fs)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(lis)
library(ggplot2)
theme_set(theme_bw())
library(biscale)
library(cowplot)

inpath <- path("data", "processed")

physicians_raw <- 
  read_csv(
    path(inpath, "physicians-202106-20210802.csv"), 
    locale = locale(encoding = "utf-8")
  )

excess_deaths_raw <- 
  read_csv(
    path(inpath, "monthly-excess-deaths-20210719.csv"), 
    locale = locale(encoding = "utf-8")
  )

physicians <- 
  physicians_raw %>% 
  group_by(ubigeo) %>% 
  summarise(physicians = mean(physicians10k, na.rm = TRUE), .groups = "drop")

excess_deaths <- 
  excess_deaths_raw %>% 
  filter(year == 2021, month == "07") %>% 
  group_by(ubigeo) %>% 
  summarise(excess = mean(excess, na.rm = TRUE), .groups = "drop")

dat <- 
  excess_deaths %>% 
  left_join(physicians, by = "ubigeo") 

data("Peru")

dat_shp <- 
  Peru %>% 
  left_join(dat, by = "ubigeo")

dat_biscale <- 
  dat_shp %>% 
  mutate(
    physicians_class = 
      cut(physicians, breaks = c(0, 1, 15, max(physicians)), 
          include.lowest = TRUE, right = FALSE, labels = c(3, 2, 1)),
    excess_class = 
      cut(excess, breaks = quantile(excess, c(0, 0.33, 0.66, 1)), 
          include.lowest = TRUE, right = FALSE, labels = FALSE)
  ) %>% 
  mutate(
    bi_class = paste0(physicians_class, "-", excess_class)
  )

table(dat_biscale$bi_class)

# bidata <- 
#   bi_class(
#     dat_bi, 
#     x = physicians, 
#     y = excess, 
#     style = "equal", 
#     dim = 3
#   )

plt_bimap <- 
  ggplot() +
  geom_sf(dat_biscale, mapping = aes(fill = bi_class), color = "white", 
          size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "DkBlue", dim = 3) +
  bi_theme()

legend <- bi_legend(pal = "DkBlue", dim = 3, xlab = "More Physicians per 10k ", 
                    ylab = "Higher Excess Death ", size = 8)

plt_bimap_excess <- 
  ggdraw() +
  draw_plot(plt_bimap, 0, 0, 1, 1) +
  draw_plot(legend, 0.15, 0.1, 0.2, 0.2)

gc()
show(plt_bimap_excess)

bimap_reg <- function(region = "LIMA", x.map = 0, y.map = 0, width.map = 1, 
                      height.map = 1, scale.map = 1, x.legend = 0, y.legend = 0, 
                      width.legend = 1, height.legend = 1, scale.legend = 1) {
  dat <- 
    dat_biscale %>% 
    filter(reg == region)
  
  bimap <- 
    ggplot() +
    geom_sf(dat, mapping = aes(fill = bi_class), color = "black", size = 0.1, 
            show.legend = FALSE) +
    bi_scale_fill(pal = "DkBlue", dim = 3) +
    bi_theme()
  
  legend <- bi_legend(pal = "DkBlue", dim = 3, xlab = "Less Physicians per 10k ", 
                      ylab = "Higher Excess Death ", size = 14)
  
  full_bimap <- 
    ggdraw() +
    draw_plot(bimap, x.map, y.map, width.map, height.map, scale.map) +
    draw_plot(legend, x.legend, y.legend, width.legend, height.legend, 
              scale.legend)
  
  full_bimap
}

bimap_lima <- bimap_reg(x.legend = 0.15, y.legend = 0.1, width.legend = 0.2, 
                        height.legend = 0.2, scale.legend = 1.7)

png("analysis/figs/excess-physicians-lima.png", width = 22, height = 32, 
    units = "cm", res = 300)
show(bimap_lima)
dev.off()

