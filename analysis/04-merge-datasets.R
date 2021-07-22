library(fs)
library(readr)
library(dplyr)
library(purrr)
library(tidyr)
source("R/fs-helpers.R")

inpath <- path("data", "interim")
infiles <- get_files(inpath, ext = "csv")
dat <- map(infiles, read_csv)

population <- dat$`population-2017`
sinadef <- dat$`sinadef-20210719`
water <- dat$`water-24h-7d-2017-20210719`

water <- 
  water %>% 
  pivot_wider(
    names_from = water_247,
    values_from = cases
  )
names(water)[names(water) == "SI"] = "water_247_yes"
names(water)[names(water) == "NO"] = "water_247_no"

mortality_water <- 
  population %>% 
  left_join(sinadef, by = c("region", "province", "district")) %>% 
  left_join(water, by = c("ubigeo", "region", "province", "district"))

outpath <- path("data", "processed")
outfile <- path(outpath , "mortality-water.csv")
write.csv(mortality_water, outfile, row.names = FALSE)
