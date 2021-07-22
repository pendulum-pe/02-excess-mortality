library(fs)
library(readxl)
library(dplyr)
library(tidyr)
library(stringi)

inpath <- path("data", "raw")
infile <- path(inpath, "population-2017.xlsx")
pop_raw <- read_excel(infile, range = "B6:D1880", col_names = TRUE, 
                      col_types = c("text", "text", "numeric"))
colnames(pop_raw) <- c("ubigeo", "location", "population")

pop <- 
  pop_raw %>% 
  mutate(ubigeo = str_pad(ubigeo, width = 6, side = "left", pad = "0")) %>% 
  separate(location, into = c("region", "province", "district"), sep = ",") %>% 
  mutate(
    region = str_remove_all(region, "Prov. Constitucional del ")
  ) %>% 
  mutate(
    district = ifelse(region == "Callao", province, district),
    province = ifelse(region == "Callao", "Callao", province)
  ) %>% 
  mutate(district = str_remove_all(district, "distrito: ")) %>% 
  mutate_at(
    vars(region, province, district),
    ~ str_trim(toupper(stri_trans_general(. , id = "Latin-ASCII")))
  ) %>% 
  mutate_at(
    vars(region, province, district),
    ~ str_remove_all(., "['~]")
  ) %>% 
  mutate_at(
    vars(region, province, district),
    ~ str_replace(., "[_-]", " ")
  ) %>% 
  mutate(population = as.numeric(population))

outpath <- path("data", "interim")
outfile <- path(outpath, "population-2017.csv")
write.csv(pop, outfile, row.names = FALSE)