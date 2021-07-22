library(fs)
library(readxl)
library(zoo)
library(dplyr)
library(tidyr)
library(stringi)

inpath <- path("data", "raw")
infile <- path(inpath, "water-24h-7d-2017-20210719.xlsx")
water_raw <- read_excel(infile, col_names = FALSE)

water_raw <- water_raw[, -1]
last <- which(water_raw$...2 == "RESUMEN") - 1
water_raw <- water_raw[1:last, 1:2]

# Filter empty rows
water <- water_raw[rowSums(is.na(water_raw)) != ncol(water_raw), ]

# Create destiny location column
water$nchar <- nchar(water$...3)
water$location <- ifelse(water$nchar >= 10, as.character(water$...3), NA)
water$location <- na.locf(water$location)
water$nchar <- NULL

# Create destiny location code column
water$ubigeo <- 
  ifelse(
    grepl("AREA # ", water$...2) == TRUE, 
    str_remove_all(water$...2, "AREA # "),
    NA
  )
water$ubigeo <- na.locf(water$ubigeo)

# Filter unnecessary rows
water <- 
  water[!(water$...2 == "Total" | water$...2 == "No Aplica :" | 
            grepl("especificado", water$...2) == TRUE |
            grepl("Continente", water$...2) == TRUE), ]

del <- which(grepl("AREA # ", water$...2) == TRUE)
del <- sort(c(del, del + 1))

water <- water[-del, ]
water <- water[!(grepl("[0-9]", water$...2) == TRUE), ]

colnames(water) <- c("water_247", "cases", "location", "ubigeo")

# Format string columns

water <- 
  water %>% 
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
  mutate(
    water_247 = 
      ifelse(
        water_247 == "Sí tiene servicio de agua todos los días de la semana",
        "SI", "NO"
      )
  ) %>% 
  mutate(cases = as.numeric(cases))

water <- 
  water %>% 
  select(ubigeo, region, province, district, water_247, cases)

outpath <- path("data", "interim")
outfile <- path(outpath ,"water-24h-7d-2017-20210719.csv")
write.csv(water, outfile, row.names = FALSE)
