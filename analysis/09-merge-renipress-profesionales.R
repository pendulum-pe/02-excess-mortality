library(fs)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

inpath <- path("data", "interim")
profesionales <- read_csv(path(inpath, "profesionales-20210802.csv"), 
                          locale = locale(encoding = "utf-8"))
renipress <- read_csv(path(inpath, "renipress-2021-20210802.csv"), 
                      locale = locale(encoding = "utf-8"))
pop <- 
  read_csv(
    path(inpath, "population-2017.csv"), 
    locale = locale(encoding = "utf-8")
  )

profesionales <- select(profesionales, -nombre)
renipress <- 
  renipress %>% 
  select(institucion, cod_ipress, clasificacion, tipo_establecimiento, ubigeo, 
         estado, situacion, condicion)

dat_raw <- 
  profesionales %>% 
  left_join(renipress, by = c("cod_ipress", "ubigeo"))

dat_prof <- 
  dat_raw %>% 
  filter(
    tipo_establecimiento %in% 
      c(
        "ESTABLECIMIENTO DE SALUD CON INTERNAMIENTO",
        "ESTABLECIMIENTO DE SALUD SIN INTERNAMIENTO"
      )
  )

dat_prof <- 
  dat_prof %>% 
  filter(
    clasificacion %in% 
      c(
        "HOSPITALES O CLINICAS DE ATENCION GENERAL",
        "PUESTOS DE SALUD O POSTAS DE SALUD",
        "CENTROS DE SALUD O CENTROS MEDICOS",
        "HOSPITALES O CLINICAS DE ATENCION ESPECIALIZADA",
        "CENTROS DE SALUD CON CAMAS DE INTERNAMIENTO",
        "POLICLINICOS",
        "HOSPITALES O CLINICAS DE ATENCION ESPECIALIZADA,HOSPITALES O CLINICAS DE ATENCION GENERAL",
        "PUESTOS DE SALUD O POSTAS DE SALUD,PUESTOS DE SALUD O POSTAS DE SALUD",
        "HOSPITALES O CLINICAS DE ATENCION GENERAL,CENTROS DE SALUD CON CAMAS DE INTERNAMIENTO",
        "CENTROS DE SALUD CON CAMAS DE INTERNAMIENTO,HOSPITALES O CLINICAS DE ATENCION ESPECIALIZADA"
      )
  )

dat_prof <- 
  dat_prof %>% 
  filter(condicion == "ACTIVO")

dat_prof <- 
  dat_prof %>% 
  filter(cole_prof %in% c("COLEGIO MEDICO DEL PERU", 
                          "COLEGIO DE ENFERMEROS DEL PERU"))

names(dat_prof) <- c("year", "month", "ubigeo", "region", "province", "district", 
                "sector", "category", "cod_ipress", "cole_prof", "physicians", 
                "institution", "class", "type", "status", "situation", 
                "condition")

dat_prof <- 
  dat_prof %>% 
  select(-c(region, province, district))

dates <- seq(ymd("2020-01-01"), ymd("2021-06-30"), by = "day")
years <- year(dates)
months <- str_pad(month(dates), 2, "left", "0")

dates_df <- 
  unique(tibble(year = years, month = months)) %>%
  slice(rep(1:n(), each = 1874))

ubigeo <- 
  pop %>% 
  slice(rep(row_number(), 18))

report_dates <- tibble(dates_df, ubigeo)

report_ipress <- 
  dat_prof %>% 
  select(year, month, ubigeo, cod_ipress) %>% 
  unique()

report_dates <- 
  report_dates %>% 
  left_join(report_ipress, by = c("year", "month", "ubigeo"))
  
dat <- 
  report_dates %>% 
  left_join(dat_prof, by = c("year", "month", "ubigeo", "cod_ipress"))

dat <- 
  dat %>% 
  mutate(physicians = replace(physicians, is.na(physicians), 0)) 

dat_district <- 
  dat %>% 
  group_by(year, month, ubigeo) %>% 
  summarise(
    physicians = sum(physicians), 
    population = mean(population), 
    .groups = "drop"
  ) %>% 
  mutate(physicians10k = 10000 * (physicians / population))

outpath <- path("data", "processed")
report_date <- "20210802"
outfile_name <- paste0("physicians-202106-", report_date, ".csv")
outfile <- path(outpath, outfile_name)
write.csv(dat_district, outfile, row.names = FALSE, fileEncoding = "utf-8")
