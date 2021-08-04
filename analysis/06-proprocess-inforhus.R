library(fs)
library(readxl)
library(dplyr)
library(stringr)
library(stringi)
library(lubridate)

inpath <- path("data", "raw")
report_date <- "202001"
infile_name <- paste0("inforhus-", report_date, ".xlsx")
infile <- path(inpath, infile_name)
inforhus_raw <- read_excel(infile, sheet = 1, col_types = "text", skip = 3)
cols_rm <- c(6, 9, 25:31)
inforhus <- inforhus_raw[, -cols_rm]
col_names <- names(inforhus)
col_names <- 
  col_names %>% 
  str_to_lower() %>% 
  str_replace_all(" ", "_") %>% 
  str_replace_all("-", "")
col_names[col_names == "tiempo_de_duración_de_especialidad"] <-
  "duracion_especialidad"
col_names[col_names == "calsificacion"] <- "clasificacion"
names(inforhus) <- col_names

# clasificacion
#  - HOSPITALES O CLINICAS DE ATENCION GENERAL
#  - CENTROS DE SALUD O CENTROS MEDICOS
#  - PUESTOS DE SALUD O POSTAS DE SALUD
#  - CENTROS DE SALUD CON CAMAS DE INTERNAMIENTO
#  - PUESTOS DE SALUD O POSTAS DE SALUD,PUESTOS DE SALUD O POSTAS DE SALUD
#  - CENTROS DE SALUD O CENTROS MEDICOS,CENTROS DE SALUD O CENTROS MEDICOS
#  - CENTROS DE SALUD CON CAMAS DE INTERNAMIENTO,CENTROS DE SALUD CON CAMAS DE INTERNAMIENTO
#  - POLICLINICOS
# 
# tipo
#  - ESTABLECIMIENTO DE SALUD CON INTERNAMIENTO
#  - ESTABLECIMIENTO DE SALUD SIN INTERNAMIENTO
# 
# estado
#  - 1


