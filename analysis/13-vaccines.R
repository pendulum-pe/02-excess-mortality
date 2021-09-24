library(readr)

inpath <- path("data", "raw")
infile <- path(inpath, "vacunas_covid.csv")
vaccines_raw <- read_csv(infile)

vaccines <- 
  vaccines_raw %>% 
  filter(!(DEPARTAMENTO != "HUANUCO" & EDAD >= 121)) %>% 
  filter(DOSIS == 2) 
