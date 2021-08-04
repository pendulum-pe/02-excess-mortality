library(fs)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(stringi)
library(lubridate)

inpath <- path("data", "raw")
report_date <- "20210802"
infile_name <- paste0("renipress-2021-", report_date, ".csv")
infile <- path(inpath, infile_name)
dat_raw <- read_csv(infile)

dat_raw[dat_raw == 0] <- NA

dat <- dat_raw[, -c(28:33)]

dat <- 
  dat %>% 
  mutate(
    COD_IPRESS = str_pad(COD_IPRESS, 8, "left", "0"),
    UBIGEO = str_pad(UBIGEO, 6, "left", "0"),
    CO_DISA = str_pad(CO_DISA, 2, "left", "0"),
    COD_RED = str_pad(COD_RED, 3, "left", "0"),
    COD_MICRORRED = str_pad(COD_MICRORRED, 4, "left", "0"),
    COD_UE = str_pad(COD_UE, 4, "left", "0")
  )

names(dat) <- str_to_lower(names(dat))
names(dat)[names(dat) == "co_disa"] <- "cod_disa"

outpath <- path("data", "interim")
outfile_name <- paste0("renipress-2021-", report_date, ".csv")
outfile <- path(outpath, outfile_name)
write.csv(dat, outfile, row.names = FALSE, fileEncoding = "utf-8")
