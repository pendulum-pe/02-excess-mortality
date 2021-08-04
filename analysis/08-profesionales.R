source("R/fs-helpers.R")
library(fs)
library(readr)
library(purrr)
library(dplyr)
library(stringr)

inpath <- path("data", "raw", "profesionales")
infiles <- get_files(inpath, "csv")
dats <- map(infiles, read_csv)
dat <- bind_rows(dats)

names(dat) <- str_to_lower(names(dat))
names(dat)[names(dat) == "co_ipress"] <- "cod_ipress"

dat <- filter(dat, !grepl("^[a-zA-Z]", total_profesionales))
dat <- mutate(dat, total_profesionales = as.numeric(total_profesionales))

outpath <- path("data", "interim")
report_date <- "20210802"
outfile_name <- paste0("profesionales-", report_date, ".csv")
outfile <- path(outpath, outfile_name)
write.csv(dat, outfile, row.names = FALSE, fileEncoding = "utf-8")
