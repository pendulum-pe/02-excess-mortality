library(fs)
library(readr)
library(dplyr)
library(stringr)
library(stringi)
library(lubridate)

inpath <- path("data", "raw")
infile <- path(inpath, "sinadef-20210920.csv")
sinadef_raw <- read_delim(
  infile, delim = "|", locale = locale(encoding = "utf8")
)

sinadef_raw <- sinadef_raw[, -1]
sinadef_raw <- 
  sinadef_raw %>% 
  transmute(
    dated = FECHA, year = `AÑO`, month = MES, country = `PAIS DOMICILIO`, 
    region = `DEPARTAMENTO DOMICILIO`, province = `PROVINCIA DOMICILIO`, 
    district = `DISTRITO DOMICILIO`, ubigeo = `COD# UBIGEO DOMICILIO`,
    age = EDAD, age_scale = `TIEMPO EDAD`, sex = SEXO, 
    marital_status = `ESTADO CIVIL`, education = `NIVEL DE INSTRUCCIÓN`,
    insurance = `TIPO SEGURO`, necropsy = NECROPSIA, violent = `MUERTE VIOLENTA`,
    place = `TIPO LUGAR`, institution = INSTITUCION, cie10_a = `CAUSA A (CIE-X)`,
    cie10_b = `CAUSA B (CIE-X)`, cie10_c = `CAUSA C (CIE-X)`, 
    cie10_d = `CAUSA D (CIE-X)`, cie10_e = `CAUSA E (CIE-X)`, 
    cie10_f = `CAUSA F (CIE-X)` 
  ) 

sinadef <- 
  sinadef_raw %>% 
  mutate(dated = format_ISO8601(dated, precision = "ymd")) %>% 
  mutate(week = week(dated))

sinadef <- 
  sinadef %>% 
  mutate_all(~ na_if(., "SIN REGISTRO")) %>% 
  mutate_all(~ na_if(., "IGNORADO"))

sinadef <- 
  sinadef %>%
  mutate(age = ifelse(grepl("[A-Za-z]", age), NA, age)) %>% 
  mutate(age = as.numeric(age)) %>% 
  mutate(age_years = ifelse((!is.na(age) & age_scale != "AÑOS"), 0, age))

sinadef <- 
  sinadef %>%
  mutate(
    marital_status = 
      recode(
        marital_status,
        !!!c(
          `CASADO` = "CASADO/A",
          `CONVIVIENT/CONCUBINA` = "CONVIVIENTE/CONCUBINATO",
          `DIVORCIADO` = "DIVORCIADO/A",
          `SEPARADO` = "SEPARADO/A",
          `SOLTERO` = "SOLTERO/A",
          `VIUDO` = "VIUDO/A"
        )
      )
  ) 

sinadef <- 
  sinadef %>%
  mutate(
    necropsy = 
      recode(
        necropsy,
        !!!c(
          `NO SE REALIZÓ NECROPSIA` = "NO",
          `SI SE REALIZÓ NECROPSIA` = "SI"
        )
      )
  ) 

sinadef <- 
  sinadef %>% 
  mutate_if(
    is.character, 
    ~ str_trim(toupper(stri_trans_general(. , id = "Latin-ASCII")))
  ) %>% 
  mutate_if(is.character, ~ str_remove_all(., "['~]")) %>% 
  mutate_if(function(col) is.character(col) & !all(col == .$dated), 
            ~ str_replace(., "[_-]", " "))

sinadef <- 
  sinadef %>% 
  mutate(
    district = 
      recode(
        district,
        !!!c(
          `APARICIO POMARES (CHUPAN)` = "APARICIO POMARES",
          `CASTA` = "SAN PEDRO DE CASTA",
          `ANTONIO RAIMONDI` = "ANTONIO RAYMONDI",
          `SANTA RITA DE SIHUAS` = "SANTA RITA DE SIGUAS",
          `SAN FCO DE ASIS DE YARUSYACAN` = "SAN FRANCISCO DE ASIS DE YARUSYACAN",
          `RAIMONDI` = "RAYMONDI",
          `CASPIZAPA` = "CASPISAPA",
          `CAPASO` = "CAPAZO",
          `NAZCA` = "NASCA", 
          `HUALLAY GRANDE` = "HUAYLLAY GRANDE",
          `HUAILLATI` = "HUAYLLATI",
          `ANTONIO RAIMONDI` = "ANTONIO RAYMONDI",
          `MILPUCC` = "MILPUC",
          `SAN FRANCISCO DE YESO` = "SAN FRANCISCO DEL YESO",
          `QUISQUI` = "QUISQUI (KICHKI)",
          `SAN JOSE DE LOS CHORRILLOS` = "CUENCA",
          `MARISCAL GAMARRA` = "GAMARRA"
        )
      )
  )

sinadef <- 
  sinadef %>% 
  filter(province != "ARICA") %>% 
  mutate(
    district = ifelse(
      (district == "LARAOS" & province == "HUAROCHIRI"), "SAN PEDRO DE LARAOS",
      district
    )
  ) %>% 
  mutate(
    province = ifelse(district == "APARICIO POMARES", "YAROWILCA", province)
  ) %>% 
  mutate(
    province = ifelse(
      (district == "BANOS" & province == "DOS DE MAYO"), "LAURICOCHA", province
    )
  ) %>% 
  mutate(
    province = ifelse(
      (district == "BARRANCA" & province == "ALTO AMAZONAS"), 
      "DATEM DEL MARANON", province
    )
  ) %>% 
  mutate(
    province = ifelse(
      (district == "CAHUAC" & province == "DOS DE MAYO"), "YAROWILCA", province
    )
  ) %>% 
  mutate(
    province = ifelse(
      (district == "CAHUAPANAS" & province == "ALTO AMAZONAS"), 
      "DATEM DEL MARANON", province
    )
  ) %>% 
  mutate(
    region = ifelse(
      (district == "CASCAS" & province == "CONTUMAZA"), "LA LIBERTAD", region
    ),
    province = ifelse(
      (district == "CASCAS" & province == "CONTUMAZA"), "GRAN CHIMU", province
    )
  ) %>% 
  mutate(
    province = ifelse(
      (district == "CHAVINILLO" & province == "DOS DE MAYO"), "YAROWILCA", 
      province
    )
  ) %>% 
  mutate(
    region = ifelse(
      (district == "HUACHOCOLPA" & province == "HUANCAYO"), "HUANCAVELICA", region
    ),
    province = ifelse(
      (district == "HUACHOCOLPA" & province == "HUANCAYO"), "TAYACAJA", province
    )
  ) %>% 
  mutate(
    region = ifelse(
      (district == "HUALLANCA" & province == "DOS DE MAYO"), "ANCASH", region
    ),
    province = ifelse(
      (district == "HUALLANCA" & province == "DOS DE MAYO"), "BOLOGNESI", province
    )
  ) %>% 
  mutate(
    province = ifelse(district == "HUANDO", "HUANCAVELICA", province)
  ) %>% 
  mutate(
    province = ifelse(district == "JACAS CHICO", "YAROWILCA", province)
  ) %>%
  mutate(
    province = ifelse(
      (district == "JESUS" & region == "HUANUCO"), "LAURICOCHA", province
    )
  ) %>% 
  mutate(
    province = ifelse(
      (district == "JIVIA" & region == "HUANUCO"), "LAURICOCHA", province
    )
  ) %>% 
  mutate(
    province = ifelse(district == "MANSERICHE", "DATEM DEL MARANON", province)
  ) %>% 
  mutate(
    region = ifelse(district == "MASISEA", "UCAYALI", region)
  ) %>% 
  mutate(
    province = ifelse(district == "MORONA", "DATEM DEL MARANON", province)
  ) %>% 
  mutate(
    province = ifelse(district == "OBAS", "YAROWILCA", province)
  ) %>% 
  mutate(
    province = ifelse(district == "PACHAMARCA", "CHURCAMPA", province)
  ) %>%
  mutate(
    province = ifelse(
      (district == "PAMPAMARCA" & region == "HUANUCO"), "YAROWILCA", province
    )
  ) %>% 
  mutate(
    province = ifelse(district == "PUTUMAYO", "PUTUMAYO", province)
  ) %>%
  mutate(
    province = ifelse(district == "PASTAZA", "DATEM DEL MARANON", province)
  ) %>%
  mutate(
    province = ifelse(district == "RONDOS", "LAURICOCHA", province)
  ) %>%
  mutate(
    province = ifelse(
      (district == "SAN FRANCISCO DE ASIS" & region == "HUANUCO"), "LAURICOCHA", 
      province
    )
  ) %>%
  mutate(
    province = ifelse(district == "SAN MIGUEL DE CAURI", "LAURICOCHA", province)
  ) %>%
  mutate(
    region = ifelse(
      (district == "SAYAPULLO" & province == "CAJABAMBA"), "LA LIBERTAD", region
    ),
    province = ifelse(
      (district == "SAYAPULLO" & province == "CAJABAMBA"), "GRAN CHIMU", province
    )
  ) %>% 
  mutate(
    province = ifelse(district == "TENIENTE MANUEL CLAVERO", "PUTUMAYO", province)
  ) %>%
  mutate(
    province = recode(
      province,
      !!!c(
        `ANTONIO RAIMONDI` = "ANTONIO RAYMONDI",
        `NAZCA` = "NASCA"
      )
    )
  )

mortality <- 
  sinadef %>% 
  filter_at(
    vars(country, region, province, district, age_years, sex), ~ !is.na(.)
  ) %>% 
  filter(country == "PERU", age_years <= 123, sex != "INDETERMINADO") %>% 
  select(dated, year, month, week, region, province, district, sex, age_years) %>% 
  arrange(dated, region, province, district)

outpath <- path("data", "interim")
outfile <- path(outpath, "sinadef-20210920.csv")
write.csv(mortality, outfile, row.names = FALSE)
