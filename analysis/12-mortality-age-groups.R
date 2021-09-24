library(fs)
library(readr)
library(dplyr)
library(lubridate)
library(stringr)
library(tsibble)
library(ggplot2)
library(viridis)
library(slider)
library(forecast)
library(tidyr)
library(forcats)
library(lis)
library(ggsci)
library(scales)
library(extrafont)
library(showtext)

inpath <- path("data", "interim")

sinadef_raw <- 
  read_csv(
    path(inpath, "sinadef-20210920.csv"), 
    locale = locale(encoding = "utf-8")
  )

sinadef <- 
  sinadef_raw %>% 
  filter(dated < as.Date("2021-09-01")) %>% 
  mutate(
    age_group = cut(
      age_years, 
      breaks = quantile(age_years),
      # breaks = c(seq(0, 80, 10), max(age_years)), 
      include.lowest = TRUE, 
      right = FALSE
    )
  ) %>% 
  mutate(
    year_week = yearweek(dated, week_start = 7)
  )

sinadef %>%
  group_by(age_group) %>% 
  summarise(
    deaths = n()
  )

rm(sinadef_raw)
gc()

age_pop <- read_csv("data/raw/age-population.csv")
age_group_pop <- 
  age_pop %>% 
  mutate(
    age_group = cut(
      age, 
      breaks = quantile(sinadef$age_years),
      # breaks = c(seq(0, 80, 10), max(sinadef$age_years)),
      include.lowest = TRUE, 
      right = FALSE
    )
  ) %>% 
  group_by(age_group) %>% 
  summarise(pop = sum(cases), .groups = "drop")

# Excess deaths -----------------------------------------------------------

weekly_deaths <- 
  sinadef %>%
  group_by(age_group, year_week) %>%
  summarise(deaths = n(), .groups = "drop") %>% 
  mutate(week = week(year_week))

weekly_base <- 
  weekly_deaths %>% 
  filter(year(year_week) < 2020) %>% 
  group_by(age_group, week) %>% 
  summarise(base = mean(deaths), .groups = "drop")

weekly_excess_deaths <- 
  weekly_deaths %>% 
  arrange(age_group, year_week) %>% 
  filter(as.Date(year_week) > as.Date("2020-01-01")) %>% 
  left_join(weekly_base, by = c("age_group", "week")) %>% 
  mutate(excess_deaths = (deaths - base) / base) %>% 
  group_by(age_group) %>% 
  arrange(age_group, year_week) %>% 
  mutate(
    excess_deaths_ma = slide_dbl(
      excess_deaths, ~ mean(.x, na.rm = TRUE), .before = 3, .complete = TRUE
    )
  ) %>% 
  ungroup()

# Seasonal

# weekly_excess_deaths %>%
#   filter(age_group == "[56,71)") %>% 
#   pull(excess_deaths) %>% 
#   ts(frequency = 365.25/7) %>% 
#   ggseasonplot(polar = TRUE)
#   
# 
# weekly_excess_deaths %>% 
#   ggplot(aes(as.Date(year_week), excess_deaths, color = age_group)) +
#   geom_line(size = 1) +
#   geom_vline(xintercept = as.Date("2021-02-09"), lty = "dashed") +
#   geom_hline(yintercept = 0) +
#   scale_color_viridis(discrete = TRUE)
# 
# weekly_excess_deaths %>% 
#   filter(year_week != yearweek("2021 W29",  week_start = 7)) %>% 
#   ggplot() +
#   geom_line(aes(as.Date(year_week), excess_deaths, color = age_group), size = 1) +
#   geom_area(aes(as.Date(year_week), excess_deaths, fill = age_group), position = "identity", alpha = 0.1) +
#   geom_vline(xintercept = as.Date("2021-03-08"), lty = "dashed") +
#   scale_fill_viridis(discrete = TRUE) 


# Cummulative excess deaths -----------------------------------------------

# cumsum_deaths <- 
#   sinadef %>%
#   group_by(age_group, year_week) %>%
#   arrange(year_week) %>% 
#   summarise(deaths = n(), .groups = "drop") %>% 
#   mutate(cumsum_deaths = cumsum(deaths)) %>% 
#   mutate(week = week(year_week))
# 
# cumsum_deaths %>% 
#   filter(year(year_week) >= 2020) %>% 
#   ggplot(aes(as.Date(year_week), cumsum_deaths, color = age_group)) + 
#   geom_line() +
#   geom_vline(xintercept = as.Date("2021-03-08"), lty = "dashed")
# 
# cumsum_deaths_pop <- 
#   cumsum_deaths %>% 
#   filter(year(year_week) >= 2020) %>%
#   left_join(age_group_pop, by = "age_group") %>% 
#   mutate(cumsum_deaths_pop = 1000 * (cumsum_deaths / pop))
# 
# cumsum_deaths_pop %>% 
#   filter(year_week != yearweek("2021 W29",  week_start = 7)) %>%
#   ggplot(aes(as.Date(year_week), cumsum_deaths_pop, color = age_group)) + 
#   geom_line() +
#   geom_vline(xintercept = as.Date("2021-03-08"), lty = "dashed")
# 
# cumsum_base <- 
#   cumsum_deaths %>% 
#   filter(year(year_week) < 2020) %>% 
#   group_by(age_group, week) %>% 
#   summarise(base = mean(cumsum_deaths), .groups = "drop")
# 
# cumsum_excess_deaths <- 
#   cumsum_deaths %>% 
#   arrange(age_group, year_week) %>% 
#   filter(year(year_week) >= 2020) %>% 
#   left_join(cumsum_base, by = c("age_group", "week")) %>% 
#   mutate(cumsum_excess_deaths = 100 * (cumsum_deaths - base) / base)
# 
# cumsum_excess_deaths %>% 
#   ggplot(aes(as.Date(year_week), cumsum_excess_deaths, color = age_group)) + 
#   geom_line() +
#   geom_vline(xintercept = as.Date("2021-03-08"), lty = "dashed")


# Deaths acceleration -----------------------------------------------------

# deaths_acc <- 
#   sinadef %>% 
#   filter(year(dated) > 2019) %>% 
#   group_by(age_group, year_week) %>% 
#   summarise(deaths = n()) %>% 
#   mutate(deaths_v = deaths - lag(deaths)) %>% 
#   mutate(deaths_a = deaths_v - lag(deaths_v)) 
  # mutate(
  #   deaths_a_ma = slide_index_dbl(
  #     deaths_a, dated, ~ mean(., na.rm = TRUE), .before = 13, 
  #     .complete = TRUE
  #   )
  # )

# deaths_acc %>% 
#   ggplot(aes(as.Date(year_week), deaths_v, color = age_group)) +
#   geom_line(size = 1) +
#   geom_vline(xintercept = as.Date("2021-03-08"), lty = "dashed") +
#   geom_hline(yintercept = 0) +
#   scale_color_viridis(discrete = TRUE)

# Vaccines ----------------------------------------------------------------

vaccines_raw <- read_csv("data/raw/vaccines-20210920.csv")

vaccines <- 
  vaccines_raw %>% 
  filter(!(DEPARTAMENTO != "HUANUCO" & EDAD >= 121)) %>% 
  mutate(vaccination_date = ymd(FECHA_VACUNACION)) %>% 
  filter(vaccination_date < as.Date("2021-09-01")) %>% 
  filter(DOSIS == 2)

rm(vaccines_raw)
gc()

weekly_vaccines_fully <- 
  vaccines %>% 
  mutate(age_group = cut(
    EDAD, 
    breaks = quantile(sinadef$age_years),
    # breaks = c(seq(0, 80, 10), max(sinadef$age_years)),
    include.lowest = TRUE, right = FALSE
    )
  ) %>% 
  mutate(year_week = yearweek(vaccination_date, week_start = 7)) %>% 
  group_by(age_group, year_week) %>% 
  arrange(year_week) %>% 
  summarise(fully_vaccinated = n(), .groups = "drop") 

# TODO: Check dates. The first second dose was given on 2021-02-15?

weekly_deaths_vaccines <- 
  weekly_excess_deaths %>% 
  left_join(weekly_vaccines_fully, by = c("age_group", "year_week")) %>% 
  left_join(age_group_pop, by = "age_group") %>%
  mutate(
    fully_vaccinated = ifelse(is.na(fully_vaccinated), 0, fully_vaccinated)
  ) %>% 
  group_by(age_group) %>%
  mutate(
    fully_vaccinated_pct = 100 * cumsum(fully_vaccinated) / pop,
    id_time = row_number()
  ) %>% 
  mutate(
    fully_vaccinated_pct_round = round(fully_vaccinated_pct, 0)
  )
  
# TODO: Check inconsistencies with weeks

# weekly_deaths_vaccines %>% 
#   mutate(
#     report_date = as.Date(year_week)
#   ) %>% 
#   write.csv("data/processed/weekly-deahts-vaccines.csv", row.names = FALSE)

plt_weekly_excess_deaths <- 
  weekly_deaths_vaccines %>% 
  ggplot(aes(id_time, excess_deaths_ma, color = age_group)) +
  geom_line(size = 1) +
  geom_point() +
  geom_vline(xintercept = 59, lty = "dashed") +
  geom_hline(yintercept = 0) +
  scale_color_viridis(discrete = TRUE) +
  scale_x_continuous(
    limits = c(1, 82), 
    expand = c(0.025, 0.025), 
    breaks = c(1, 26, 52, 54, 68, 82)
  ) +
  scale_y_continuous(n.breaks = 6) +
  labs(x = NULL, y = NULL)

print(plt_weekly_excess_deaths)

# DON'T RUN
# pdf("analysis/figs/weekly-excess-deaths.pdf", width = 17, height = 10)
# print(plt_weekly_excess_deaths)
# dev.off()

#-----------------------

weekly_deaths_vaccines_progress <- 
  weekly_deaths_vaccines %>% 
  filter(as.Date(year_week) > as.Date("2021-02-01")) %>% 
  mutate(
    vacc_progress = cut(
      fully_vaccinated_pct,
      breaks = seq(0, 100, 20),
      include.lowest = TRUE, 
      right = TRUE
    )
  ) %>% 
  mutate(
    vacc_progress = factor(
      vacc_progress,
      levels = c(
        "[0,20]", "(20,40]", "(40,60]", "(60,80]", "(80,100]"
      ),
      labels = c(
        " 20%", "< 20 - 40%", "< 40 - 60%", "< 60 - 80%", "< 80%"
      )
    )
  ) %>%
  group_by(age_group, vacc_progress) %>% 
  summarise(excess_deaths_mean = mean(excess_deaths), .groups = "drop") 

weekly_deaths_vaccines_progress <- 
  weekly_deaths_vaccines_progress %>% 
  group_by(age_group) %>% 
  complete(
    vacc_progress = levels(weekly_deaths_vaccines_progress$vacc_progress),
    fill = list(excess_deaths_mean = 0)
  ) %>% 
  mutate(
    vacc_progress = factor(vacc_progress)
  ) %>% 
  ungroup() %>% 
  mutate(
    age_group = factor(
      age_group,
      levels = c(
        "[0,56)", "[56,71)", "[71,83)", "[83,123]"
      ),
      labels = c(
        "Menores a 56", "De 56 a 70", "De 70 a 82", "Mayores a 83"
      )
    )
  ) 

weekly_deaths_vaccines_progress %>% 
  mutate(age_group = fct_rev(age_group)) %>% 
  ggplot(
    aes(
      x = vacc_progress, 
      y = excess_deaths_mean, 
      fill = age_group, 
      group = interaction(age_group, vacc_progress)
    )
  ) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_y_continuous(labels = label_percent()) +
  scale_x_discrete(
    limits = rev(levels(weekly_deaths_vaccines_progress$vacc_progress))
  ) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(
    x = "Avance de vacunación", y = "Exceso de mortalidad", fill = "Grupo etário"
  ) +
  coord_flip() +
  scale_fill_npg()

weekly_deaths_vaccines_progress %>% 
  ggplot(
    aes(
      x = vacc_progress, 
      y = excess_deaths_mean, 
      fill = age_group, 
      group = interaction(age_group, vacc_progress)
    )
  ) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_y_continuous(labels = label_percent(), expand = c(0, 0)) +
  labs(
    x = "Avance de vacunación", y = "Exceso de mortalidad", fill = "Grupo etário"
  ) +
  scale_fill_npg() +
  theme(legend.position = "top")
  
names(lis:::lis_palettes) %>%
  purrr::map(.f = ~lis_pal(., reverse = T)(10)) %>%
  unikn::seecol(pal_names = names(lis:::lis_palettes))

#--------------------------------

death_peaks <- 
  weekly_excess_deaths %>% 
  filter(year(year_week) == 2020) %>% 
  group_by(age_group) %>% 
  summarise(excess_death_peak = max(excess_deaths))

weekly_deaths_vaccines_peaks <- 
  weekly_deaths_vaccines %>% 
  filter(year(year_week) == 2021) %>% 
  left_join(death_peaks, by = "age_group") %>% 
  mutate(
    percent_peak = 100 * (excess_deaths / excess_death_peak)
  ) %>% 
  

weekly_deaths_vaccines_peaks %>% 
  ggplot(aes(x = year_week, y = percent_peak, color = age_group)) +
  geom_line()

#--------------------------------

weekly_deaths_vaccines_milestones <- 
  weekly_deaths_vaccines %>% 
  group_by(age_group) %>% 
  slice(
    c(
      which(fully_vaccinated_pct_round > 9)[1],
      which(fully_vaccinated_pct_round > 29)[1],
      which(fully_vaccinated_pct_round > 49)[1],
      which(fully_vaccinated_pct_round > 69)[1],
      which(fully_vaccinated_pct_round > 89)[1]
    )
  ) %>% 
  ungroup() %>% 
  mutate(
    milestone = ifelse(
      (fully_vaccinated_pct_round >= 10 & 
         fully_vaccinated_pct_round < 30),
      "> 10%",
      ifelse(
        (fully_vaccinated_pct_round >= 30 & 
           fully_vaccinated_pct_round < 50),
        "> 30%",
        ifelse(
          (fully_vaccinated_pct_round >= 50 & 
             fully_vaccinated_pct_round < 70),
          "> 50%",
          ifelse(
            (fully_vaccinated_pct_round >= 70 & 
               fully_vaccinated_pct_round < 90),
            "> 70%",
            "> 90%"
          )
        )
      )
    )
  ) %>% 
  mutate(
    milestone = factor(
      milestone,
      levels = c("> 10%", "> 30%", "> 50%", "> 70%", "> 90%")
    ),
    age_group = factor(
      age_group,
      levels = c(
        "[0,56)", "[56,71)", "[71,83)", "[83,123]"
      ),
      labels = c(
        "Menores de 56", "De 56 a 70", "De 71 a 82", "Mayores de 82"
      )
    )
  )

weekly_deaths_vaccines_milestones <- 
  weekly_deaths_vaccines_milestones %>% 
  group_by(age_group) %>% 
  complete(
    milestone = levels(weekly_deaths_vaccines_milestones$milestone),
    fill = list(excess_deaths = 0)
  ) %>% 
  ungroup()

theme_set(theme_minimal())

font_add_google("Roboto", "roboto")
font_add_google("Inconsolata", "inconsolata")
showtext_auto()

final_plot <- 
  weekly_deaths_vaccines_milestones %>% 
  ggplot(
    aes(
      x = milestone, 
      y = excess_deaths, 
      fill = age_group, 
      group = interaction(age_group, milestone)
    )
  ) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_y_continuous(labels = label_percent(), expand = c(0, 0)) +
  labs(
    x = "Hitos de vacunación", y = "Exceso de mortalidad (%)", fill = "Grupo etário",
    title = "Exceso de mortalidad por hitos de vacunación en diferentes grupos etários",
    subtitle = "Periodo de análisis desde la primera semana de febrero a la última de agosto de 2021. \nExceso de mortalidad calculado con base en el periodo 2017 - 2019.",
    caption = "Datos Ministerio de Salud, Elaboración: Pendulum - Data Lab"
  ) +
  scale_fill_npg() +
  theme(
    legend.position = "top", 
    panel.grid.major.x = element_blank(), 
    plot.title = element_text(
      size = 18, 
      face = "bold"
    ),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = 14),
    text = element_text(size = 12, family = "roboto"),
    plot.caption = element_text(
      family = "inconsolata", margin = margin(10, 10, 30, 10)
    )
  )

print(final_plot)

pdf("analysis/figs/plot.pdf", width = 10, height = 7)
print(final_plot)
dev.off()
