library(tidyr)
library(dplyr)
library(lubridate)
library(stringr)

d0 = as.Date("2024-04-01")
d1 = as.Date("2024-04-30")

formated <- read.table("output.csv", sep = ",", header = T) %>%
  reframe(
    debut = as.POSIXct(dtstart, format = "%Y-%m-%dT%H:%M:%SZ"),
    fin = as.POSIXct(dtend, format = "%Y-%m-%dT%H:%M:%SZ"),
    day_debut = as.Date(debut),
    day_fin = as.Date(fin),
    event = str_to_title(summary),
    duree = as.numeric(fin - debut)
  ) %>%
  filter(between(day_debut, d0, d1)) %>%
    group_by(day_debut) %>%
    mutate(sum = sum(duree)) %>%
    group_by(day_debut, event) %>%
    reframe(prop = round(sum(duree) / sum, 2)) %>% distinct

formated %>%
    group_split(day_debut)

formated %>% group_by(event) %>% summarise(sum = sum(prop))
