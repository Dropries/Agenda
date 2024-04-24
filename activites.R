library(tidyr)
library(dplyr)
library(lubridate)
library(stringr)
library(yaml)

conf<-read_yaml("config.yaml")

csv_files<-list.files(pattern=".csv")

if(length(csv_files)==0){
  stop(paste0("L'agenda doit être converti au format .csv et être placé dans le répértoire ",
             rstudioapi::getSourceEditorContext()$path%>%dirname,
             ". Voir le README si besoin d'aide."))
}
if(length(csv_files)>1){
  stop(paste0("Il ne doit y avoir qu'un export au format .csv dans le répértoire ",
              rstudioapi::getSourceEditorContext()$path%>%dirname,
              ". Supprimer les .csv non utilisés."))
}

formated <- read.table(csv_files, sep = "\t", header = T) %>%
  reframe(
    debut = as.POSIXct(Given.planned.earliest.start, tryFormats=c("%d.%m.%Y %H:%M","%m/%d/%Y %H:%M")),
    fin = as.POSIXct(Given.planned.earliest.end, tryFormats=c("%d.%m.%Y %H:%M","%m/%d/%Y %H:%M")),
    day_debut = as.Date(debut),
    day_fin = as.Date(fin),
    event = str_to_title(Title),
    duree = as.numeric(fin - debut)
  ) %>%
  filter(between(day_debut, as.Date(conf$date_de_debut), as.Date(conf$date_de_fin))) %>%
    group_by(day_debut) %>%
    mutate(sum = sum(duree)) %>%
    group_by(day_debut, event) %>%
    reframe(prop = round(sum(duree) / sum, conf$precision)) %>% distinct%>%
  rename(Jour=day_debut,Activité=event,Temps=prop)

# Remplissage par jour
formated %>%
    split(.$Jour)

# Comparaison avec les objectifs
formated %>% group_by(Activité) %>% summarise('Temps total' = sum(Temps))
