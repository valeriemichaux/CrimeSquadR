#### Pakete laden: ---- 
library(tidyverse)
library(psych)
source("qualtricshelpers.R")

# Rohdaten einlesen: ----
filename <- "data/data_final.csv"
raw <- readr::read_csv("data/data_final.csv")

#Spezielle Funktion, die helfen soll nachher besser mit dem Codebook zu arbeiten.
# Entfernt die ersten beiden Zeilen. Später prüfen, ob wir diese wirklich brauchen.

# Testdaten und unvollständige Daten entfernen ----
# 1. Hinweis: Pretester nicht enthalten, da Pretest im Testmode stattgfunden hat
# 2. Hinweis: Ausschluss unvollständiger Fragebögen über Spalte "sys_LastQuestion". Probanden, bei denen in diesem Feld "EndText" steht haben die Umfrage vollständig ausgefüllt. Probanden, bei denen in diesem Feld "CBC_Random12" steht haben die Umfrag eauf dem EndText weggeklickt.

raw %>% 
  filter(sys_LastQuestion %in% c("EndText", "CBC_Random12")) -> raw

# Datensatz noch auf Speeder prüfen und diese ggf. rausfiltern
#Spalte "sys_ElapsedTime" ist die Dauer in Sekunden 

speedlimit <- median(raw$sys_ElapsedTime) / 2
raw %>% 
  filter(!sys_ElapsedTime < speedlimit) -> data.sawtooth

write.csv(data.sawtooth,'data.sawtooth.csv')
