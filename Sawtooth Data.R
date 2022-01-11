#### Pakete laden: ---- 
library(tidyverse)
library(psych)
source("qualtricshelpers.R")

# Rohdaten einlesen: ----
filename <- "data/data_final.csv"
raw <- load_qualtrics_csv(filename)
#raw <- readr::read_csv("data/data_final.csv")

#Spezielle Funktion, die helfen soll nachher besser mit dem Codebook zu arbeiten.
# Entfernt die ersten beiden Zeilen. Später prüfen, ob wir diese wirklich brauchen.

# Testdaten und unvollständige Daten entfernen ----
# 1. Hinweis: Pretester nicht enthalten, da Pretest im Testmode stattgfunden hat
# 2. Hinweis: Ausschluss unvollständiger Fragebögen über Spalte "sys_RespStatus", wenn diese 5 ist, war die letzte "Frage" der Probanden der "EndText"

raw %>% 
  filter(sys_LastQuestion %in% c("EndText", "CBC_Random12")) -> raw

# Datensatz noch auf Speeder prüfen und diese ggf. rausfiltern
#Spalte "sys_ElapsedTime" ist die Dauer in Sekunden 

speedlimit <- median(raw$sys_ElapsedTime) / 2
raw %>% 
  filter(!sys_ElapsedTime < speedlimit) -> data.sawtooth

write.csv(data.sawtooth,'data.sawtooth.csv')
