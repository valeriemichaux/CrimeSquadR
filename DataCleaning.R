#### Pakete laden: ---- 
library(tidyverse)
library(psych)
source("qualtricshelpers.R")

# Rohdaten einlesen: ----
filename <- "data/data_final.csv"
raw <- readr::read_csv("data/data_final.csv")

# Testdaten und unvollständige Daten entfernen ----
# 1. Hinweis: Pretester nicht enthalten, da Pretest im Testmode stattgfunden hat
# 2. Hinweis: Ausschluss unvollständiger Fragebögen über Spalte "sys_LastQuestion". Probanden, bei denen in diesem Feld "EndText" steht haben die Umfrage vollständig ausgefüllt. Probanden, bei denen in diesem Feld "CBC_Random12" steht haben die Umfrag eauf dem EndText weggeklickt.

raw %>% 
  filter(sys_LastQuestion %in% c("EndText", "CBC_Random12")) -> raw

# Datensatz noch auf Speeder prüfen und diese ggf. rausfiltern
#Spalte "sys_ElapsedTime" ist die Dauer in Sekunden 

speedlimit <- median(raw$sys_ElapsedTime) / 2
raw %>% 
filter(!sys_ElapsedTime < speedlimit) -> raw

#Durch den Code werden 2 Speeder ausgeschlossen. Probanden haben 268 (4 Min, 28 Sek) bzw. 132 (2. Min, 12 Sek.) Sekunden für die Umfrage benötigt

# Unnötige Spalten entfernen: ----
# mit Entfernen der Spalten 40 bis 51 würde CBC Teil ebenfalls ausgekürzt
raw.short <- raw[,c(-2:-19, -40:-51, -54:-81)]

# Spalten umbenennen: ----

#generate_codebook(raw.short, filename, "data/codebook.csv")

codebook<-read_codebook("data/codebook_final.csv")

names(raw.short) <- codebook$variable

# Richtige Skalenniveaus zuordnen: ----

# Alter ist bereits numerisch

# Faktoren werden in raw.short als numerics geliefert, wir müssen Zahlen also die richtigen Faktoren zuordnen

raw.short %>% 
  mutate_at(vars(gender, jobtype, education), as.factor) -> raw.short

raw.short %>% 
  mutate(gender = case_when(
    gender == 1 ~ "maennlich",
    gender == 2 ~ "weiblich",
    gender == 3 ~ "divers"
  )) -> raw.short

raw.short %>% 
  mutate(jobtype = case_when(
    jobtype == 1 ~ "Schueler*in",
    jobtype == 2 ~ "Student*in",
    jobtype == 3 ~ "Auszubildende*r",
    jobtype == 4 ~ "Angestellte*r",
    jobtype == 5 ~ "Rentner*in",
    jobtype == 6 ~ "Hausfrau/Hausmann",
    jobtype == 7 ~ "Zurzeit arbeitssuchend"
    )) -> raw.short

#Dieser Code funktioniert nicht, keine Ahnung wieso 

raw.short %>% 
  mutate(education = case_when(
    education == 1 ~ "Kein Schulabschluss",
    education == 2 ~ "Volks- und Hochschulabschluss",
    education == 3 ~ "Mittlere Reife/Realschulabschluss",
    education == 4 ~ "Berufsausbildung",
    education == 5 ~ "Fach-/Allgemeine Hochschulreife",
    education == 6 ~ "Fach- oder Hochschulabschluss",
    education == 7 ~ "Promotion",
    education == 8 ~ "Sonstiges"
  )) -> raw.short

# Skalen berechnen: ----

# Items zu Skalen zuordnen. Negative Items erhalten ein Minus INNERHALB der Anführungszeichen.
schluesselliste <- list(
  KUT = c("kut_1","-kut_2", "kut_3", "kut_4", "-kut_5", "kut_6", "-kut_7", "-kut_8"),
  GAAIS = c("gaais_1","-gaais_2", "-gaais_3", "gaais_4", "gaais_5", "-gaais_6", "gaais_7", "-gaais_8", "gaais_9", "-gaais_10"))

# Skalen berechnen. Über min max festlegen, ob 1 bis 6 oder 0 bis 5 angewendet werden soll. 
scores <- scoreItems(schluesselliste, items = raw.short, missing = TRUE, min = 1, max = 6)

#Check Cronbachs Alpha
scores$alpha


# Berechnete Skalan hinten an raw.short anfügen.
data <- bind_cols(raw.short, as_tibble(scores$scores))

data_short <- data[,c(-6:-23)]


write.csv(data_short,'datacleaning.csv')





  
