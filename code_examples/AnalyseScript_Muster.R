#### Pakete laden: ---- 
library(tidyverse)
library(psych)
source("qualtricshelpers.R")

# Rohdaten einlesen: ----
filename <- "data/"
raw <- load_qualtrics_csv(filename)

# Testdaten und unvollständige Daten entfernen ----
# Hinweis: Pretester haben bei SID "Pretest" stehen. 
raw %>% 
  filter(Status == "IP Address") %>% 
  filter(Progress == 100) -> raw

# Unnötige Spalten entfernen: ----
# ResponseID sollte erhalten bleiben. 
raw.short <- raw[,c(-1:-8, -10:-17)]

# Spalten umbenennen: ----

generate_codebook(raw.short, filename, "data/codebook.csv")
# Hier codebook.csv in excel o.Ä. öffnen und in der ersten Spalte die Variablen umbenennen. 
codebook <- read_codebook("data/codebook_final.csv")
# Namen anwenden. 
names(raw.short) <- codebook$variable

# Richtige Skalenniveaus zuordnen: ----

# Einfachster Fall: Variablen sind Zahlen oder ungeordnete Faktoren: 
raw.short$age <- as.numeric(raw.short$age)

raw.short %>% 
  mutate_at(vars(gender, branch, jobtype), as.factor) -> raw.short

# Variablen sind ordinale Variablen. 
# Fall 1: Die Skala kommt genau einmal vor: 
raw.short$edu <- ordered(raw.short$edu, levels = c("Haupt- oder Realschulabschluss", 
                                                   "Fach-/Hochschulreife (Abitur)", 
                                                   "Ausbildung",
                                                   "Hochschulabschluss",
                                                   "Promotion"))

# Fall 2: Die Skala kommt mehrfach zum Einsatz.
ordered.zustimmung <- function(x){
  res <- ordered(x, levels = c("Stimme gar nicht zu", 
                               "Stimme nicht zu", 
                               "Stimme eher nicht zu", 
                               "Stimme eher zu", 
                               "Stimme zu", 
                               "Stimme voll zu"))
  res
}

raw.short %>% 
  mutate_at(vars(starts_with(c("bf", "moti", "wrfq", "willi", "abili", "commi", "dark"), ignore.case = F)), ordered.zutreffen) %>% 
  mutate_at(vars(starts_with(c("ati"), ignore.case = F)), ordered.zutreffen)-> raw.short

# Skalen berechnen: ----

# Items zu Skalen zuordnen. Negative Items erhalten ein Minus INNERHALB der Anführungszeichen.
schluesselliste <- list(
  SKALA1 = c("item1", "item2", "item3", "-item4negativ"),
  SKALA2 = c("-item1negativ", "-item2negativ", "item3", "item4")
)

# Skalen berechnen. Über min max können Sie festlegen, ob 1 bis 6 oder 0 bis 5 angewendet werden soll. 
scores <- scoreItems(schluesselliste, items = raw.short, missing = TRUE, min = 1, max = 6)

# Berechnete Skalan hinten an raw.short anfügen.
data <- bind_cols(raw.short, as_tibble(scores$scores))

# Items entfernen: ----
# irgnore.case = F ist wichtig, da ansonsten die Skalenwerte auch entfernt werden. 
data <- data %>% 
  select(-starts_with("kut", ignore.case = F)) %>% 
  select(-starts_with("nfc", ignore.case = F))

# RDS abspeichern: ----
# Die abgespeicherte rds Datei kann über readRDS() eingelesen werden.  
saveRDS(data, "data/data.rds")