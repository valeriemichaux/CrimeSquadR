library(tidyverse)
library(psych)
source("qualtricshelpers.R")

# Thema: Veränderungskompetenz und Persönlichkeit

# Vor Corona
filename01 <- "data/Umfrage 01.csv"
raw01 <- load_qualtrics_csv(filename01)


# Erste Welle
raw02 <- load_qualtrics_csv("data/Umfrage 02.csv")

# Dritte Welle
raw03 <- load_qualtrics_csv("data/Umfrage 03.csv")


# Todo:
# 1. Kontrollieren, ob Fragebogen 1 und 2 identisch sind. Wo kommt das Zusatzitem her??----
# > Fragebogen 1 und 2 sind identisch; das Zusatzitem (110 statt 109 Variablen) ist die SID am Ende von Fragebogen 2


#SID und Durchgang bei 01 einfügen
raw01 %>%
  select(everything()) %>%
  mutate(SID = NA) %>%
  mutate(iteration = 1) -> raw01_new

#Durchgang bei 02 einfügen
raw02 %>%
  select(everything()) %>%
  mutate(iteration = 2) -> raw02_new


# 2. Fragen zu Gesundheit aus Fraghebogen 3 entfernen und mit dem selben Schema adaptieren.----
# Offene Frage: Was soll hier für edu genommen werden? Höchster Schulabschluss oder höchster Ausbildungsabschluss?
# entweder bind_cols(raw03[,c(20)]) für höchsten Schulabschluss oder bind_cols(raw03[,c(21)] anstelle des mutate(edu=NA)

raw03_new <- raw03[, c(1:19)]
raw03_new %>%
  bind_cols(raw03[, c(24:26)]) %>%
  mutate(edu = NA) %>%
  mutate(ANAG = NA) %>%
  bind_cols(raw03[, c(27)]) %>%
  bind_cols(raw03[, c(29:95)]) %>%
  bind_cols(raw03[, c(172:187)]) %>%
  mutate(comment = NA) %>%
  mutate(SID = NA) %>%
  mutate(iteration = 3) -> raw03_new


# 3. Ziel: Ein gemeinsames Codebook, was für alle 3 dataframes korrekt ist----

#codebook für alle 3 Umfragen generieren
if(FALSE){
generate_codebook(raw01_new, filename01, "data/codebook.csv")
}

#Codebook auf die Datensätze anwenden

codebook <- read_codebook("data/codebook_final.csv")

names(raw01_new) <- codebook$variable
names(raw02_new) <- codebook$variable
names(raw03_new) <- codebook$variable


#Datensätze zusammenfügen
DTAlonterm<- rbind(raw01_new,raw02_new,raw03_new)


#weitere Data-cleaning steps (falls du brauchst)----
#Testdaten und unvollständige Daten entfernen in allen 3 Datensätzen ----
if (FALSE) {
  DTAlongterm <-DTAlongterm %>%
    filter(Status == "IP Address") %>%
    filter(Progress == 100)%>%
    filter(comment !="Pretest"| is.na(comment))%>%
    filter(comment !="Pretest!"| is.na(comment))
}

# Unnötige Spalten entfernen: ----
# ResponseID sollte erhalten bleiben.
if (FALSE) {
  raw.short <- raw[,c(-1:-8, -10:-17)]
}

#richtige Skalenniveaus zuordnen----
if(FALSE) {
  DTAlongterm <- DTAlonterm %>%
  mutate_at(vars(gender, jobtype, branch, iteration), as.factor)


  DTAlongterm$edu <- ordered(DTAlongterm$edu, levels = c("Haupt- oder Realschulabschluss",
                                                         "Fach-/Hochschulreife(Abitur)",
                                                         "Ausbildung",
                                                         "Hochschulabschlusss",
                                                         "Promotion"))

DTAlonterm$jobtype <- ordered(DTAlonterm$jobtype, levels = c("In Ausbildung / Studium",
                                                           "Arbeitnehmer/-in und Studierende/-r",
                                                           "Arbeitnehmer/-in",
                                                           "Arbeitgeber/-in",
                                                           "Selbstständig ohne Mitarbeiter",
                                                           "Rentner/-in"))

DTAlonterm$duration <- ordered(DTAlonterm$duration, levels = c("Weniger als ein Jahr",
                                                             "1-4 Jahre",
                                                             "5-9 Jahre",
                                                             "10-20 Jahre",
                                                             "Mehr als 20 Jahre"))
ordered.zustimmung <- function(x){
  res <- ordered(x, levels = c("Stimme gar nicht zu",
                               "Stimme nicht zu",
                               "Stimme eher nicht zu",
                               "Stimme eher zu",
                               "Stimme zu",
                               "Stimme voll zu"))
  res
}

ordered.zutreffen <- function(x){
  res <- ordered(x, levels = c("Trifft überhaupt nicht zu",
                               "Trifft nicht zu",
                               "Trifft eher nicht zu",
                               "Trifft eher zutreffend",
                               "Trifft zu",
                               "Trifft voll zu"))
  res
}

raw.short <- raw.short %>%
  mutate_at(vars(starts_with(c("ati"), ignore.case = F)), ordered.zustimmung) %>%
  mutate_at(vars(starts_with(c("bf", "moti", "wrfq", "willi", "abili", "commi", "darf"), ignore.case = F)), ordered.zutreffen)


}
