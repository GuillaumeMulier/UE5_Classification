# --------------------------------- #
# Devoir UE Classification SMSDS    #
# Benoit Gachet et Guillaume Mulier #
# 29/11/2020                        #
# --------------------------------- #

#### Packages ----

library(tidyverse)

#### Partie classification non supervisée ----

## Import de la base

les_inconnus <- read_delim("DonneesNSGpe.txt", delim = "\t") %>% 
  filter(Groupe == 7) %>% 
  select(-Groupe)
sum(is.na(les_inconnus)) # Pas de données manquantes
glimpse(les_inconnus)

#### Partie classification supervisée ----

## Import de la base

dix_pour_cent <- read_csv("https://archive.ics.uci.edu/ml/machine-learning-databases/00471/Data_for_UCI_named.csv") %>% 
  select(-p1, -stab)
sum(is.na(dix_pour_cent)) # Pas de données manquantes
glimpse(dix_pour_cent)

###

vingt_pour_cent <- dix_pour_cent