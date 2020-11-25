# --------------------------------- #
# Devoir UE Classification SMSDS    #
# Benoit Gachet et Guillaume Mulier #
# 29/11/2020                        #
# --------------------------------- #

#### Packages ----

library(tidyverse)
library(patchwork)
library(ggtext)
library(FactoMineR)
library(factoextra)
library(dendextend)

theme_set(theme_light() +
            theme(plot.title = element_markdown()))

#### Partie classification non supervisée ----

### Import de la base

les_inconnus <- read_delim("DonneesNSGpe.txt", delim = "\t") %>% 
  filter(Groupe == 7) %>% 
  select(-Groupe)
sum(is.na(les_inconnus)) # Pas de données manquantes
glimpse(les_inconnus)

### Description des données

## Ordre de grandeur

les_inconnus %>% 
  summarise(across(everything(), .fns = list(moyenne = mean, ecart_type = sd, mini = min, maxi = max))) %>% 
  pivot_longer(everything(), names_to = c("Variable", "stat"), names_pattern = "^(V[0-9]*)_(.*)") %>% 
  pivot_wider(names_from = "stat")
# Variables autour de 0 avec des unités assez proches, même si certaines un peu plus grandes

## ACP

ACP_inc <- PCA(les_inconnus, scale.unit = TRUE, graph = FALSE)
fviz_screeplot(ACP_inc, addlabels = TRUE)
# les 2 1ers axes captent la quasi-totalité de l'inertie de l'échantillon donc on ne regardera que ces 2 1ers axes
plot_axes <- fviz_pca_var(ACP_inc, repel = TRUE) +
  labs(title = "Cercle des corrélations des variables aux axes 1 et 2") +
  theme(plot.title = element_markdown(size = 10))
plot_ind <- fviz_pca_ind(ACP_inc, geom.ind = "point") +
  labs(title = "Projection des individus sur les 2 1<sup>ers</sup> axes de l'ACP") +
  theme(plot.title = element_markdown(size = 10))
plot_ind + plot_axes
# On a l'air d'avoir 3 groupes

### Classification non supervisée

## CAH

# Dendrogramme en 3 classes
dendro <- les_inconnus %>% 
  dist(method = "euclidean") %>% 
  hclust(method = "ward.D2") %>% 
  as.dendrogram() %>%
  set("branches_k_color", k = 3) %>% 
  set("branches_lwd", 0.5) %>%
  set("labels_cex", 0) %>%
  set("leaves_pch", 10) %>% 
  set("leaves_cex", 0.1) %>% 
  as.ggdend() %>% 
  ggplot(theme = theme_light()) +
  theme(axis.ticks.x = element_blank())

#### Partie classification supervisée ----

## Import de la base

dix_pour_cent <- read_csv("https://archive.ics.uci.edu/ml/machine-learning-databases/00471/Data_for_UCI_named.csv") %>% 
  select(-p1, -stab)
sum(is.na(dix_pour_cent)) # Pas de données manquantes
glimpse(dix_pour_cent)

###


## lalalallaa

vingt_pour_cent <- dix_pour_cent
x_pour_cent <- vingt_pour_cent
