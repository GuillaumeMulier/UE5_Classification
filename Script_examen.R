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
library(purrr)

theme_set(theme_light() +
            theme(plot.title = element_markdown()))

#### Partie classification non supervisée ----

### Import de la base

les_inconnus <- read_delim("DonneesNSGpe.txt", delim = "\t") %>% 
  filter(Groupe == 7) %>% 
  select(-Groupe)
sum(is.na(les_inconnus)) # Pas de données manquantes
glimpse(les_inconnus)

les_inconnus_shopenhauer <- les_inconnus

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
  hclust(method = "ward.D2")
dendro_marches <- cbind(x = 1:500, inertie = sort(dendro$height, decreasing = TRUE), color = c(1, 1, 1, rep(2, 497))) %>% 
  as.data.frame() %>% 
  slice(1:20) %>% 
  ggplot(aes(x = x, y = inertie)) +
  geom_step() +
  geom_point(aes(color = as.factor(color)), show.legend = FALSE) +
  geom_vline(xintercept = 3.5, color = "blue") +
  scale_color_manual(values = c("blue", "black")) +
  scale_x_continuous(breaks = 1:20) +
  labs(x = "Nombre de branches",
       y = "Inertie intra-classe",
       title = "Décroissance de l'inertie avec le nombre de branches") +
  theme(panel.grid.minor.x = element_blank())
dendro_arbre <- dendro %>% 
  as.dendrogram() %>%
  set("branches_k_color", k = 3) %>% 
  set("branches_lwd", 0.5) %>%
  set("labels_cex", 0) %>%
  set("leaves_pch", 10) %>% 
  set("leaves_cex", 0.1) %>% 
  as.ggdend() %>% 
  ggplot(theme = theme_light()) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank()) +
  labs(y = "Inertie intra-classe",
       x = NULL,
       title = "Classification ascendante hiérarchique selon la distance euclidienne") +
  geom_hline(yintercept = 25, linetype = "dashed", color = "purple3") +
  geom_segment(aes(x = 179, xend = 179, y = 25, yend = 0), linetype = "dashed", color = "purple3") +
  geom_segment(aes(x = 329, xend = 329, y = 25, yend = 0), linetype = "dashed", color = "purple3")
# Il y a un beau saut dans les inerties intra-classes et on retient 3 classes
dendro_marches + dendro_arbre

les_inconnus_shopenhauer$class_cah <- cutree(dendro, k = 3)
table(les_inconnus_shopenhauer$class_cah) # Répartition des individus en 3 classes

ACP_inc <- PCA(les_inconnus_shopenhauer, scale.unit = TRUE, graph = FALSE, quali.sup = 11)
plot_ind2 <- fviz_pca_ind(ACP_inc, geom.ind = "point", habillage = 11) +
  labs(title = "Projection des individus sur les 2 1<sup>ers</sup> axes de l'ACP",
       subtitle = "Colorés selon les groupes faits par CAH") +
  theme(plot.title = element_markdown(size = 10),
        plot.subtitle = element_text(size = 7),
        legend.position = "none")
plot_ind2 + plot_axes

## k-means

# Détermination du nombre de clusters

replication_kmeans <- function(data, n_rep, nb_clust, result = "modele") {
  
  result <- match.arg(result, c("modele", "liste"))
  
  kmeans_rep <- replicate(n_rep, kmeans(data, centers = nb_clust, nstart = 25))
  best <- which.min(unlist(kmeans_rep['tot.withinss', ]))
  kmeans <- kmeans_rep[, best]
  
  if (result == "modele") {
    return(kmeans)
  } else if (result == "liste") {
    return(kmeans_rep)
  }
}

modeles <- vector(mode = "list", length = 10)
for (i in 1:10) {
  cat(i, "\n")
  modeles[[i]] <- replication_kmeans(data = les_inconnus, nb_clust = i, n_rep = 25)
}
map_dfr(modeles, ~ c(intra = .x[["tot.withinss"]]), .id = "k") %>% 
  bind_cols(color = c(1, 1, 1, rep(2, 7))) %>% 
  ggplot(aes(x = as.numeric(k), y = intra)) +
  geom_point(aes(color = as.factor(color)), show.legend = FALSE) +
  geom_step() +
  geom_vline(xintercept = 3.5, color = "blue") +
  scale_color_manual(values = c("blue", "black")) +
  scale_x_continuous(breaks = 1:10) +
  labs(x = "Nombre de clusters",
       y = "Inertie intra-clusters",
       title = "Décroissance de l'inertie avec le nombre de clusters",
       subtitle = "Sélection de la meilleure classification parmi 25 réplicats à chaque k") +
  theme(panel.grid.minor.x = element_blank())

# Choix de k = 3
kmeans_replicats <- replication_kmeans(data = les_inconnus, nb_clust = 3, n_rep = 25, result = "liste")
kmeans_replicats[1, ] %>% 
  map_dfr(~ c(clust = .x)) %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  mutate(rowname = str_remove(rowname, "^clust"))

kmeans_replicats[7, ] %>% 
  map_dfr(~ c(c1 = .x[1], c2 = .x[2], c3 = .x[3])) %>% 
  bind_cols(rep = 1:25) %>% 
  pivot_longer(cols = -rep) %>% 
  ggplot(aes(rep, value, color = name)) +
  geom_point() +
  geom_line() +
  coord_cartesian(ylim = c(140, 180))
# Les effectifs dans les clusters sont relativement stables et équilibrés

for (i in 2:25) {
  tableau <- table(kmeans_replicats[, 1]$cluster, kmeans_replicats[, i]$cluster)
  num_1 <- as.numeric(which.max(tableau[, 1]))
  num_2 <- as.numeric(which.max(tableau[, 2]))
  num_3 <- as.numeric(which.max(tableau[, 3]))
  kmeans_replicats[, i]$cluster <- case_when(kmeans_replicats[, i]$cluster == 1 ~ num_1,
                                             kmeans_replicats[, i]$cluster == 2 ~ num_2,
                                             kmeans_replicats[, i]$cluster == 3 ~ num_3,
                                             TRUE ~ NA_real_) 
}
liste_clusters <- kmeans_replicats[1, ] %>% 
  map_dfr(~ c(clust = .x)) %>% 
  t() %>% 
  as.data.frame() %>% 
  apply(1, function(x) {paste(x, collapse = "")}) %>% 
  data.frame(seq = .)
count(liste_clusters, seq, sort = TRUE)
liste_clusters %>% 
  mutate(seq = ifelse(str_detect(seq, "^(1+|2+|3+)$"), "Mêmes clusters", "Clusters différents")) %>% 
  count(seq)
# Pas l'air d'y avoir des clusters qui sont différents parmi les réplicats

# Représentation
kmeans_3 <- replication_kmeans(data = les_inconnus, nb_clust = 3, n_rep = 25, result = "mode")
class(kmeans_3) <- "kmeans"
plot_ind3 <- fviz_cluster(kmeans_3, les_inconnus, ellipse.type = "convex",
                          geom = "point", ggtheme = theme_light()) +
  labs(title = "Projection des individus sur les 2 1<sup>ers</sup> axes de l'ACP",
       subtitle = "Colorés selon les groupes faits par méthode K-means") +
  theme(plot.title = element_markdown(size = 10),
        plot.subtitle = element_text(size = 7),
        legend.position = "none")
plot_ind3 + plot_axes

## Modèle de mélanges Gaussiens

#### Partie classification supervisée ----

## Import de la base

dix_pour_cent <- read_csv("https://archive.ics.uci.edu/ml/machine-learning-databases/00471/Data_for_UCI_named.csv") %>% 
  select(-p1, -stab)
sum(is.na(dix_pour_cent)) # Pas de données manquantes
glimpse(dix_pour_cent)

