# --------------------------------- #
# Devoir UE Classification SMSDS    #
# Benoit Gachet et Guillaume Mulier #
# 29/11/2020                        #
# --------------------------------- #

#### Packages ----

library(conflicted)
library(tidyverse)
library(patchwork)
library(ggtext)
library(FactoMineR)
library(factoextra)
library(dendextend)
library(purrr)
library(scales)
library(mclust)
library(cowplot)
library(ggpubr)

theme_set(theme_light() +
            theme(plot.title = element_markdown(),
                  strip.background = element_blank(),
                  strip.text = element_textbox(
                    size = 12, 
                    color = "white", fill = "#7888C0", box.color = "#000066",
                    halign = 0.5, linetype = 1, r = unit(3, "pt"), width = unit(0.75, "npc"),
                    padding = margin(2, 0, 1, 0), margin = margin(3, 3, 3, 3))))

conflict_prefer(name = "filter", winner = "dplyr")
conflict_prefer(name = "summarise", winner = "dplyr")
conflict_prefer(name = "map", winner = "purrr")
conflict_prefer(name = "get_legend", winner = "cowplot")

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

les_inconnus_réduits <- les_inconnus %>% 
  mutate(across(everything(), .fns = ~ (.x - mean(.x)) / sd(.x)))
# Variables centrées réduites

## ACP

ACP_inc <- PCA(les_inconnus_réduits, scale.unit = TRUE, graph = FALSE)
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

set.seed(7)

# Dendrogramme en 3 classes
dendro <- les_inconnus_réduits %>% 
  dist(method = "euclidean") %>% 
  hclust(method = "ward.D2") # Distance euclidienne avec aggrégation pour clusters sphériques
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
  geom_segment(aes(x = 168, xend = 168, y = 25, yend = 0), linetype = "dashed", color = "purple3") +
  geom_segment(aes(x = 324, xend = 324, y = 25, yend = 0), linetype = "dashed", color = "purple3")
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

replication_kmeans <- function(data, n_rep, nb_clust, result = "modele", nstart) {
  
  result <- match.arg(result, c("modele", "liste"))
  
  kmeans_rep <- replicate(n_rep, kmeans(data, centers = nb_clust, nstart = nstart))
  best <- which.min(unlist(kmeans_rep['tot.withinss', ]))
  kmeans <- kmeans_rep[, best]
  
  if (result == "modele") {
    return(kmeans)
  } else if (result == "liste") {
    return(kmeans_rep)
  }
}

set.seed(7)

modeles <- vector(mode = "list", length = 10)
for (i in 1:10) {
  cat(i, "\n")
  modeles[[i]] <- replication_kmeans(data = les_inconnus_réduits,
                                     nb_clust = i, n_rep = 25, nstart = 25)
}
plot_inertie <- map_dfr(modeles, ~ c(intra = .x[["tot.withinss"]]), .id = "k") %>% 
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
# Là encore, il y a un saut dans les inerties pour k = 3 environ
plot_calinski <- map_dfr(modeles, ~ c(intra = .x[["tot.withinss"]], inter = .x[["betweenss"]]), .id = "k") %>% 
  mutate(k = as.numeric(k),
         calinski = ifelse(k == 1, 0, ((500 - k) * inter) / ((k - 1) * intra)),
         color = c(2, 2, 1, rep(2, 7))) %>% 
  ggplot(aes(x = k, y = calinski)) +
  geom_point(aes(color = as.factor(color)), show.legend = FALSE) +
  geom_line() +
  geom_vline(xintercept = 3, color = "blue", linetype = "dashed") +
  scale_color_manual(values = c("blue", "black")) +
  scale_x_continuous(breaks = 1:10) +
  labs(x = "Nombre de clusters",
       y = "Indice de Calinski-Harabasz",
       title = "Evolution de l'indice de Calinski-Harabasz en fonction du nombre de clusters faits") +
  theme(panel.grid.minor.x = element_blank())
# k = 3 maximise l'indice de calinski
plot_inertie + plot_calinski

# Choix de k = 3
kmeans_replicats <- replication_kmeans(data = les_inconnus_réduits, nb_clust = 3, n_rep = 25, result = "liste", nstart = 1)
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
  scale_color_discrete(name = "Clusters :", labels = c("1", "2", "3")) +
  coord_cartesian(ylim = c(140, 180)) +
  labs(x = "Numéro de réplication",
       y = "Effectif",
       title = "Vérification de la taille des clusters en réplicant 25 fois les K-means")
# Les effectifs dans les clusters sont relativement stables et équilibrés

# Décompte des clusters, ne marche pas pour les ex aequos
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
  map_dfr(~ c(ind = .x)) %>% 
  t() %>% 
  as.data.frame() %>% 
  apply(1, function(x) {paste(x, collapse = "")}) %>% 
  data.frame(seq = .)
count(liste_clusters, seq, sort = TRUE)
liste_clusters %>% 
  mutate(seq = ifelse(str_detect(seq, "^(1+|2+|3+)$"), "Mêmes clusters", "Clusters différents")) %>% 
  count(seq) %>%  
  mutate(prop = paste0(100 * round(n / sum(n), 1), "%"))
# Pas l'air d'y avoir des clusters qui sont différents parmi les réplicats

# Matrice de similarité
n <- 500
mat <-  matrix(0, nrow = n, ncol = n) # matrice carrée de taille n de 0
clusters <- kmeans_replicats[1, ] %>% 
  map_dfr(~ c(ind = .x)) %>% 
  t() %>% 
  as.data.frame()
clusters <- clusters[order(clusters$V1), ]
# Tri du tableau selon le 1er vecteur de clusters
for (r in seq_len(ncol(clusters))) {
  classe <- clusters[, r]
  for (i in 1:(n-1)) {
    for(j in (i + 1):n) {
      if (classe[i] == classe[j]) mat[i, j] <- mat[i, j] + 1
    }
  }
}
(mat / ncol(clusters)) %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  pivot_longer(-rowname) %>% 
  mutate(rowname = as.numeric(rowname),
         name = as.numeric(str_remove(name, "^V"))) %>% 
  ggplot(aes(rowname, name, fill = value)) +
  geom_tile() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradient2(low = "white", mid = "green", high = "blue", 
                       midpoint = 0.5, labels = percent_format(),
                       name = NULL) +
  labs(x = NULL,
       y = NULL) +
  annotate(geom = "richtext", x = 250, y = 200, size = 3, hjust = 0.2,
           label = "**Visualisation des individus triés selon la <br> 1<sup>ère</sup> réplication de K-means :<br>Pourcentage de fois où les 2 individus sont <br>dans le même cluster sur les 25 réplicats**")

# Représentation
kmeans_3 <- replication_kmeans(data = les_inconnus_réduits, nb_clust = 3, n_rep = 25, result = "mode", nstart = 25)
class(kmeans_3) <- "kmeans"
plot_ind3 <- fviz_cluster(kmeans_3, les_inconnus_réduits, ellipse.type = "convex",
                          geom = "point", ggtheme = theme_light()) +
  labs(title = "Projection des individus sur les 2 1<sup>ers</sup> axes de l'ACP",
       subtitle = "Colorés selon les groupes faits par méthode K-means") +
  theme(plot.title = element_markdown(size = 10),
        plot.subtitle = element_text(size = 7),
        legend.position = "none")
# Pour une raison inconnue, fviz_cluster inverse les coordonnées sur le 1er axes principal
plot_ind3 <- plot_ind3 +
  scale_x_reverse(labels = function(x) -x)
plot_ind3 + plot_axes 

les_inconnus_shopenhauer$class_kmeans <- kmeans_3$cluster

## Modèle de mélanges Gaussiens

set.seed(7)

# A la vue de l'ACP, choix de modèle Gaussiens sphériques (et donc VII/EII pour l'opposer aux K-means)
boules_gaus <- map(1:20, ~ Mclust(les_inconnus_réduits, G = .x, modelNames = "VII"))
boules_gaus %>% 
  map_dfr(~ c(BIC = as.numeric(.x[["bic"]]), ICL = as.numeric(.x[["icl"]])), .id = "k") %>% 
  mutate(k = as.numeric(k)) %>% 
  pivot_longer(-k) %>% 
  ggplot(aes(x = k, y = value)) +
    geom_point() +
    geom_line() +
    facet_wrap(vars(name), nrow = 1)
# Sur les critères BIC et ICL, il n'y a pas l'air d'y avoir de maximum
# Sur les bases des classifications précédantes et le coude dans les valeurs, on essaiera k = 3 classes

# k = 3
boules_gaus3 <- boules_gaus[[3]]
plot_ind_gaus3 <- fviz_cluster(boules_gaus3, les_inconnus_réduits, ellipse.type = "norm",
                               geom = "point", ggtheme = theme_light()) +
  labs(title = "Projection des individus sur les 2 1<sup>ers</sup> axes de l'ACP",
       subtitle = "Colorés selon les groupes faits par méthode des mélanges Gaussiens",
       caption = "Ellipse de concentration selon la loi normale") +
  theme(plot.title = element_markdown(size = 10),
        plot.subtitle = element_text(size = 7),
        legend.position = "none")
plot_ind_gaus3 <- plot_ind_gaus3 +
  scale_x_reverse(labels = function(x) -x)
plot_ind_gaus3 + plot_axes 

les_inconnus_shopenhauer$class_gaus <- boules_gaus3$classification
table(les_inconnus_shopenhauer$class_gaus, les_inconnus_shopenhauer$class_cah, deparse.level = 2, useNA = "ifany")
table(les_inconnus_shopenhauer$class_kmeans, les_inconnus_shopenhauer$class_cah, deparse.level = 2, useNA = "ifany")
les_inconnus_shopenhauer$class_kmeans <- factor(les_inconnus_shopenhauer$class_kmeans,
                                                levels = c(1, 2, 3), labels = c(2, 1, 3)) %>% 
  as.character() %>% as.numeric()
# Recodage des clusters pour qu'ils représentent la même chose
plot_diff <- les_inconnus_shopenhauer %>% 
  select(starts_with("class")) %>% 
  arrange(class_cah) %>% 
  rownames_to_column() %>% 
  mutate(rowname = as.numeric(rowname)) %>% 
  pivot_longer(-rowname) %>% 
  mutate(name = fct_recode(name,
                           "CAH" = "class_cah",
                           "Mélanges de\nGaussiennes" = "class_gaus",
                           "K-means" = "class_kmeans")) %>% 
  ggplot(aes(x = name, y = rowname, fill = as.factor(value))) +
  scale_x_discrete(name = "Technique de classification non supervisée", expand = c(0, 0)) +
  scale_y_continuous(name = "Nombre d'individus", expand = c(0, 0)) +
  scale_fill_discrete(name = "Cluster n°") +
  geom_tile()

# Graphe de conclusions du non supervisé

(plot_ind2 + plot_ind3) / (plot_ind_gaus3 + plot_diff)

#### Partie classification supervisée ----

## Import de la base

dix_pour_cent <- read_csv("https://archive.ics.uci.edu/ml/machine-learning-databases/00471/Data_for_UCI_named.csv") %>% 
  select(-p1, -stab)
set.seed(7)
dix_pour_cent <- dix_pour_cent[sample(1:nrow(dix_pour_cent), size = 1000), ]
sum(is.na(dix_pour_cent)) # Pas de données manquantes
glimpse(dix_pour_cent)

## Description de la base

# Ordres de grandeur

dix_pour_cent %>% 
  summarise(across(tau1:g4, .fns = list(moyenne = mean, ecart_type = sd, mini = min, maxi = max))) %>% 
  pivot_longer(everything(), names_to = c("Variable", "stat"), names_pattern = "^([a-z]*[0-9])_(.*)") %>% 
  pivot_wider(names_from = "stat")
# Pas les mêmes unités de variables, décision de centrer et réduire les variables

dix_pour_cent_reduits <- dix_pour_cent %>% 
  mutate(across(tau1:g4, .fns = ~ (.x - mean(.x)) / sd(.x)),
         stabf = factor(stabf, levels = c("stable", "unstable"), labels = c("Stable", "Instable")))

# Density plot

helper <- tibble(noms = names(dix_pour_cent)[-12],
                 titres = c("&tau;<sub>1</sub>", "&tau;<sub>2</sub>", "&tau;<sub>3</sub>", "&tau;<sub>4</sub>", 
                            "P<sub>1</sub>", "P<sub>2</sub>", "P<sub>3</sub>",
                            "G<sub>1</sub>", "G<sub>2</sub>", "G<sub>3</sub>", "G<sub>4</sub>"))

density_plot <- function(var, data, titre){
  ggplot(data = data, aes(x = .data[[var]], colour = stabf, fill = stabf))+
    geom_density(alpha = 0.3) +
    geom_vline(aes(xintercept = mean(.data[[var]])), linetype = "dashed", color = "darkgrey", size = 1) +
    labs(x = titre[["titres"]][match(var, titre[["noms"]])],
         y = "Densité") +  
    scale_fill_discrete(name = "Stabilité du système") +
    scale_color_discrete(name = "Stabilité du système") +
    theme(axis.title.x = element_markdown())
}
legende <- get_legend(density_plot(var = "tau1", data = dix_pour_cent_reduits, titre = helper))
liste_plot <- helper$noms %>% 
  map(~ density_plot(var = .x, data = dix_pour_cent_reduits, titre = helper) +
        theme(legend.position = "none"))
liste_plot[[12]] <- legende
ggarrange(plotlist = liste_plot, nrow = 3, ncol = 4)

# ACP

ACP_elec <- PCA(dix_pour_cent_reduits, scale.unit = TRUE, graph = FALSE, quali.sup = 12, ncp = 11)
fviz_screeplot(ACP_elec, addlabels = TRUE, ncp = 11)
# Il y a au total 11 compsantes principales qui expliquent tous environ la même part de variance
plot_indACP <- function(acp, axes, couleur = "none") {
  plot_axes <- fviz_pca_var(acp, repel = TRUE, axes = axes) +
    labs(title = paste0("Cercle des corrélations des variables aux axes ", paste(axes, collapse = " et "))) +
    theme(plot.title = element_markdown(size = 10))
  plot_ind <- fviz_pca_ind(acp, geom.ind = "point", axes = axes, habillage = couleur) +
    labs(title = paste0("Projection des individus sur les axes ", paste(axes, collapse = " et "), " de l'ACP")) +
    theme(plot.title = element_markdown(size = 10),
          legend.position = "bottom") +
    scale_color_discrete(name = "Stabilité") +
    guides(shape = FALSE,
           color = guide_legend(override.aes = list(shape = c(19, 17))))
  plot_ind + plot_axes
}
plot_indACP(acp = ACP_elec, axes = c(1, 2), couleur = 12)
plot_indACP(acp = ACP_elec, axes = c(3, 4), couleur = 12)
plot_indACP(acp = ACP_elec, axes = c(5, 6), couleur = 12)
# en observant les 2 groupes sur les 6 1ers axes principaux, on peut voir qu'ils ont l'air difficiles à individualiser
# Globalement, ils ont une forme sphérique, un volume assez équivalent (sur la projection en tout cas)

### Classisification supervisée

## Création des jeux d'entraînement/test et des échantillons pour cross-validation

set.seed(7)
num_test <- sample(1000, size = 700, replace = FALSE)
ordre_cv <- sample(1000, size = 1000, replace = FALSE)

# Echantillon de CV
dix_pour_cent_CV <- cbind(dix_pour_cent_reduits[ordre_cv, ], ech = rep(1:10, each = 1000))

# Echantillons test et d'apprentissage
dix_pour_cent_app <- dix_pour_cent_reduits[num_test, ]
dix_pour_cent_test <- dix_pour_cent_reduits[-num_test, ]

## Analyse factorielle discriminante

# Modèle avec même matrice de variance dans les 2 classes
AFD1 <- MclustDA(dix_pour_cent_app[,-12], class = dix_pour_cent_app$stabf, modelType = "EDDA", modelNames = c("EII", "VII"))
AFD1$models
# Modèle EII choisi car BIC plus grand

AFD2 <- MclustDA(dix_pour_cent_app[,-12], class = dix_pour_cent_app$stabf, modelType = "MclustDA", modelNames = c("EII", "VII"))
# Choix modèle EII :
mod_stable <- which.max(AFD2$models$Stable$BIC[, 1])
mod_instable <- which.max(AFD2$models$Instable$BIC[, 1])
AFD2$models$Stable$BIC[mod_stable, 1] + AFD2$models$Instable$BIC[mod_instable, 1]

# Choix du modèle VII :
mod_stable2 <- which.max(AFD2$models$Stable$BIC[, 2])
mod_instable2 <- which.max(AFD2$models$Instable$BIC[, 2])
AFD2$models$Stable$BIC[mod_stable2, 2] + AFD2$models$Instable$BIC[mod_instable2, 2]
# Equivalence des EII et VII donc choix EII pour diminuer le nombre de paramètres

AFD1 <- MclustDA(dix_pour_cent_app[,-12], class = dix_pour_cent_app$stabf, modelType = "EDDA", modelNames = "EII")
AFD2 <- MclustDA(dix_pour_cent_app[,-12], class = dix_pour_cent_app$stabf, modelType = "MclustDA", modelNames = "EII")
# Validation sur jeu test
summary(AFD1, newdata = dix_pour_cent_test[, -12], newclass = dix_pour_cent_test$stabf) # Risque = 17.33%
summary(AFD2, newdata = dix_pour_cent_test[, -12], newclass = dix_pour_cent_test$stabf) # Risque = 17.33%
# Même risque pour les 2 mais pas même erreur

# Cross-validation sur jeu complet
erreur_AFD <- function(echantillon) {
  test <- dix_pour_cent_CV %>% filter(ech == echantillon)
  train <- dix_pour_cent_CV %>% filter(ech != echantillon)
  AFD1 <- MclustDA(train[,-12], class = train$stabf, modelType = "EDDA", modelNames = "EII")
  err1 <- summary(AFD1, newdata = test[, -12], newclass = test$stabf)$err.newdata
  AFD2 <- MclustDA(train[,-12], class = train$stabf, modelType = "MclustDA", modelNames = "EII")
  err2 <- summary(AFD2, newdata = test[, -12], newclass = test$stabf)$err.newdata
  return(c(edda = err1, MclustDA = err2))
}
cross_val <- map_dfr(1:10, ~ erreur_AFD(.x), .id = "jeu")
cross_val %>% 
  summarise(across(edda:MclustDA, .fns = ~ mean(.x)))
# Pour cross-validation, MclustDA (variances différentes dans les groupes meilleur)
