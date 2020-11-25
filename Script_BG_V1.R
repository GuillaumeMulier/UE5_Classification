
### Appliquer les methodes vues en cours. ---
# 1. Classification non supervisée (CAH, Kmeans, modèles de mélanges Gaussiens)
# 2. Classification supervisée (AFD, KNN, arbres, random forest)
### Explorer et de comparer les méthodes :
# 1.Tester la robustesse d'une classification en fonction du nombre de classes.
# 2. Comparer les partitions obtenues / les performances de classification avec les différentes méthodes. Validation croisée.
# 3.Mesurer l'erreur de classification, le risque

library(data.table)
library(mclust)
library(fastcluster)
library(FactoMineR)
library(ggplot2)
library(devtools)
library(JLutils)
library(ggpubr)
library(factoextra)
library(cluster)


seed <- 7
set.seed(seed)

## Données non supervisées -------------------------
data_NS <- read.table("D:/Benoît/Desktop/Cours master 2/5_Classification/Devoir/DonneesNSGpe.txt", header = TRUE)
data_NS <- data_NS[,-1]
# Premiere ACP pour obtenir une representation "resumee" des donnees
PCA_NS <- PCA(data_NS, scale.unit = FALSE, graph = FALSE)
PPCA <- plot.PCA(data_NS, choix = "var", label = "none", axes = c(1, 2)) +
  ggtitle(paste("Representation des individus du groupe", seed))


# Pourcentage de la variance expliquée par les dimensions
eigenvalue <- round(get_eigenvalue(res.pca), 1)
variance.percent <- eigenvalue$variance.percent
head(eigenvalue)

ggscatter(
  ind.coord, x = "Dim.1", y = "Dim.2",
  xlab = paste0("Dim 1 (", variance.percent[1], "% )" ),
  ylab = paste0("Dim 2 (", variance.percent[2], "% )" )
)

## -------------------------- ##

# CAH - classification hiérarchique ascendante 
X <- data_NS               # une matrice n x p
D <- dist(X)     # la matrice des distances entre individus de X (défaut distance Euclidienne)
arbre <- hclust(D)   # le dendrogramme 
plot(arbre, labels = FALSE, main = "Dendogram")         # affiche le dendrogrammme
# Pour obtenir une partition de la population, il suffit de découper le dendrogramme obtenu à une certaine hauteur. 
# En premier lieu, une analyse de la forme du dendrogramme pourra nous donner une indication sur le nombre de classes à retenir. Dans notre devoir quatres branches bien distinctes apparaissent sur l’arbre.
# Pour nous aider, nous pouvons représenter les sauts d’inertie du dendrogramme selon le nombre de classes retenues
inertie <- sort(arbre$height, decreasing = TRUE)
plot(inertie[1:20], type = "s", xlab = "Nombre de classes", ylab = "Inertie")
# On voit trois sauts assez nets à 2, 3 et 4 classes, que nous avons représentés ci-dessous respectivement en vert, en rouge et en bleu
plot(inertie[1:20], type = "s", xlab = "Nombre de classes", ylab = "Inertie")
points(c(2, 3, 4, 5), inertie[c(2, 3, 4, 5)], col = c("green3", "red3", "blue3","purple3"), cex = 2, lwd = 3)
# La fonction rect.hclust permet de visualiser les différentes partitions directement sur le dendrogramme.
plot(arbre, labels = FALSE, main = "Partition en 2, 3, 4 ou 5 classes", xlab = "", ylab = "", sub = "", axes = FALSE, hang = -1)
rect.hclust(arbre, 2, border = "green3")
rect.hclust(arbre, 3, border = "red3")
rect.hclust(arbre, 4, border = "blue3")
rect.hclust(arbre, 5, border = "purple3")

install_github("larmarange/JLutils")
best.cutree(arbre)
# On peut également représenter le graphique des pertes relatives d’inertie avec graph=TRUE. La meilleure partition selon ce critère est représentée par un point noir et la seconde par un point gris.
best.cutree(arbre, min = 2, graph = TRUE, xlab = "Nombre de classes", ylab = "Inertie relative")
# Un découpage en deux classes minimise ce critère. Cependant, si l’on souhaite réaliser une analyse un peu plus fine, un nombre de classes plus élevé serait pertinent. Nous allons donc retenir un découpage en trois classes. Le découpage s’effectue avec la fonction cutree.
typo <- cutree(arbre, 4)
table(typo)
A2Rplot(arbre, k = 4, boxes = FALSE, col.up = "gray50", col.down = brewer.pal(4, "Dark2"), show.labels = FALSE)


# Kmean

# Elbow Method
intra <- rep(NA, l = 15)
intra[1] <- kmeans(data_NS,1)$tot.withinss # inertie intra k=1
for (k in 2:15) intra[k] <- min(replicate(20,kmeans(data_NS,k)$tot.withinss))

PKmeansK <- ggplot(cbind.data.frame(K = 1:15, Intra = intra), aes(K, intra)) +
  geom_point() +
  ggtitle(paste0("Inertie Intra estimée avec 20 replicats - Groupe ")) +
  theme_bw()
PKmeansK
fviz_nbclust(data_NS, kmeans, method = "wss")
# Average Silhouette Method
fviz_nbclust(data_NS, kmeans, method = "silhouette")
# Gap statistique
gap_stat <- clusGap(data_NS, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

# Qu'est ce que cela veut dire
kmeans.rep <- replicate(20, kmeans(data_NS, 5))
best <- which.min(unlist(kmeans.rep['tot.withinss', ]))
resKmeans <- kmeans.rep[, best]
resKmeans

# représentation graphique
res.km <- kmeans(data_NS, 5, nstart = 25)
res.km
fviz_cluster(res.km, data = data_NS,
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

# Réduction de dimension en utilisant l'ACP
res.pca <- prcomp(data_NS,  scale = TRUE)
ind.coord <- as.data.frame(get_pca_ind(res.pca)$coord) # Coordonnées des individus

ind.coord$cluster <- factor(res.km$cluster) # Ajouter les clusters obtenus à l'aide de l'algorithme k-means
head(ind.coord) # Inspection des données
ggscatter(
  ind.coord, x = "Dim.1", y = "Dim.2", 
  color = "cluster", palette = "npg", ellipse = TRUE, ellipse.type = "convex",
  size = 1.5,  legend = "right", ggtheme = theme_bw(),
  xlab = paste0("Dim 1 (", variance.percent[1], "% )" ),
  ylab = paste0("Dim 2 (", variance.percent[2], "% )" )
) +
  stat_mean(aes(color = cluster), size = 4)



# modèle des mélanges Gaussiens
MG <- Mclust(data_NS)
MG
summary(MG)
summary(MG$BIC)
summary(MG$icl)
mclustICL(data_NS)


plot(MG$BIC)
plot(MG, what = "BIC", ylim = range(MG$BIC[,-(1:5)], na.rm = TRUE),
     legendArgs = list(x = "topright"))

MG_EEV <- Mclust(data_NS, modelNames = "EEV")
MG_VEV <- Mclust(data_NS, modelNames = "VEV")
MG_EVV <- Mclust(data_NS, modelNames = "EVV")

summary(MG_EEV)
summary(MG_VEV)
summary(MG_EVV)

plot(MG_EEV, what = "BIC")
plot(MG_VEV, what = "BIC")
plot(MG_EVV, what = "BIC")



# Données supervisées -------------------------------
ElecGrid <- fread("D:/Benoît/Desktop/Cours master 2/5_Classification/Devoir/Data_for_UCI_named.csv", data.table=F) 
data_S <- ElecGrid[, -grep("^p1$|^stab$", colnames(ElecGrid))]
any(is.na(data_S))
head(data_S, n = 4)
table(data_S$stabf)
# Selection de votre sous-ensemble de données

data_S7 <- data_S[sample(1:nrow(data_S), size = 1000), ]
resPCA <- PCA(data_S7, scale.unit = TRUE, graph = FALSE, quali.sup = ncol(data_S7))
plot.PCA(resPCA, choix = "ind", label = "none", axes = c(1, 2), habillage = ncol(data_S7))