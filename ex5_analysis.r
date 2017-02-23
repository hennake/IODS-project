# Henna Kettunen
# 2017-02-23
# Exercise 5: analysis


# Load human data
library(dplyr)
library(corrplot)
library(tidyr)
load("./data/human.RData")


# Structure and dimensions
str(human4)


# Variable summaries and graphical overview
summary(human4)
library(GGally)
library(ggplot2)
p <- ggpairs(human4, mapping = aes(alpha = 0.3), lower = list(combo = wrap("facethist", bins = 20)))
p


# PCA on unstandardized data
pca_human <- prcomp(human4)
pca_human
summary(pca_human)
biplot(pca_human, choices = 1:2, cex = c(0.6, 1), col = c("grey40", "deeppink2"))


# Standardize and repeat
human_std <- scale(human4)
summary(human_std)
pca_human_std <- prcomp(human_std)
pca_human_std
summary(pca_human_std)
biplot(pca_human_std, choices = 1:2, cex = c(0.6, 1), col = c("grey40", "darkviolet"))


# Load tea dataset
library(FactoMineR)
data(tea)
summary(tea)


# MCA
res.mca <- MCA(tea,quanti.sup=19,quali.sup=20:36,graph=FALSE)
plot(res.mca, invisible = c("var"))
plot(res.mca, invisible = c("ind"), habillage="quali")
summary(res.mca)



