library(dplyr)
library(tidyverse)
library(factoextra)
library(NbClust)
library(FactoMineR)
library(ggcorrplot)
library('corrr')
library("FactoMineR")
library(cluster)
setwd("C:\\Users\\karas\\Documents\\ES data")

library("xlsx")
# Read initial 'What' xlsx file for correlation
data <- read.xlsx(file.choose(), 1)  # read first sheet
# OR (for clustering)
data = read.csv("what_where_who_df_flood_fix.csv", header= TRUE)



# Explore data structure
str(data)
colnames(data)


# Select columns for ecosystem services
numerical_data <- data %>% select(FOOD_WATER,CLEAN_WATER,CLEAN_AIR,NOISE_RED,
                            TEMP_REG,EXTR_WEATHER,FLOOD_REG,NAT_CONNECT,RECREATION,
                            SOCIAL_BOND,RELAX,SAFETY,KNOWLEDGE,SPIRI_IDENT_SYMBOL,
                            AESTHETIC,BIODIVE,ATTACHMENT)



# Standardize the variables (prerequisite for PCA) and omit NA
data_normalized <- scale(numerical_data)
data_normalized <- na.omit(data_normalized)

#PCA

corr_matrix <- cor(data_normalized)
ggcorrplot(corr_matrix)
data.pca <- princomp(corr_matrix)
summary(data.pca)
fviz_eig(data.pca, addlabels = TRUE)
# Graph of the variables
fviz_pca_var(data.pca, col.var = "black")
fviz_cos2(data.pca, choice = "var", axes = 1)
fviz_cos2(data.pca, choice = "var", axes = 2)
write.csv(corr_matrix,"corr_matrix.csv", row.names = TRUE)



# Compute correlation matrix
corr <- round(cor(numerical_data,method=c("spearman"), use="complete.obs"), 1)
head(corr[, 1:17])

# Compute a matrix of correlation p-values
p.mat <- cor_pmat(numerical_data,method=c("spearman"), use="complete.obs")
head(p.mat[, 1:17])

# Visualize the correlation matrix
# --------------------------------
# method = "square" (default)
ggcorrplot(corr)



# Reordering the correlation matrix
# --------------------------------
# using hierarchical clustering
ggcorrplot(corr, hc.order = TRUE, outline.col = "white")

# Types of correlogram layout
# --------------------------------
# Get the lower triangle
ggcorrplot(corr, hc.order = TRUE, type = "lower",
           outline.col = "white")

# Get the upper triangle
ggcorrplot(corr, hc.order = TRUE, type = "upper",
           outline.col = "white")

# Add correlation coefficients
# --------------------------------
# argument lab = TRUE
ggcorrplot(corr, hc.order = TRUE, type = "upper",
           lab = TRUE, p.mat = p.mat)




var_coord <- data.frame(var$coord)
var_cor <- data.frame(var$cor)
var_cos2 <- data.frame(var$cos2)



# Elbow method
fviz_nbclust(var_coord, kmeans, method = "wss") +
  geom_vline(xintercept = 5, linetype = 2)+
  labs(title = "A", subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(var_coord, kmeans, method = "silhouette")+
  labs(title = "B",subtitle = "Silhouette method")

res.dist <- dist(var_coord, method = "euclidean")
res.hc <- hclust(d = res.dist, method = "ward.D2")
fviz_dend(res.hc, cex = 0.5)

# Compute cophentic distance
res.coph <- cophenetic(res.hc)
res.hc2 <- hclust(res.dist, method = "average")
cor(res.dist, res.coph)
cor(res.dist, cophenetic(res.hc2))
# Cut tree into 4 groups
grp <- cutree(res.hc, k = 5)
head(grp, n = 5)
# Cut in 5 groups and color by groups
fviz_dend(res.hc, k = 5, # Cut in four groups
          cex = 0.8, # label size
          horiz = TRUE,
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07","#7570b3"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)

fviz_cluster(list(data = var_coord, cluster = grp),
             k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07","#7570b3"),
             ellipse.type = "convex", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             show.clust.cent = FALSE, ggtheme = theme_minimal())



km.res <- kmeans(var_coord, 5, nstart = 25)
library("factoextra")
fviz_cluster(km.res, data = var_coord,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07","#7570b3"),
             ggtheme = theme_minimal(),
             main = "Partitioning Clustering Plot"
)

res.dist <- dist(var_coord, method = "euclidean")
res.hc <- hclust(d = res.dist, method = "ward")
fviz_dend(res.hc, cex = 0.5)

write.csv(var_coord,"var_coord.csv", row.names = TRUE)
# Compute hierarchical clustering and cut into 5 clusters
res <- hcut(res.dist, k = 5, stand = FALSE,
            hc_func = "agnes",
            hc_method = "ward.D2",
            hc_metric = "euclidean")
# Visualize
fviz_dend(res, rect = TRUE, cex = 0.6,
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07","#7570b3"))

fviz_silhouette(res, k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07","#7570b3"))
