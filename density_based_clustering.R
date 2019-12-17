install.packages("fpc")
install.packages("dbscan")
install.packages("factoextra")

library("fpc")
library("dbscan")
library("factoextra")
library(tidyverse)
library(cluster)  
library(factoextra)
library(purrr)

# Load the data 
data("multishapes", package = "factoextra")
df <- multishapes[, 1:2]

# Applying K-Means to non convex problem
k2 <- kmeans(df, centers = 4, nstart = 25)
str(k2)
fviz_cluster(k2, data = df)

# Applying DBSCAN
# Compute DBSCAN using fpc package
library("fpc")
set.seed(123)
db <- fpc::dbscan(df, eps = 0.15, MinPts = 5)

# Plot DBSCAN results
library("factoextra")
fviz_cluster(db, data = df, stand = FALSE,
             ellipse = FALSE, show.clust.cent = FALSE,
             geom = "point",palette = "jco", ggtheme = theme_classic())

print(db)

# Cluster membership. Noise/outlier observations are coded as 0
# A random subset is shown
db$cluster[sample(1:1089, 20)]

dbscan::kNNdistplot(df, k =  5)
abline(h = 0.15, lty = 2)
