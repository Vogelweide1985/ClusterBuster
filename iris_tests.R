library(plotly)
library(corrplot)
library(factoextra)
source("exploration_funs.R")


#Online Resorurces
"https://www.datanovia.com/en/blog/types-of-clustering-methods-overview-and-quick-start-r-code/"
"https://uc-r.github.io/kmeans_clustering"
"http://www.sthda.com/english/wiki/factoextra-r-package-easy-multivariate-data-analyses-and-elegant-visualization"
"https://stats.stackexchange.com/questions/81481/why-does-k-means-clustering-algorithm-use-only-euclidean-distance-metric"
#youtube
"https://www.youtube.com/watch?v=mtkWR8sx0NA"
"https://www.youtube.com/watch?v=P2KZisgs4A4&t=460s" # Read comments, he made some mistakes

df <- iris

# Simple k-means
model <- kmeans(df[1:4], 3, 20 , 5)
table(df$Species, model$cluster)
plot_ly(x = df$Sepal.Length, y = df$Sepal.Width, z= df$Petal.Length, color= model$cluster, type = "scatter3d", mode = "markers") 

#does Scale matter?
df_scale <- scale(df[1:4])
model_2 <- kmeans(df_scale, 3, 20 , 5)
table(df$Species, model_2$cluster)
plot_ly(x = df_scale[,1], y = df_scale[,2], z= df_scale[,3], color= model_2$cluster, type = "scatter3d", mode = "markers") 
# Don't Scale if it the same measure


#FactorExtra Package
d <- get_dist(df[,1:4])
fviz_dist(d)
model <- kmeans(df[1:4], 3, 20 , 5)
fviz_cluster(model, df[1:4])



#Get optimal Clusters:

set.seed(123)

fviz_nbclust(df[, 1:4], kmeans, method = "wss")
fviz_nbclust(df[, 1:4], kmeans, method = "silhouette")
fviz_nbclust(df[, 1:4], kmeans, method = "gap_stat")
fviz_gap_stat(df[, 1:4])


