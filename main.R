library(plotly)
library(corrplot)
source("exploration_funs.R")


df <- iris



model <- kmeans(df[1:4], 3, 20 , 5)
table(df$Species, model$cluster)

plot_ly(x = df$Sepal.Length, y = df$Sepal.Width, z= df$Petal.Length, color= model$cluster, type = "scatter3d", mode = "markers") 


