library(plotly)
library(corrplot)
source("exploration_funs.R")


df <- iris


#### All about metrics  ####
#1-dim metric
iplot_metric(df$Sepal.Length)
iplot_metric(df$Sepal.Width)
iplot_metric(df$Petal.Length)
iplot_metric(df$Petal.Width)

#2-dim metric
iplot_metric_metric(df$Sepal.Length, df$Sepal.Width)
iplot_metric_metric(df$Sepal.Length, df$Petal.Length)
iplot_metric_metric(df$Sepal.Length, df$Petal.Width)
iplot_metric_metric(df$Sepal.Width, df$Petal.Length)
iplot_metric_metric(df$Sepal.Width, df$Petal.Width)
iplot_metric_metric(df$Petal.Length, df$Petal.Width)


#3dim and multimetric
iplot_metric_mult()
pairs(df) # all
plot_ly(x = df$Sepal.Length, y = df$Sepal.Width, z= df$Petal.Length, color= df$Species, type = "scatter3d", mode = "markers") 
corrplot(cor(df[1:4]), type = "upper") # metric only






