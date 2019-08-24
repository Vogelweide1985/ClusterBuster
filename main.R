library(plotly)

source("exploration_funs.R")

df <- iris

v <- df$Sepal.Length
plot_univar_metric(df$Sepal.Length)
plot_univar_metric(df$Sepal.Width)
plot_univar_metric(df$Petal.Length)
plot_univar_metric(df$Petal.Width)



iplot_univar_metric(df$Sepal.Length)
iplot_univar_metric(df$Sepal.Width)
iplot_univar_metric(df$Petal.Length)
iplot_univar_metric(df$Petal.Width)

