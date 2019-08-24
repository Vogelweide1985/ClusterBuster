library(plotly)

source("exploration_funs.R")


df <- iris
x <- df$Sepal.Length
y <- df$Sepal.Width
z <- df$Species

plot_univar_metric(df$Sepal.Length)
plot_univar_metric(df$Sepal.Width)
plot_univar_metric(df$Petal.Length)
plot_univar_metric(df$Petal.Width)

iplot_univar_metric(df$Sepal.Length)
iplot_univar_metric(df$Sepal.Width)
iplot_univar_metric(df$Petal.Length)
iplot_univar_metric(df$Petal.Width)

iplot_bivar_metric_metric(df$Sepal.Length, df$Sepal.Width)
iplot_bivar_metric_metric(df$Sepal.Length, df$Petal.Length)
iplot_bivar_metric_metric(df$Sepal.Length, df$Petal.Width)
iplot_bivar_metric_metric(df$Sepal.Width, df$Petal.Length)
iplot_bivar_metric_metric(df$Sepal.Width, df$Petal.Width)
iplot_bivar_metric_metric(df$Petal.Length, df$Petal.Width)


iplot_bivar_metric_metric(x, y)
iplot_bivar_metric_metric(x, y)



