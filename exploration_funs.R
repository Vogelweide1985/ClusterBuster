
plot_metric <- function(v) {
      par(mfrow=c(2,2))
      hist(v)
      plot(v)
      boxplot(v)
      #plot(ecdf(v))
      qqnorm(v)
      qqline(v)
      
}

iplot_metric <- function(x) {
      p1 <- plot_ly(x = ~x, type = "scatter", mode = "markers")
      p2 <- plot_ly(x = ~x, type = "histogram") 
      p3 <- plot_ly(y = ~x, type = "box")
      p4 <- plot_ly(y = ~x, type = "violin")
      
      subplot(p1, p2, p3, p4, nrows = 2)
      
}

iplot_metric_metric <- function(x, y) {
   #TO DO:  p4
   p1 <- plot_ly(x = ~x, y = ~y, type = 'scatter', mode = "markers")
   p2 <- plot_ly(x= x, y = y, type = "histogram2dcontour")
   p3 <- plot_ly(x = x, type = "histogram", alpha = 0.6) %>%
      add_histogram(x = y) %>%
      layout(barmode = "overlay")
   
   subplot(p1, p2, p3, nrows = 2)
   
}

#3dim and multimetric
#pairs(df) # all
#plot_ly(x = df$Sepal.Length, y = df$Sepal.Width, z= df$Petal.Length, color= df$Species, type = "scatter3d", mode = "markers") 
#corrplot(cor(df[1:4]), type = "upper") # metric only
