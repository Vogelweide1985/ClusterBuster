
plot_univar_metric <- function(v) {
      par(mfrow=c(2,2))
      hist(v)
      plot(v)
      boxplot(v)
      #plot(ecdf(v))
      qqnorm(v)
      qqline(v)
      
}

iplot_univar_metric <- function(x) {
      p1 <- plot_ly(x = ~x, type = "scatter", mode = "markers")
      p2 <- plot_ly(x = ~x, type = "histogram") 
      p3 <- plot_ly(y = ~x, type = "box")
      p4 <- plot_ly(y = ~x, type = "violin")
      
      subplot(p1, p2, p3, p4, nrows = 2)
      
}

iplot_bivar_metric_metric <- function(x, y) {
   #TO DO:  p4
   p1 <- plot_ly(x = ~x, y = ~y, type = 'scatter', mode = "markers")
   p2 <- plot_ly(x = x, type = "histogram") %>%
      add_histogram(x = y) %>%
      layout(barmode = "overlay")
   p3 <- plot_ly(x= x, y = y, type = "histogram2dcontour")
   p4 <- plot_ly(y = x, type = "violin")
   
   subplot(p1, p2, p3, p4, nrows = 2)
   
}