
plot_univar_metric <- function(v) {
      par(mfrow=c(2,2))
      hist(v)
      plot(v)
      boxplot(v)
      #plot(ecdf(v))
      qqnorm(v)
      qqline(v)
      
}

iplot_univar_metric <- function(v) {
      p1 <- plot_ly(x = v, type = "scatter")
      p2 <- plot_ly(x = v, type = "box")
      p3 <- plot_ly(x = v, type = "violin")
      p4 <- plot_ly(x = v, type = "histogram")
      
      subplot(p1, p2, p3, p4, nrows = 2)
      
}