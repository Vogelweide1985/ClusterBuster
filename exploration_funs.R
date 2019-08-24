
plot_univar_metric <- function(v) {
      par(mfrow=c(2,2))
      hist(v)
      plot(v)
      boxplot(v)
      #plot(ecdf(v))
      qqnorm(v)
      qqline(v)
      
}
