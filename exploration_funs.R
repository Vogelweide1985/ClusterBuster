library(plotly)
library(dplyr)
library(e1071)



iplot_categorial <- function(x) {
   df <- data.frame(x = x, y = rep.int(1, length(x)))
   df <- df %>% group_by(x) %>% summarise(y = sum(y))
   #plots
   p1 <- plot_ly(df, x= ~x, y=~y, type = "bar")
   p2 <- plot_ly(df, labels = ~x, values = ~y, type = 'pie',
           textposition = 'inside',
           textinfo = 'label+percent',
           insidetextfont = list(color = '#FFFFFF'),
           hoverinfo = 'text',
           text = ~y,
           marker = list(colors = colors,
                         line = list(color = '#FFFFFF', width = 1)),
           #The 'pull' attribute can also be used to create space between the sectors
           showlegend = FALSE)
   print(p1)
   print(p2)
   return(summary(x))
}


iplot_metric <- function(x) {
      l <- list()
      #Create Plots
      l[[1]] <- plot_ly(x = ~x, type = "histogram") 
      l[[2]] <- plot_ly(x = ~x[order(x, decreasing = F)], type = "scatter", mode = "lines+markers")
      l[[3]] <- plot_ly(y = ~x, type = "box" ) 
      l[[4]] <- plot_ly(y = ~x, type = "violin") 
      print(subplot(l[1:4], nrows = 2))
      #Create Stats
      l[["summary"]] <- summary(x)
      l[["quantile"]] <- quantile(x, seq(0, 1, 0.1))
      l[["quantile_extreme"]] <- quantile(x, seq(0.9, 1, 0.01))
      l[["weitere"]] <- data.frame("varianz" = var(x, na.rm = T),
                                    "standardabw." = sd(x, na.rm = T), 
                                    "var.koeff" = sd(x, na.rm = T)/ mean(x, na.ram = T),
                                    "schiefe" = skewness(x, na.rm = T), 
                                    "kurtosis"= kurtosis(x, na.rm = T))
      return(l[5:8])
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

iplot_metric_mult <- function() {
   print("SPLOM: pairs(df)")
   print("3 dimensionen: plot_ly(x = dx y = y, z= t, color= c, type = 'scatter3d', mode = 'markers') ")
   print("Corrplot nur Metrische: corrplot(cor(df[1:4]), type = 'upper') ")
}

