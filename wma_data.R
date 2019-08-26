library(plotly)
library(corrplot)
library(factoextra)
library(dplyr)
source("exploration_funs.R")


df<- readRDS("wma.Rds")



#### Gruppieren nach Jahr und Mediengruppe ####
df_j_m <- df %>%
  group_by(Jahr, Mediengruppe) %>%
  summarise(Summe = sum(Euro))


p <- plot_ly(df_j_m, x = ~Jahr, y = ~Summe, type = 'bar', color = ~Mediengruppe) %>%
  layout(yaxis = list(title = 'Count'), barmode = 'stack')
p
