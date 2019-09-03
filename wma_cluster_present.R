library(plotly)
library(corrplot)
library(factoextra)
library(dplyr)
library(tidyr)
library(forcats)
source("exploration_funs.R")


df<- readRDS("wma.Rds")
df$media_fein <- ifelse(df$Medienuntergruppe == "KAUFZEITUNGEN", "KAUFZEITUNGEN", df$Mediengruppe )
df$media_fein <- ifelse(df$Medienuntergruppe == "REGIONAL ABO ZEITUNGEN", "REGIONAL ABO ZEITUNGEN", df$media_fein )
df$media_fein <- ifelse(df$Medienuntergruppe == "UEBERREGIONALE ZEITUNGEN", "UEBERREGIONALE ZEITUNGEN", df$media_fein )
df$media_fein <- ifelse(df$Medienuntergruppe == "SONNTAGSZEITUNGEN",  "Zeitung Rest", df$media_fein)
df$media_fein <- ifelse(df$Medienuntergruppe == "ZEITUNGEN SONSTIGE",  "Zeitung Rest", df$media_fein)
df$media_fein <- ifelse(df$Medienuntergruppe == "WOCHENZEITUNGEN",  "Zeitung Rest", df$media_fein)


# Filter
analyse_jahr <- 2018


df2 <- df %>%
  filter(Jahr == analyse_jahr) %>%
  filter(Marke != "KEINE ANGABE") %>%
  group_by(Marke, Mediengruppe) %>%
  summarise(summe = sum(Euro)) %>%
  arrange(desc(summe))


# kmeans über Total summen -> kann ncihts rauskommen
df_total <- spread(df2, Mediengruppe, summe )
df_total[is.na(df_total)] <- 0
df_total <- as.data.frame(df_total)
df_total$total <- rowSums(df_total[, -1])
df_total[, "DESKTOP_prz"] <- df_total[, "DESKTOP"] / df_total$total
df_total[, "FERNSEHEN_prz"] <- df_total[, "FERNSEHEN"] / df_total$total
df_total[, "PLAKAT_prz"] <- df_total[, "PLAKAT"] / df_total$total
df_total[, "PUBLIKUMSZEITSCHRIFTEN_prz"] <- df_total[, "PUBLIKUMSZEITSCHRIFTEN"] / df_total$total
df_total[, "RADIO_prz"] <- df_total[, "RADIO"] / df_total$total
df_total[, "WERBESENDUNGEN_prz"] <- df_total[, "WERBESENDUNGEN"] / df_total$total
df_total[, "ZEITUNGEN_prz"] <- df_total[, "ZEITUNGEN"] / df_total$total



m_total <- as.matrix(df_total[,2:8]) # Rechnen mit totals Spends
m_prz <- as.matrix(df_total[,10:16]) # Rechnen mit Prozenten
set.seed(123)

# Visualisierungen
colnames(df_total[,2:8])
iplot_metric(df_total[,2]) # DESKTOP
iplot_metric(df_total[,3]) # FERNSEHEN
iplot_metric(df_total[,4]) # PLAKAT
iplot_metric(df_total[,5]) # PUBLIKUMSZEITSCHRIFTEN
iplot_metric(df_total[,6]) # RADIO_prz
iplot_metric(df_total[,7]) # WERBESENDUNGEN
iplot_metric(df_total[,8]) # ZEITUNGEN

iplot_metric(df_total[,10]) # DESKTOP_prz
iplot_metric(df_total[,11]) # FERNSEHEN_prz
iplot_metric(df_total[,12]) # PLAKAT_prz
iplot_metric(df_total[,13]) # PUBLIKUMSZEITSCHRIFTEN_prz
iplot_metric(df_total[,14]) # RADIO_prz
iplot_metric(df_total[,15]) # WERBESENDUNGEN_prz
iplot_metric(df_total[,16]) # ZEITUNGEN_prz


iplot_metric_metric(df_total[,2], df_total[,3])
iplot_metric_metric(df_total[,3], df_total[,8])
iplot_metric_metric(df_total[,8], df_total[,7])
iplot_metric_metric(df_total[,8], df_total[,6])



iplot_metric_metric(df_total[,10], df_total[,11])
iplot_metric_metric(df_total[,11], df_total[,16])
iplot_metric_metric(df_total[,16], df_total[,15])
iplot_metric_metric(df_total[,16], df_total[,14])


## Modell mit total Spends
fviz_nbclust(m_total, kmeans, method = "wss")
fviz_nbclust(m_total, kmeans, method = "silhouette")
fviz_nbclust(m_total, kmeans, method = "gap_stat")

m1 <- kmeans(m_total, centers = 2)
m2 <- kmeans(m_total, centers = 3)
m3 <- kmeans(m_total, centers = 4)

erg_total <- cbind(df_total, m1 = m1$cluster, m2 =m2$cluster, m3 =m3$cluster)
erg_total <- left_join(erg_total, df[, c("Marke", "Gruppe", "Wirtschaftsbereich")])

#Darstellung nach Wirtschaftsbereich
t <- table( erg_total$Wirtschaftsbereich, erg_total$m1)
prop.table(t, 1)*100
t <- table( erg_total$Wirtschaftsbereich, erg_total$m2)
prop.table(t, 1)*100
t <- table( erg_total$Wirtschaftsbereich, erg_total$m3)
prop.table(t, 1)*100
