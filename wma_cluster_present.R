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
  group_by(Marke, media_fein) %>%
  summarise(summe = sum(Euro)) %>%
  arrange(desc(summe))


# kmeans über Total summen -> kann ncihts rauskommen
df_total <- spread(df2, media_fein, summe )
df_total[is.na(df_total)] <- 0
df_total <- as.data.frame(df_total)
m_total <- as.matrix(df_total[,2:ncol(df_total)])

set.seed(123)

kmeans(m_total, centers = 3)
fviz_nbclust(m_total, kmeans, method = "wss")
fviz_nbclust(m_total, kmeans, method = "silhouette")
fviz_nbclust(m_total, kmeans, method = "gap_stat")

m1 <- kmeans(m_total, centers = 2)
m2 <- kmeans(m_total, centers = 3)
m3 <- kmeans(m_total, centers = 4)

erg_total <- cbind(df_total, m1 = m1$cluster, m2 =m2$cluster, m3 =m3$cluster)
erg_total <- left_join(erg_total, df[, c("Marke", "Gruppe", "Wirtschaftsbereich")])

#Darstellung nach Wirtschaftsbereich
t <- table(erg_total$m1, erg_total$Wirtschaftsbereich)
prop.table(t, 2)
t <- table(erg_total$m2, erg_total$Wirtschaftsbereich)
prop.table(t, 2)
t <- table(erg_total$m3, erg_total$Wirtschaftsbereich)
prop.table(t, 2)
