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

#### Gruppieren nach Jahr  ####
df_j <- df %>%
  group_by(Jahr) %>%
  summarise(jahr_summe = sum(Euro))

p1 <- plot_ly(df_j, x = ~Jahr, y = ~jahr_summe, type = 'bar') 
p1

#### Gruppieren nach Jahr und media_fein ####
df_j_m <- df %>%
  group_by(Jahr, media_fein) %>%
  summarise(summe = sum(Euro))

#bar Charts
df_j_m <- left_join(df_j_m, df_j )
df_j_m$prozent <- df_j_m$summe / df_j_m$jahr_summe *100

p2 <- plot_ly(df_j_m, x = ~Jahr, y = ~summe, type = 'bar', color = ~media_fein ) %>%
  layout(yaxis = list(title = 'Investitionen'), barmode = 'stack')
p2

p3 <- plot_ly(df_j_m, x = ~Jahr, y = ~prozent, type = 'bar', color = ~media_fein ) %>%
  layout(yaxis = list(title = 'Investitionen'), barmode = 'stack')
p3

# Pie Charts
tmp  <- filter(df_j_m, Jahr == 2009)
tmp2 <- filter(df_j_m, Jahr == 2012)
tmp3 <- filter(df_j_m, Jahr == 2015)
tmp4 <- filter(df_j_m, Jahr == 2018)

p4 <- plot_ly(labels = tmp$media_fein, values = tmp$prozent, type = "pie", 
        domain = list(x = c(0, 0.4), y = c(0.6, 1)), showlegend = F) %>% 
  add_trace(labels = tmp2$media_fein, values = tmp2$prozent, type = "pie", 
            domain = list(x = c(0.6,1), y = c(0.6, 1)), showlegend = F) %>% 
  add_trace(labels = tmp3$media_fein, values = tmp3$prozent, type = "pie", 
            domain = list(x = c(0, 0.4), y = c(0, 0.4)), showlegend = F) %>% 
  add_trace(labels = tmp4$media_fein, values = tmp4$prozent, type = "pie", 
            domain = list(x = c(0.6, 1), y = c(0, 0.4)), showlegend = F) %>%
  
  layout(title = "Pie chart - subplot")
p4


# Line Plots
tmp <- filter(df, Mediengruppe == "ZEITUNGEN")
tmp2 <- tmp %>%
  group_by(Jahr, media_fein) %>%
  summarise(summe = sum(Euro))
tmp2 <- as.data.frame(tmp2)
p5 <- plot_ly(tmp2, x = ~Jahr, y = ~summe, color = ~media_fein, type = "scatter", mode = "lines+markers")

tmp3 <- tmp %>% 
  filter(Jahr == 2009) %>%
  group_by(media_fein) %>%
  summarise(start_2019 = sum(Euro))
tmp3
tmp2 <- left_join(tmp2, tmp3)  
tmp2$index <- tmp2$summe / tmp2$start_2019

p6 <- plot_ly(tmp2, x = ~Jahr, y = ~index, color = ~media_fein, type = "scatter", mode = "lines+markers")
p7 <- subplot(p5, p6, nrows = 2)
p7




#### Gruppieren nach Wirtschaftsbereich ####
analyse_jahr <- 2018
tmp <- df %>%
  filter(Jahr == analyse_jahr) %>%
  group_by(Wirtschaftsbereich) %>%
  summarise(summe = sum(Euro)) %>%
  arrange(desc(summe))
tmp$Wirtschaftsbereich <- fct_inorder(tmp$Wirtschaftsbereich)
tmp1 <- tmp
p8 <- plot_ly(tmp, x = ~Wirtschaftsbereich, y = ~summe, type = 'bar')


tmp <- df %>%
  filter(Jahr == analyse_jahr) %>%
  filter(media_fein == "REGIONAL ABO ZEITUNGEN") %>%
  group_by(Wirtschaftsbereich) %>%
  summarise(summe = sum(Euro)) %>%
  arrange(desc(summe))
tmp$Wirtschaftsbereich <- fct_inorder(tmp$Wirtschaftsbereich)
tmp2 <- tmp
p9 <- plot_ly(tmp, x = ~Wirtschaftsbereich, y = ~summe, type = 'bar')



tmp <- df %>%
  filter(Jahr == analyse_jahr) %>%
  filter(media_fein == "UEBERREGIONALE ZEITUNGEN") %>%
  group_by(Wirtschaftsbereich) %>%
  summarise(summe = sum(Euro)) %>%
  arrange(desc(summe))
tmp$Wirtschaftsbereich <- fct_inorder(tmp$Wirtschaftsbereich)
tmp3 <- tmp
p10 <- plot_ly(tmp, x = ~Wirtschaftsbereich, y = ~summe, type = 'bar')



tmp <- df %>%
  filter(Jahr == analyse_jahr) %>%
  filter(media_fein == "KAUFZEITUNGEN") %>%
  group_by(Wirtschaftsbereich) %>%
  summarise(summe = sum(Euro)) %>%
  arrange(desc(summe))
tmp$Wirtschaftsbereich <- fct_inorder(tmp$Wirtschaftsbereich)
tmp4 <- tmp
p11 <- plot_ly(tmp, x = ~Wirtschaftsbereich, y = ~summe, type = 'bar')
p12 <- subplot(p8, p9, p10, p11, nrows = 2)
p12

tmp1$summe_total <- tmp1$summe
tmp2$summe_rtz <- tmp2$summe
tmp3$summe_ureg <- tmp3$summe
tmp4$summe_kauf <- tmp4$summe

tmp5 <- left_join(tmp1, tmp2, by = "Wirtschaftsbereich")
tmp5 <- left_join(tmp5, tmp3, by = "Wirtschaftsbereich")
tmp5 <- left_join(tmp5, tmp4, by = "Wirtschaftsbereich")

tmp5$tot_prz <- tmp5$summe_total / tmp5$summe_total *100
tmp5$rtz_prz <- tmp5$summe_rtz / tmp5$summe_total*100
tmp5$ureg_prz <- tmp5$summe_ureg / tmp5$summe_total*100
tmp5$kauf_prz <- tmp5$summe_kauf / tmp5$summe_total*100

tmp5 <- filter(tmp5, Wirtschaftsbereich != "INDUSTR.VERBRAUCHS-GUETER")
tmp5$Wirtschaftsbereich <- fct_inorder(tmp$Wirtschaftsbereich)




p13 <- plot_ly(tmp5, x = ~Wirtschaftsbereich, y = ~tot_prz, type = 'bar')
p14 <- plot_ly(tmp5, x = ~Wirtschaftsbereich, y = ~rtz_prz, type = 'bar')
p15 <- plot_ly(tmp5, x = ~Wirtschaftsbereich, y = ~ureg_prz, type = 'bar')
p16 <- plot_ly(tmp5, x = ~Wirtschaftsbereich, y = ~kauf_prz, type = 'bar')
p17 <- subplot(p13, p14, p15, p16, nrows = 2)
p17

#### Gruppieren nach TOP-X Branchen ####
analyse_jahr = 2018

tmp <- df %>%
  filter(Jahr == analyse_jahr) %>%
  group_by(Gruppe) %>%
  summarise(summe = sum(Euro)) %>%
  arrange(desc(summe))
tmp$Gruppe <- fct_inorder(as.character(tmp$Gruppe))
tmp1 <- tmp[1:20, ]
p18 <- plot_ly(tmp1, x = ~Gruppe, y = ~summe, type = 'bar')


tmp <- df %>%
  filter(Jahr == analyse_jahr) %>%
  filter(media_fein == "REGIONAL ABO ZEITUNGEN") %>%
  group_by(Gruppe) %>%
  summarise(summe = sum(Euro)) %>%
  arrange(desc(summe))
tmp$Gruppe <- fct_inorder(as.character(tmp$Gruppe))
tmp2 <- tmp[1:30, ]
p19 <- plot_ly(tmp2, x = ~Gruppe, y = ~summe, type = 'bar')

tmp <- df %>%
  filter(Jahr == analyse_jahr) %>%
  filter(media_fein == "UEBERREGIONALE ZEITUNGEN") %>%
  group_by(Gruppe) %>%
  summarise(summe = sum(Euro)) %>%
  arrange(desc(summe))
tmp$Gruppe <- fct_inorder(as.character(tmp$Gruppe))
tmp3 <- tmp[1:30, ]
p20 <- plot_ly(tmp3, x = ~Gruppe, y = ~summe, type = 'bar')

tmp <- df %>%
  filter(Jahr == analyse_jahr) %>%
  filter(media_fein == "KAUFZEITUNGEN") %>%
  group_by(Gruppe) %>%
  summarise(summe = sum(Euro)) %>%
  arrange(desc(summe))
tmp$Gruppe <- fct_inorder(as.character(tmp$Gruppe))
tmp4 <- tmp[1:30, ]
p21 <- plot_ly(tmp4, x = ~Gruppe, y = ~summe, type = 'bar')
p22 <- subplot(p18, p19, p20, p21, nrows = 2)
p22


tmp1$summe_total <- tmp1$summe
tmp2$summe_rtz <- tmp2$summe
tmp3$summe_ureg <- tmp3$summe
tmp4$summe_kauf <- tmp4$summe

tmp5 <- left_join(tmp1, tmp2, by = "Gruppe")
tmp5 <- left_join(tmp5, tmp3, by = "Gruppe")
tmp5 <- left_join(tmp5, tmp4, by = "Gruppe")

tmp5$tot_prz <- tmp5$summe_total / tmp5$summe_total *100
tmp5$rtz_prz <- tmp5$summe_rtz / tmp5$summe_total*100
tmp5$ureg_prz <- tmp5$summe_ureg / tmp5$summe_total*100
tmp5$kauf_prz <- tmp5$summe_kauf / tmp5$summe_total*100


p23 <- plot_ly(tmp5, x = ~Gruppe, y = ~tot_prz, type = 'bar')
p24 <- plot_ly(tmp5, x = ~Gruppe, y = ~rtz_prz, type = 'bar')
p25 <- plot_ly(tmp5, x = ~Gruppe, y = ~ureg_prz, type = 'bar')
p26 <- plot_ly(tmp5, x = ~Gruppe, y = ~kauf_prz, type = 'bar')
p27 <- subplot(p23, p24, p25, p26, nrows = 2)
p27

#### Kundenanalyse ####

# Gesamt
analyse_jahr <- 2009
top_anteil <- 100

tmp <- df %>%
  filter(Jahr == analyse_jahr) %>%
  filter(Marke != "KEINE ANGABE") %>%
  group_by(Marke) %>%
  summarise(summe = sum(Euro)) %>%
  arrange(desc(summe))
tmp$Marke <- fct_inorder(as.character(tmp$Marke))

p28 <- plot_ly(tmp, x = ~Marke, y = ~summe, type = 'bar')
p28


# TZ Kunden

tmp1 <- df %>%
  filter(Jahr == analyse_jahr) %>%
  filter(Marke != "KEINE ANGABE") %>%
  filter(media_fein == "REGIONAL ABO ZEITUNGEN") %>%
  group_by(Marke) %>%
  summarise(summe = sum(Euro)) %>%
  arrange(desc(summe))
tmp1$Marke <- fct_inorder(as.character(tmp1$Marke))
tmp1 <- tmp1[1:top_anteil, ]
p29 <- plot_ly(tmp1, x = ~Marke, y = ~summe, type = 'bar')
p29

tmp$total <- tmp$summe
tmp$summe <- NULL
tmp2 <- left_join(tmp1, tmp , by = "Marke")
tmp2$soa <- tmp2$summe / tmp2$total*100
tmp2$Marke <- fct_inorder(as.character(tmp1$Marke))
p30 <- plot_ly(tmp2, x = ~Marke, y = ~soa, type = 'bar')
p31 <- subplot(p29, p30, nrows = 2)
p31




# Gesamt
analyse_jahr <- 2018
top_anteil <- 100

tmp <- df %>%
  filter(Jahr == analyse_jahr) %>%
  filter(Marke != "KEINE ANGABE") %>%
  group_by(Marke) %>%
  summarise(summe = sum(Euro)) %>%
  arrange(desc(summe))
tmp$Marke <- fct_inorder(as.character(tmp$Marke))

p32 <- plot_ly(tmp, x = ~Marke, y = ~summe, type = 'bar')
p32


# TZ Kunden

tmp1 <- df %>%
  filter(Jahr == analyse_jahr) %>%
  filter(Marke != "KEINE ANGABE") %>%
  filter(media_fein == "REGIONAL ABO ZEITUNGEN") %>%
  group_by(Marke) %>%
  summarise(summe = sum(Euro)) %>%
  arrange(desc(summe))
tmp1$Marke <- fct_inorder(as.character(tmp1$Marke))
tmp1 <- tmp1[1:top_anteil, ]
p33 <- plot_ly(tmp1, x = ~Marke, y = ~summe, type = 'bar')
p33

tmp$total <- tmp$summe
tmp$summe <- NULL
tmp2 <- left_join(tmp1, tmp , by = "Marke")
tmp2$soa <- tmp2$summe / tmp2$total*100
tmp2$Marke <- fct_inorder(as.character(tmp1$Marke))
p34 <- plot_ly(tmp2, x = ~Marke, y = ~soa, type = 'bar')
p35 <- subplot(p33, p34, nrows = 2)
p35


# Zeitvergleichs Kundenplot:

p36 <- subplot(p29, p30, p33, p34, nrows =2 )
p36
