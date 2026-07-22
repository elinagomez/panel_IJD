library(dplyr)
library(tidyr)
library(ggplot2) 
library(treemapify) 
library(showtext)
library(ggpubr)
year <- 2025
round_id <- "R10"


font_add_google("Montserrat", "montserrat")
showtext_auto()

etiq <- "sentimiento_q9"

round_preserve <- function(x, digits = 0) {
  up <-  10 ^ digits
  x <-  x * up
  y <-  floor(x)
  indices <-  tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <-  y[indices] + 1
  y / up
}

color1 <- "#EC4343"
color2 <- "#009E73"
color3 <- "#F0E442"

df_sent <- readr::read_csv(paste0("data/processed/analysis/", year, "/", round_id, "/R10_sentiment_q9.csv"))
df_sent <- df_sent %>% filter(!is.na(sentimiento_q9))

tab <- df_sent %>% count(sentimiento_q9)
tab <- tab %>% mutate(texto = paste(sentimiento_q9, n))

p01 <- ggplot(tab, aes(area = n, fill = sentimiento_q9, label = texto))+ 
  treemapify::geom_treemap(layout="squarified")+ 
  geom_treemap_text(place = "centre", color = "white", size = 16,
                    grow=TRUE, family = "montserrat")+ 
  labs(title="Economía LLP 9/8")+
  scale_fill_manual(values=c("NEG"=color1, "NEU"=color3, "POS"=color2)) + 
  theme(legend.position = "none",
        strip.text = element_text(size = 20, family = "montserrat"),
        title = element_text(size = 250, face = 'bold', family = "montserrat"))
p01

ggsave(paste0(paste0("/Users/simonherrera/Desktop/paneles/plots/", year, "/", round_id, "/"), etiq,".png"),
       width = 30, height = 30, units = "cm")
dir.create(paste0("/Users/simonherrera/Desktop/paneles/plots/", year, "/", round_id, ""), recursive = TRUE, showWarnings = FALSE)
