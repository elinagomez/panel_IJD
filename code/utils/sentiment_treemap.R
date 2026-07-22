library(dplyr)
library(tidyr)
library(ggplot2) 
library(treemapify) 
library(showtext)
library(ggpubr)

font_add_google("Montserrat", "montserrat")
showtext_auto()

etiq <- "sentimiento_q6"

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

df_sent <- readr::read_csv("data/processed/analysis/2025/R8/r8_sentiment_q6.csv")
df_sent <- df_sent %>% filter(!is.na(sentimiento_q6))
dir.create("/Users/simonherrera/Desktop/paneles/plots/2025/R8", recursive = TRUE, showWarnings = FALSE)

tab <- df_sent  %>% count(sentimiento_q6)
tab <- tab %>% mutate(freq = round_preserve(n/sum(n)*100, 0))

tab <- tab %>% mutate(texto=paste(sentimiento_q6, paste(freq, "%")))
tab

p01 <- ggplot(tab, aes(area = n, fill = sentimiento_q6, label = texto))+ 
  treemapify::geom_treemap(layout="squarified")+ 
  geom_treemap_text(place = "centre", color = "white", size = 16,
                    grow=TRUE, family = "montserrat")+ 
  labs(title="Desempeño internacional Orsi")+
  scale_fill_manual(values=c("NEG"=color1, "NEU"=color3, "POS"=color2)) + 
  theme(legend.position = "none",
        strip.text = element_text(size = 20, family = "montserrat"),
        title =element_text(size = 150, face='bold', family = "montserrat"))
p01

ggsave(paste0("/Users/simonherrera/Desktop/paneles/plots/2025/R8/", etiq,".png"), width = 30, height = 30, units = "cm")
