library(dplyr)
library(readr)
library(ggplot2)
year <- 2025
round_id <- "R9"


s <- read_csv(paste0("data/processed/analysis/", year, "/", round_id, "/etiq_r9.csv"))
dir.create(paste0("plots/", year, "/", round_id, ""), recursive = TRUE, showWarnings = FALSE)

levels_q7 <- c(
  "suficiente",
  "no_eficiencia",
  "corrupción",
  "si_eficiencia",
  "insuficiente",
  "universalismo_descentralización"
)

s <- s |> 
  filter(!is.na(etiq_q7)) |>
  mutate(etiq_q7 = factor(etiq_q7, levels = levels_q7)) |>
  mutate(etiq_q7 = forcats::fct_recode(etiq_q7,
    "\"La presencia que hay es suficiente\"" = "suficiente",
    "\"No quiero mayor presencia porque\nla que hay es ineficiente\"" = "no_eficiencia",
    "Mencionan hechos de corrupción" = "corrupción",
    "\"Es necesaria mayor presencia porque\nla que hay es ineficiente\"" = "si_eficiencia", 
    "\"Los servicios o la presencia del Estado\nson insuficientes\"" = "insuficiente",
    "\"Todos deberían poder acceder\", o\n\"el Estado no llega a mi zona\"" = "universalismo_descentralización"
  ))

ggplot(s, aes(x = segmento, fill = etiq_q7)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set3") +  # Cambiá Set2 por Paired, Pastel1, Dark2, etc.
  labs(
    title = "",
    x = "Segmento",
    y = "",
    fill = "Postura sobre si el Estado\ndebería estar más presente\nen su zona"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
    legend.key.height = unit(0.8, "cm")
  )

ggsave(paste0("plots/", year, "/", round_id, "/r9_q7.jpg"), width = 18, height = 12, units = "cm", dpi = 300)

# ------------------------------------------------------------------ #

levels_impuesto <- c(
  "no",
  "ns",
  "si"  
)

s <- s |> 
  mutate(aprueba_impuesto = factor(aprueba_impuesto, levels = levels_impuesto)) |> 
  mutate(aprueba_impuesto = forcats::fct_recode(aprueba_impuesto,
  "No" = "no",
  "No sabe" = "ns",
  "Sí" = "si"
  ))

ggplot(s, aes(x = segmento, fill = aprueba_impuesto)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "RdBu") +  # Cambiá Set2 por Paired, Pastel1, Dark2, etc.
  labs(
    title = "",
    x = "Segmento",
    y = "",
    fill = "Postura sobre el\nimpuesto al 1%"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

ggsave(paste0("plots/", year, "/", round_id, "/r9_q8.jpg"), width = 18, height = 12, units = "cm", dpi = 300)

# ------------------------------------------------------------------ #

s <- s |> 
  mutate(across(
    c(q2, q4, q6),
    ~ recode(.x, `Muy en desacuerdo` = "Muy en desacuerdo", `2` = "En desacuerdo", `3` = "Ni de acuerdo, ni en desacuerdo",
             `4` = "De acuerdo", `Muy de acuerdo` = "Muy de acuerdo")
  ))

s <- s |> 
  mutate(across(c(q2, q4, q6), factor, levels = c("Muy en desacuerdo", "En desacuerdo", "Ni de acuerdo, ni en desacuerdo", "De acuerdo", "Muy de acuerdo")))

s |>
  filter(!is.na(q2)) |> 
  ggplot(aes(x = segmento, fill = forcats::fct_rev(q2))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "RdYlGn", direction = -1) +
  labs(
    title = "\"Uruguay necesita un gobierno que genere oportunidades...\"",
    x = "Segmento",
    y = "",
    fill = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title  = element_text(hjust = 0.5)
  )
ggsave(paste0("plots/", year, "/", round_id, "/r9_q2.jpg"), width = 18, height = 12, units = "cm", dpi = 300)

# ------------------------------------------------------------------ #

s |>
  filter(!is.na(q4)) |> 
  ggplot(aes(x = segmento, fill = forcats::fct_rev(q4))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "RdYlGn", direction = -1) +
  labs(
    title = "\"En los últimos tiempos la presencia del Estado es más débil en mi zona\"",
    x = "Segmento",
    y = "",
    fill = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title  = element_text(hjust = 0.5)
  )
ggsave(paste0("plots/", year, "/", round_id, "/r9_q4.jpg"), width = 18, height = 12, units = "cm", dpi = 300)

s |>
  filter(!is.na(q6)) |> 
  ggplot(aes(x = segmento, fill = forcats::fct_rev(q6))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "RdYlGn", direction = -1) +
  labs(
    title = "\"El Estado debe recuperar un rol más activo con servicios y presencia de\noficinas de organismos públicos en mi zona\"",
    x = "Segmento",
    y = "",
    fill = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title  = element_text(hjust = 0.5)
  )
ggsave(paste0("plots/", year, "/", round_id, "/r9_q6.jpg"), width = 18, height = 12, units = "cm", dpi = 300)

# ------------------------------------------------------------------ #

library(quanteda)
library(FactoMineR)
library(factoextra)
library(ggrepel)

corpus <- corpus(
 s$q7,
 docvars = data.frame(etiq_q7 = s$etiq_q7)
)

stops <- setdiff(stopwords("spanish"), "no")

tokens_limpios <- tokens(
 corpus,
 remove_punct = TRUE,
 remove_numbers = TRUE,
 remove_symbols = TRUE
) |>
 tokens_tolower() |>
 tokens_remove(stops) |>
 tokens_keep(min_nchar = 3)

dfm_q7 <- dfm(tokens_limpios) |>
 dfm_trim(min_termfreq = 5)

dfm_agrupado <- dfm_group(dfm_q7, groups = etiq_q7)
matriz <- as.matrix(dfm_agrupado)

print(dim(matriz))
print(matriz[, 1:min(10, ncol(matriz))])

ca_resultado <- CA(matriz, graph = FALSE)
print(ca_resultado$eig)

biplot_principal <- fviz_ca_biplot(
 ca_resultado,
 repel = TRUE,
 col.row = "red",
 col.col = "blue",
 alpha.col = 0.7,
 alpha.row = 1,
 labelsize = 3,
 pointsize = 2
) +
 ggtitle("") +
 theme_minimal() +
 theme(
   plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
   axis.title = element_text(size = 12),
   legend.position = "bottom"
 )

print(biplot_principal)
ggsave(paste0("plots/", year, "/", round_id, "/escalamiento_q7.jpg"), width = 18, height = 12, units = "cm", dpi = 300)

# ------------------------------------------------------------------ #

corpus <- corpus(
 s$q8,
 docvars = data.frame(aprueba_impuesto = s$etiqueta)
)

stops <- setdiff(stopwords("spanish"), "no")

tokens_limpios <- tokens(
 corpus,
 remove_punct = TRUE,
 remove_numbers = TRUE,
 remove_symbols = TRUE
) |>
 tokens_tolower() |>
 tokens_remove(stops) |>
 tokens_keep(min_nchar = 3)

dfm_q8 <- dfm(tokens_limpios) |>
 dfm_trim(min_termfreq = 5)

dfm_agrupado <- dfm_group(dfm_q8, groups = aprueba_impuesto)
matriz <- as.matrix(dfm_agrupado)

print(dim(matriz))
print(matriz[, 1:min(10, ncol(matriz))])

ca_resultado <- CA(matriz, graph = FALSE)
print(ca_resultado$eig)

biplot_principal <- fviz_ca_biplot(
 ca_resultado,
 repel = TRUE,
 col.row = "#ff4400ff",
 col.col = "#455b55",
 alpha.col = 0.7,
 alpha.row = 1,
 labelsize = 3,
 pointsize = 1
) +
 ggtitle("") +
 theme_minimal() +
 theme(
   plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
   axis.title = element_text(size = 12),
   legend.position = "bottom"
 )

print(biplot_principal)
ggsave(paste0("plots/", year, "/", round_id, "/escalamiento_q8_etiqueta.jpg"), width = 18, height = 12, units = "cm", dpi = 300)

# ------------------------------------------------------------------ #

library(stringr)

citas_q7 <- s |>
 mutate(word_count = str_count(q7, "\\S+")) |>
 arrange(desc(word_count)) |>
 select(segmento, genero, edad, departamento, etiq_q7, q7, word_count) |>
 slice_head(n = 10)

# s |>
#  mutate(word_count = str_count(q7, "\\S+")) |>
#  group_by(etiq_q7) |>
#  arrange(desc(word_count)) |>
#  slice_head(n = 3) |>
#  select(etiq_q7, q7, word_count) |>
#  ungroup()

citas_q6 <- s |>
 mutate(word_count = str_count(q7, "\\S+")) |>
 arrange(desc(word_count)) |>
 select(segmento, genero, edad, departamento, q6, word_count) |>
 slice_head(n = 10)

# ------------------------------------------------------------------ #

corpus <- corpus(
 s$q9,
 docvars = data.frame(group = s$etiqueta)
)

stops <- setdiff(stopwords("spanish"), "no")

tokens_limpios <- tokens(
 corpus,
 remove_punct = TRUE,
 remove_numbers = TRUE,
 remove_symbols = TRUE
) |>
 tokens_tolower() |>
 tokens_remove(stops) |>
 tokens_keep(min_nchar = 3)

dfm_q8 <- dfm(tokens_limpios) |>
 dfm_trim(min_termfreq = 5)

dfm_agrupado <- dfm_group(dfm_q8, groups = group)
matriz <- as.matrix(dfm_agrupado)

print(dim(matriz))
print(matriz[, 1:min(10, ncol(matriz))])

ca_resultado <- CA(matriz, graph = FALSE)
print(ca_resultado$eig)

biplot_principal <- fviz_ca_biplot(
 ca_resultado,
 repel = TRUE,
 col.row = "#000000ff",
 col.col = "#ff4545ff",
 alpha.col = 0.5,
 alpha.row = 1,
 labelsize = 2,
 pointsize = 1
) +
 ggtitle("") +
 theme_minimal() +
 theme(
   plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
   axis.title = element_text(size = 12),
   legend.position = "bottom"
 )

print(biplot_principal)
ggsave(paste0("plots/", year, "/", round_id, "/escalamiento_q9_etiqueta.jpg"), width = 18, height = 12, units = "cm", dpi = 300)

# ------------------------------------------------------------------ #

library(dplyr)
library(tidyr)
library(ggplot2) 
library(treemapify) 
library(showtext)
library(ggpubr)

font_add_google("Montserrat", "montserrat")
showtext_auto()

etiq <- "ETIQ"

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

df_sent <- read_csv("/Users/simonherrera/Downloads/transcripcion_R9 - S.csv") %>%
  mutate(ETIQ = case_when(
      ETIQ == "NUE" ~ "NEU",
      TRUE ~ ETIQ
    )
  )
df_sent <- df_sent %>% filter(!is.na(ETIQ))

tab <- df_sent  %>% filter(etiqueta == "dialoguista") %>% count(ETIQ)
tab <- tab %>% mutate(freq = round_preserve(n/sum(n)*100, 0))

tab <- tab %>% mutate(texto=paste(ETIQ, paste(freq, "%")))
tab

p01 <- ggplot(tab, aes(area = n, fill = ETIQ, label = texto))+ 
  treemapify::geom_treemap(layout="squarified")+ 
  geom_treemap_text(place = "centre", color = "white", size = 16,
                    grow=TRUE, family = "montserrat")+ 
  labs(title="Dialoguistas")+
  scale_fill_manual(values=c("NEG"=color1, "NEU"=color3, "POS"=color2)) + 
  theme(legend.position = "none",
        strip.text = element_text(size = 20, family = "montserrat"),
        title =element_text(size = 50, face='bold', family = "montserrat"))
p01

# ggsave(paste0(paste0("/Users/simonherrera/Desktop/paneles/plots/", year, "/", round_id, "/"), etiq,".png"), width = 30, height = 30, units = "cm")

tab <- df_sent  %>% filter(etiqueta == "oposicion_abierta") %>% count(ETIQ)
tab <- tab %>% mutate(freq = round_preserve(n/sum(n)*100, 0))

tab <- tab %>% mutate(texto=paste(ETIQ, paste(freq, "%")))
tab

p02 <- ggplot(tab, aes(area = n, fill = ETIQ, label = texto))+ 
  treemapify::geom_treemap(layout="squarified")+ 
  geom_treemap_text(place = "centre", color = "white", size = 16,
                    grow=TRUE, family = "montserrat")+ 
  labs(title="Oposición abierta")+
  scale_fill_manual(values=c("NEG"=color1, "NEU"=color3, "POS"=color2)) + 
  theme(legend.position = "none",
        strip.text = element_text(size = 20, family = "montserrat"),
        title =element_text(size = 80, face='bold', family = "montserrat"))
p02

tab <- df_sent  %>% filter(etiqueta == "oficialista_abierto") %>% count(ETIQ)
tab <- tab %>% mutate(freq = round_preserve(n/sum(n)*100, 0))

tab <- tab %>% mutate(texto=paste(ETIQ, paste(freq, "%")))
tab

p03 <- ggplot(tab, aes(area = n, fill = ETIQ, label = texto))+ 
  treemapify::geom_treemap(layout="squarified")+ 
  geom_treemap_text(place = "centre", color = "white", size = 16,
                    grow=TRUE, family = "montserrat")+ 
  labs(title="Oficialista abierto")+
  scale_fill_manual(values=c("NEG"=color1, "NEU"=color3, "POS"=color2)) + 
  theme(legend.position = "none",
        strip.text = element_text(size = 20, family = "montserrat"),
        title =element_text(size = 80, face='bold', family = "montserrat"))
p03

tab <- df_sent  %>% filter(etiqueta == "oposicion_cerril") %>% count(ETIQ)
tab <- tab %>% mutate(freq = round_preserve(n/sum(n)*100, 0))

tab <- tab %>% mutate(texto=paste(ETIQ, paste(freq, "%")))
tab

p04 <- ggplot(tab, aes(area = n, fill = ETIQ, label = texto))+ 
  treemapify::geom_treemap(layout="squarified")+ 
  geom_treemap_text(place = "centre", color = "white", size = 16,
                    grow=TRUE, family = "montserrat")+ 
  labs(title="Oposición cerril")+
  scale_fill_manual(values=c("NEG"=color1, "NEU"=color3, "POS"=color2)) + 
  theme(legend.position = "none",
        strip.text = element_text(size = 20, family = "montserrat"),
        title =element_text(size = 80, face='bold', family = "montserrat"))
p04

tab <- df_sent  %>% filter(etiqueta == "oficialista_acerrimo") %>% count(ETIQ)
tab <- tab %>% mutate(freq = round_preserve(n/sum(n)*100, 0))

tab <- tab %>% mutate(texto=paste(ETIQ, paste(freq, "%")))
tab

p05 <- ggplot(tab, aes(area = n, fill = ETIQ, label = texto))+ 
  treemapify::geom_treemap(layout="squarified")+ 
  geom_treemap_text(place = "centre", color = "white", size = 16,
                    grow=TRUE, family = "montserrat")+ 
  labs(title="Oficialista acérrimo")+
  scale_fill_manual(values=c("NEG"=color1, "NEU"=color3, "POS"=color2)) + 
  theme(legend.position = "none",
        strip.text = element_text(size = 20, family = "montserrat"),
        title =element_text(size = 80, face='bold', family = "montserrat"))
p05

tab <- df_sent  %>% filter(etiqueta == "desinformadas") %>% count(ETIQ)
tab <- tab %>% mutate(freq = round_preserve(n/sum(n)*100, 0))

tab <- tab %>% mutate(texto=paste(ETIQ, paste(freq, "%")))
tab

p06 <- ggplot(tab, aes(area = n, fill = ETIQ, label = texto))+ 
  treemapify::geom_treemap(layout="squarified")+ 
  geom_treemap_text(place = "centre", color = "white", size = 16,
                    grow=TRUE, family = "montserrat")+ 
  labs(title="Desinformadas")+
  scale_fill_manual(values=c("NEG"=color1, "NEU"=color3, "POS"=color2)) + 
  theme(legend.position = "none",
        strip.text = element_text(size = 20, family = "montserrat"),
        title =element_text(size = 50, face='bold', family = "montserrat"))
p06

# descreidas/alejadas

tab <- df_sent  %>% filter(etiqueta == "descreidas/alejadas") %>% count(ETIQ)
tab <- tab %>% mutate(freq = round_preserve(n/sum(n)*100, 0))

tab <- tab %>% mutate(texto=paste(ETIQ, paste(freq, "%")))
tab

p07 <- ggplot(tab, aes(area = n, fill = ETIQ, label = texto))+ 
  treemapify::geom_treemap(layout="squarified")+ 
  geom_treemap_text(place = "centre", color = "white", size = 16,
                    grow=TRUE, family = "montserrat")+ 
  labs(title="Descreidas/alejadas")+
  scale_fill_manual(values=c("NEG"=color1, "NEU"=color3, "POS"=color2)) + 
  theme(legend.position = "none",
        strip.text = element_text(size = 20, family = "montserrat"),
        title =element_text(size = 50, face='bold', family = "montserrat"))
p07

ggarrange(p03, p05, ncol = 2)
# ggsave(paste0(paste0("plots/", year, "/", round_id, "/oficialista_sent_1pct.png")), width = 20, height = 10, units = "cm")

ggarrange(p01, p06, p07, ncol = 3)
ggsave(paste0(paste0("plots/", year, "/", round_id, "/des_sent_1pct.png")), width = 30, height = 10, units = "cm")

ggarrange(p02, p04, ncol = 2)
ggsave(paste0(paste0("plots/", year, "/", round_id, "/oposicion_sent_1pct.png")), width = 20, height = 10, units = "cm")

# ------------------------------------------------------------------ #
levels_calle <- c(
 "no",
 "ns",
 "si"
)
s <- s |>
 filter(!is.na(mantener_medidas)) |>
 mutate(mantener_medidas = factor(mantener_medidas, levels = levels_calle)) |>
 mutate(mantener_medidas = forcats::fct_recode(mantener_medidas,
   "No" = "no",
   "No sabe" = "ns",
   "Sí" = "si"
 ))
ggplot(s, aes(x = segmento, fill = mantener_medidas)) +
 geom_bar(position = "fill") +
 scale_y_continuous(labels = scales::percent) +
 scale_fill_brewer(palette = "RdBu") +
 labs(
   title = "",
   x = "Segmento",
   y = "",
   fill = "Postura sobre mantener medidas de\nsituación de calle, aún pasado el invierno"
 ) +
 theme_minimal() +
 theme(
   axis.text.x = element_text(angle = 45, hjust = 1),
   plot.title = element_text(hjust = 0.5)
 )
ggsave(paste0("plots/", year, "/", round_id, "/r9_q1.jpg"), width = 18, height = 12, units = "cm", dpi = 300)
