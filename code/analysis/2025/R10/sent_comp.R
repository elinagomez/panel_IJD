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
  up <- 10 ^ digits
  x  <- x * up
  y  <- floor(x)
  indices <- tail(order(x - y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y / up
}

color1 <- "#EC4343"
color2 <- "#009E73"
color3 <- "#F0E442"

df_a <- readr::read_csv(paste0("data/processed/analysis/", year, "/", round_id, "/R10_sentiment_q9.csv")) |>
  mutate(numero = as.character(numero)) |>
  filter(!is.na(sentimiento_q9), !is.na(numero))

df_b <- readr::read_csv("/Users/simonherrera/Downloads/transcripcion_R4 - etiq_transcripcion_R4.csv") |>
  mutate(numero = as.character(numero)) |>
  filter(!is.na(etiq_economiallp), !is.na(numero))

common_ids <- intersect(df_a$numero, df_b$numero)
df_a_int <- df_a |> filter(numero %in% common_ids)
df_b_int <- df_b |> filter(numero %in% common_ids)
dir.create(paste0("/Users/simonherrera/Desktop/paneles/plots/", year, "/", round_id, ""), recursive = TRUE, showWarnings = FALSE)

tab <- df_a_int %>% count(sentimiento_q9) %>% mutate(texto = paste(sentimiento_q9, n))

p01 <- ggplot(tab, aes(area = n, fill = sentimiento_q9, label = texto)) +
  treemapify::geom_treemap(layout = "squarified") +
  geom_treemap_text(place = "centre", color = "white", size = 12,
                    grow = TRUE, family = "montserrat") +
  labs(title = "Economía LLP 9/8") +
  scale_fill_manual(values = c("NEG" = color1, "NEU" = color3, "POS" = color2)) +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 18, family = "montserrat"),
    title = element_text(size = 140, face = "bold", family = "montserrat")
  )

ggsave(paste0(paste0("/Users/simonherrera/Desktop/paneles/plots/", year, "/", round_id, "/"), etiq, ".png"),
       p01, width = 30, height = 30, units = "cm")

# ------------------------------------------------------------------ #

etiq <- "etiq_economiallp"

tab2 <- df_b_int %>% count(etiq_economiallp) %>% mutate(texto = paste(etiq_economiallp, n))

p02 <- ggplot(tab2, aes(area = n, fill = etiq_economiallp, label = texto)) +
  treemapify::geom_treemap(layout = "squarified") +
  geom_treemap_text(place = "centre", color = "white", size = 12,
                    grow = TRUE, family = "montserrat") +
  labs(title = "Economía LLP 20/6") +
  scale_fill_manual(values = c("NEG" = color1, "NEU" = color3, "POS" = color2)) +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 18, family = "montserrat"),
    title = element_text(size = 140, face = "bold", family = "montserrat")
  )

ggsave(paste0(paste0("/Users/simonherrera/Desktop/paneles/plots/", year, "/", round_id, "/"), etiq, ".png"),
       p02, width = 30, height = 30, units = "cm")

# --- Comparativo lado a lado ---------------------------------------------------
p_comb <- ggarrange(p02, p01, ncol = 2, nrow = 1, align = "h")
p_comb

ggsave(paste0("/Users/simonherrera/Desktop/paneles/plots/", year, "/", round_id, "/compare_sent_q9_etiq_economiallp.png"),
       p_comb, width = 60, height = 30, units = "cm")
