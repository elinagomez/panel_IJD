# Escalamiento Multidimensional con Análisis de Sentimientos
# Usando FactoMineR para visualizar palabras en torno a sentimientos

library(readr)
library(FactoMineR)
library(factoextra)
library(quanteda)
library(dplyr)
library(ggplot2)
library(ggrepel)
year <- 2025
round_id <- "R8"


# ==== PASO 1: PREPARACIÓN DE DATOS ====

survey      <- read_csv(paste0("data/processed/analysis/", year, "/", round_id, "/r8_sentiment_q6.csv"))
respuesta   <- survey$q3
sentimiento <- survey$sentimiento_q3

# ==== PASO 2: PROCESAMIENTO DE TEXTO ====

corpus_sentimientos <- corpus(
  survey$q3,
  docvars = data.frame(sentimiento = sentimiento)
)

# ——— tokenización ———
# 1) conservar “no” entre las stop-words
stops <- setdiff(stopwords("spanish"), "no")

tokens_limpios <- tokens(
  corpus_sentimientos,
  remove_punct   = TRUE,
  remove_numbers = TRUE,
  remove_symbols = TRUE
) %>%
  tokens_tolower() %>%
  # 2) agrupar la secuencia “no conozco” → “no_conozco”
  tokens_compound(phrase("no conozco")) %>%
  tokens_remove(stops) %>%
  tokens_keep(min_nchar = 3)

# Document-Feature Matrix
dfm_sentimientos <- dfm(tokens_limpios) %>%
  dfm_trim(min_termfreq = 3)

# ==== PASO 3: AGRUPAR POR SENTIMIENTO ====

dfm_agrupado        <- dfm_group(dfm_sentimientos, groups = sentimiento)
matriz_sentimientos <- as.matrix(dfm_agrupado)
print(dim(matriz_sentimientos))
print(matriz_sentimientos[, 1:min(10, ncol(matriz_sentimientos))])

# ==== PASO 4: ANÁLISIS DE CORRESPONDENCIA ====

ca_resultado <- CA(matriz_sentimientos, graph = FALSE)
print(ca_resultado$eig)

# ==== PASO 5: VISUALIZACIONES ====

scree_plot <- fviz_screeplot(ca_resultado, addlabels = TRUE, ylim = c(0, 100)) +
  ggtitle("Varianza Explicada por Dimensión") +
  theme_minimal()
print(scree_plot)

biplot_principal <- fviz_ca_biplot(
  ca_resultado,
  repel      = TRUE,
  col.row    = "red",
  col.col    = "blue",
  arrow = c(FALSE, FALSE),
  alpha.col  = 0.7,
  alpha.row  = 1,
  labelsize  = 4,
  pointsize  = 2
) +
  ggtitle("Mapa de Sentimientos y Palabras") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    legend.position = "bottom"
  )
print(biplot_principal)

plot_sentimientos <- fviz_ca_row(
  ca_resultado,
  col.row       = "contrib",
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel         = TRUE,
  labelsize     = 5,
  pointsize     = 3
) +
  ggtitle("Posicionamiento de Sentimientos") +
  theme_minimal()
print(plot_sentimientos)

plot_palabras <- fviz_ca_col(
  ca_resultado,
  col.col       = "contrib",
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel         = TRUE,
  select.col    = list(contrib = 15),
  labelsize     = 3,
  pointsize     = 1.5
) +
  ggtitle("Palabras más Discriminantes") +
  theme_minimal()
print(plot_palabras)

# ==== PASO 6: CONTRIBUCIONES ====

contrib_sentimientos <- fviz_contrib(
  ca_resultado,
  choice = "row",
  axes   = 1:2,
  top    = 10
) +
  ggtitle("Contribución de Sentimientos a las Dimensiones")
print(contrib_sentimientos)

contrib_palabras_dim1 <- fviz_contrib(
  ca_resultado,
  choice = "col",
  axes   = 1,
  top    = 15
) +
  ggtitle("Palabras que más Contribuyen a la Dimensión 1")
print(contrib_palabras_dim1)

# ==== PASO 7: EXTRAS CUANTITATIVOS ====

coordenadas_sent <- as.data.frame(ca_resultado$row$coord) %>%
  mutate(sentimiento = rownames(.))
print(coordenadas_sent)

coordenadas_pal <- as.data.frame(ca_resultado$col$coord) %>%
  mutate(palabra = rownames(.))

top_dim1 <- coordenadas_pal |>
  slice_max(order_by = abs(`Dim 1`), n = 10) |>
  select(palabra, `Dim 1`)
print(top_dim1)

top_dim2 <- coordenadas_pal |>
  slice_max(order_by = abs(`Dim 2`), n = 10) |>
  select(palabra, `Dim 2`)
print(top_dim2)

# ==== PASO 8: INTERPRETACIÓN AUTOMÁTICA ====

interpretar_resultados <- function(ca_result) {
  cat("=== INTERPRETACIÓN DEL ANÁLISIS ===\n\n")
  var_expl <- ca_result$eig[1:2, 2]
  cat("Varianza explicada (Dim 1 + 2):", round(sum(var_expl), 1), "%\n\n")
  coords <- ca_result$row$coord
  cat("DIM 1 (", round(var_expl[1], 1), "%): +", names(which.max(coords[, 1])),
      " / –", names(which.min(coords[, 1])), "\n")
  cat("DIM 2 (", round(var_expl[2], 1), "%): +", names(which.max(coords[, 2])),
      " / –", names(which.min(coords[, 2])), "\n\n")
  cat("Calidad promedio (cos²):",
      round(mean(ca_result$row$cos2[, 1:2]) * 100, 1), "%\n")
}
interpretar_resultados(ca_resultado)
