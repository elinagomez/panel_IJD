library(dplyr)
library(readr)
library(stringr)

s <- read_csv(paste0("data/processed/analysis/", year, "/", round_id, "_sentiment_q9.csv"))

# "El esfuerzo individual alcanza para progresar en Uruguay, sin necesidad de ayuda estatal."

q3 <- s |>
 mutate(word_count = str_count(q3, "\\S+")) |>
 arrange(desc(word_count)) |>
 select(segmento, genero, edad, departamento, voto, etiqueta, q3, word_count) |>
 slice_head(n = 10)

library(quanteda)
library(FactoMineR)
library(factoextra)
library(ggrepel)
year <- 2025
round_id <- "R10"


corpus <- corpus(
 s$q3,
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

dfm <- dfm(tokens_limpios) |>
 dfm_trim(min_termfreq = 3)

dfm_agrupado <- dfm_group(dfm, groups = group)
matriz <- as.matrix(dfm_agrupado)

print(dim(matriz))
print(matriz[, 1:min(10, ncol(matriz))])

ca_resultado <- CA(matriz, graph = FALSE)
print(ca_resultado$eig)

biplot_principal <- fviz_ca_biplot(
 ca_resultado,
 repel = TRUE,
 col.row = "#EC4343",
 col.col = "#455b55",
 alpha.col = 0.7,
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

# modificar capas de ggrepel para que las líneas guía sean más tenues
for (i in seq_along(biplot_principal$layers)) {
  layer <- biplot_principal$layers[[i]]
  if (inherits(layer$geom, "GeomTextRepel")) {
    layer$aes_params$segment.alpha <- 0.5  # opacidad
    layer$aes_params$segment.size  <- 0.1   # grosor
    biplot_principal$layers[[i]] <- layer
  }
}

print(biplot_principal)
dir.create(paste0("plots/", year, "/", round_id, ""))
ggsave(paste0("plots/", year, "/", round_id, "/escalamiento_q3_etiqueta.jpg"), width = 18, height = 12, units = "cm", dpi = 300)

# "El estado uruguayo debe reconocer el esfuerzo, pero también hacerlo posible generando oportunidades."

q5 <- s |>
 mutate(word_count = str_count(q5, "\\S+")) |>
 arrange(desc(word_count)) |>
 select(segmento, genero, edad, departamento, voto, etiqueta, q4, q5, word_count) |>
 slice_head(n = 10)

corpus <- corpus(
 s$q5,
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

dfm <- dfm(tokens_limpios) |>
 dfm_trim(min_termfreq = 3)

dfm_agrupado <- dfm_group(dfm, groups = group)
matriz <- as.matrix(dfm_agrupado)

print(dim(matriz))
print(matriz[, 1:min(10, ncol(matriz))])

ca_resultado <- CA(matriz, graph = FALSE)
print(ca_resultado$eig)

biplot_principal <- fviz_ca_biplot(
 ca_resultado,
 repel = TRUE,
 col.row = "#EC4343",
 col.col = "#455b55",
 alpha.col = 0.7,
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

# modificar capas de ggrepel para que las líneas guía sean más tenues
for (i in seq_along(biplot_principal$layers)) {
  layer <- biplot_principal$layers[[i]]
  if (inherits(layer$geom, "GeomTextRepel")) {
    layer$aes_params$segment.alpha <- 0.5  # opacidad
    layer$aes_params$segment.size  <- 0.1   # grosor
    biplot_principal$layers[[i]] <- layer
  }
}

print(biplot_principal)
ggsave(paste0("plots/", year, "/", round_id, "/escalamiento_q5_etiqueta.jpg"), width = 18, height = 12, units = "cm", dpi = 300)

# "El Estado debe crear oportunidades para que el esfuerzo individual tenga frutos."

q7 <- s |>
 mutate(word_count = str_count(q7, "\\S+")) |>
 arrange(desc(word_count)) |>
 select(segmento, genero, edad, departamento, voto, etiqueta,q6, q7, word_count) |>
 slice_head(n = 10)

corpus <- corpus(
 s$q7,
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

dfm <- dfm(tokens_limpios) |>
 dfm_trim(min_termfreq = 3)

dfm_agrupado <- dfm_group(dfm, groups = group)
matriz <- as.matrix(dfm_agrupado)

print(dim(matriz))
print(matriz[, 1:min(10, ncol(matriz))])

ca_resultado <- CA(matriz, graph = FALSE)
print(ca_resultado$eig)

biplot_principal <- fviz_ca_biplot(
 ca_resultado,
 repel = TRUE,
 col.row = "#EC4343",
 col.col = "#455b55",
 alpha.col = 0.7,
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

# modificar capas de ggrepel para que las líneas guía sean más tenues
for (i in seq_along(biplot_principal$layers)) {
  layer <- biplot_principal$layers[[i]]
  if (inherits(layer$geom, "GeomTextRepel")) {
    layer$aes_params$segment.alpha <- 0.5  # opacidad
    layer$aes_params$segment.size  <- 0.1   # grosor
    biplot_principal$layers[[i]] <- layer
  }
}

print(biplot_principal)
ggsave(paste0("plots/", year, "/", round_id, "/escalamiento_q7_etiqueta.jpg"), width = 18, height = 12, units = "cm", dpi = 300)


# ¿Cuál es su opinión sobre la situación económica del país que el gobierno de Lacalle Pou le dejó al gobierno de Orsi?
# ¿Diría que el gobierno de Lacalle Pou dejó una situación económica muy buena, buena, ni buena ni mala, mala o muy mala?

q9 <- s |>
 mutate(word_count = str_count(q9, "\\S+")) |>
 arrange(desc(word_count)) |>
 select(segmento, genero, edad, departamento, voto, etiqueta, q9, word_count) |>
 slice_head(n = 10)

corpus <- corpus(
 s$q9,
 docvars = data.frame(group = s$sentimiento_q9)
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

dfm <- dfm(tokens_limpios) |>
 dfm_trim(min_termfreq = 3)

dfm_agrupado <- dfm_group(dfm, groups = group)
matriz <- as.matrix(dfm_agrupado)

print(dim(matriz))
print(matriz[, 1:min(10, ncol(matriz))])

ca_resultado <- CA(matriz, graph = FALSE)
print(ca_resultado$eig)

biplot_principal <- fviz_ca_biplot(
 ca_resultado,
 repel = TRUE,
 col.row = "#EC4343",
 col.col = "#455b55",
 alpha.col = 0.7,
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

# modificar capas de ggrepel para que las líneas guía sean más tenues
for (i in seq_along(biplot_principal$layers)) {
  layer <- biplot_principal$layers[[i]]
  if (inherits(layer$geom, "GeomTextRepel")) {
    layer$aes_params$segment.alpha <- 0.5  # opacidad
    layer$aes_params$segment.size  <- 0.1   # grosor
    biplot_principal$layers[[i]] <- layer
  }
}

print(biplot_principal)
ggsave(paste0("plots/", year, "/", round_id, "/escalamiento_q9_sentimiento.jpg"), width = 18, height = 12, units = "cm", dpi = 300)

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

dfm <- dfm(tokens_limpios) |>
 dfm_trim(min_termfreq = 3)

dfm_agrupado <- dfm_group(dfm, groups = group)
matriz <- as.matrix(dfm_agrupado)

print(dim(matriz))
print(matriz[, 1:min(10, ncol(matriz))])

ca_resultado <- CA(matriz, graph = FALSE)
print(ca_resultado$eig)

biplot_principal <- fviz_ca_biplot(
 ca_resultado,
 repel = TRUE,
 col.row = "#EC4343",
 col.col = "#455b55",
 alpha.col = 0.7,
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

# modificar capas de ggrepel para que las líneas guía sean más tenues
for (i in seq_along(biplot_principal$layers)) {
  layer <- biplot_principal$layers[[i]]
  if (inherits(layer$geom, "GeomTextRepel")) {
    layer$aes_params$segment.alpha <- 0.5  # opacidad
    layer$aes_params$segment.size  <- 0.1   # grosor
    biplot_principal$layers[[i]] <- layer
  }
}

print(biplot_principal)
ggsave(paste0("plots/", year, "/", round_id, "/escalamiento_q9_etiqueta.jpg"), width = 18, height = 12, units = "cm", dpi = 300)