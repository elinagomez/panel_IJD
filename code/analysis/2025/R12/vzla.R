library(tidyverse)

vzla <- read_csv(paste0("data/processed/transcriptions/output/", year, "/transcripcion_", round_id, ".csv"))

vzla <- vzla |> select(-c(q1, q2, q3))

vzla <- vzla |>
  rename(
    "postura_intervencion" = q4,
    "postura_abierta" = q5
  )|>
  mutate(
    postura_intervencion = postura_intervencion |>
      str_trim() |>
      str_replace_all(" ", "_") |>
      str_replace_all("\\.", "")
  )

# ca

library(quanteda)
library(FactoMineR)
library(factoextra)
library(ggrepel)
library(sysfonts)
library(showtext)
year <- 2025
round_id <- "R12"


font_add_google("Montserrat", "montserrat")
showtext_auto()

dir.create(paste0("plots/", year, "/", round_id, ""), showWarnings = FALSE, recursive = TRUE)

# postura abierta por etiqueta
corpus <- corpus(
 vzla$postura_abierta,
 docvars = data.frame(group = vzla$etiqueta)
)

stops <- stopwords("spanish")

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

biplot_abierta_etiqueta <- fviz_ca_biplot(
 ca_resultado,
 repel = TRUE,
 col.row = "#D62728",
 col.col = "#455b55",
 alpha.col = 0.7,
 alpha.row = 1,
 labelsize = 18,
 pointsize = 1,
 font.family = "montserrat"
) +
  theme_minimal() +
  theme(
    text = element_text(family = "montserrat", size = 18),
    plot.title = element_blank()
  )

for (i in seq_along(biplot_abierta_etiqueta$layers)) {
  lyr <- biplot_abierta_etiqueta$layers[[i]]
  if (inherits(lyr$geom, "GeomTextRepel")) {
    es_filas <- identical(lyr$aes_params$colour, "#D62728")
    lyr$aes_params$fontface      <- if (es_filas) "bold" else "plain"
    lyr$aes_params$segment.alpha <- 0.5
    lyr$aes_params$segment.size  <- 0.1
    biplot_abierta_etiqueta$layers[[i]] <- lyr
  }
}

print(biplot_abierta_etiqueta)
ggsave(paste0("plots/", year, "/", round_id, "/CA_postura_abierta_por_etiqueta.png"), width = 12, height = 8, dpi = 300, bg = "white")

# postura abierta por postura intervención
corpus <- corpus(
 vzla$postura_abierta,
 docvars = data.frame(group = vzla$postura_intervencion)
)

stops <- stopwords("spanish")

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

biplot_abierta_postura <- fviz_ca_biplot(
 ca_resultado,
 repel = TRUE,
 col.row = "#D62728",
 col.col = "#455b55",
 alpha.col = 0.7,
 alpha.row = 1,
 labelsize = 18,
 pointsize = 1,
 font.family = "montserrat"
) +
  theme_minimal() +
  theme(
    text = element_text(family = "montserrat", size = 18),
    plot.title = element_blank()
  )

for (i in seq_along(biplot_abierta_postura$layers)) {
  lyr <- biplot_abierta_postura$layers[[i]]
  if (inherits(lyr$geom, "GeomTextRepel")) {
    es_filas <- identical(lyr$aes_params$colour, "#D62728")
    lyr$aes_params$fontface      <- if (es_filas) "bold" else "plain"
    lyr$aes_params$segment.alpha <- 0.5
    lyr$aes_params$segment.size  <- 0.1
    biplot_abierta_postura$layers[[i]] <- lyr
  }
}

print(biplot_abierta_postura)
ggsave(paste0("plots/", year, "/", round_id, "/CA_postura_abierta_por_postura_intervencion.png"), width = 12, height = 8, dpi = 300, bg = "white")

# postura intervención por etiqueta
corpus <- corpus(
 vzla$postura_intervencion,
 docvars = data.frame(group = vzla$etiqueta)
)

stops <- stopwords("spanish")

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

biplot_intervencion_etiqueta <- fviz_ca_biplot(
 ca_resultado,
 repel = TRUE,
 col.row = "#D62728",
 col.col = "#455b55",
 alpha.col = 0.7,
 alpha.row = 1,
 labelsize = 18,
 pointsize = 1,
 font.family = "montserrat"
) +
  theme_minimal() +
  theme(
    text = element_text(family = "montserrat", size = 18),
    plot.title = element_blank()
  )

for (i in seq_along(biplot_intervencion_etiqueta$layers)) {
  lyr <- biplot_intervencion_etiqueta$layers[[i]]
  if (inherits(lyr$geom, "GeomTextRepel")) {
    es_filas <- identical(lyr$aes_params$colour, "#D62728")
    lyr$aes_params$fontface      <- if (es_filas) "bold" else "plain"
    lyr$aes_params$segment.alpha <- 0.5
    lyr$aes_params$segment.size  <- 0.1
    biplot_intervencion_etiqueta$layers[[i]] <- lyr
  }
}

print(biplot_intervencion_etiqueta)
ggsave(paste0("plots/", year, "/", round_id, "/CA_postura_intervencion_por_etiqueta.png"), width = 12, height = 8, dpi = 300, bg = "white")

write_csv(vzla, "/Users/simonherrera/Downloads/intervencion_vzla.csv")
