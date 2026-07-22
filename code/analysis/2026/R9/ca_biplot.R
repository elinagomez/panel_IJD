library(dplyr)
library(stringr)
library(quanteda)
library(FactoMineR)
library(factoextra)
library(ggrepel)
library(sysfonts)
library(showtext)

path <- "data/processed/transcriptions/output/2026/transcripcion_R9.xlsx"
df <- readxl::read_excel(path)

font_add_google("Montserrat", "montserrat") # fuente para las etiquetas, puedes cambiarla pero debes cambiarla en todas las líneas en que aparece
showtext_auto()

corpus <- corpus(
 df$q2,
 docvars = data.frame(group = df$etiqueta)
)

stops <- stopwords("spanish")

tokens_limpios <- tokens(
 corpus,
 remove_punct = TRUE,
 remove_numbers = TRUE,
 remove_symbols = TRUE
) |>
 tokens_tolower() |>
 tokens_remove(stops) |> # elimina stopwords
 tokens_keep(min_nchar = 3) # elimina palabras con menos de 3 caracteres

dfm <- dfm(tokens_limpios) |>
 dfm_trim(min_termfreq = 3) # filtro de términos con frecuencia mínima (3)

dfm_agrupado <- dfm_group(dfm, groups = group)
matriz <- as.matrix(dfm_agrupado)

print(dim(matriz))
print(matriz[, 1:min(10, ncol(matriz))])

ca_resultado <- CA(matriz, graph = FALSE)
print(ca_resultado$eig)

biplot_principal <- fviz_ca_biplot(
 ca_resultado,
 repel = TRUE,
 col.row = "#D62728", # color de las filas
 col.col = "#455b55", # color de las columnas
 alpha.col = 0.7,
 alpha.row = 1,
 labelsize = 14, # tamaño de las etiquetas
 pointsize = 1, # tamaño de los puntos
 font.family = "montserrat"
) +
  theme_minimal() +
  theme(
    text = element_text(family = "montserrat"), # ejes/leyenda
    plot.title = element_blank()
  )

for (i in seq_along(biplot_principal$layers)) {
  lyr <- biplot_principal$layers[[i]]
  if (inherits(lyr$geom, "GeomTextRepel")) {
    es_filas <- identical(lyr$aes_params$colour, "#D62728")  # mismo valor que col.row
    lyr$aes_params$fontface      <- if (es_filas) "bold" else "plain"
    lyr$aes_params$segment.alpha <- 0.5 # transparencia de las líneas
    lyr$aes_params$segment.size  <- 0.1 # grosor de las líneas
    biplot_principal$layers[[i]] <- lyr
  }
}

print(biplot_principal)
dir.create("plots/2026/R9")
ggsave("plots/2026/R9/2026_ca_r9.png",width = 12, height = 8, dpi = 300, bg = "white")