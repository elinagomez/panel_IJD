library(dplyr)
library(readr)
library(stringr)
library(quanteda)
library(FactoMineR)
library(factoextra)
library(ggrepel)
library(sysfonts)
library(showtext)
year <- 2025
round_id <- "R11"


font_add_google("Montserrat", "montserrat")
showtext_auto()

path <- paste0("data/processed/transcriptions/output/", year, "/transcripcion_", round_id, ".csv")
s <- read_csv(path)

ensure_dir <- function(path) if (!dir.exists(path)) dir.create(path, recursive = TRUE)

build_ca_biplot <- function(matriz, font_family = "montserrat") {
  ca_resultado <- CA(matriz, graph = FALSE)
  p <- fviz_ca_biplot(
    ca_resultado,
    repel = TRUE,
    col.row = "#D62728",
    col.col = "#455b55",
    alpha.col = 0.7,
    alpha.row = 1,
    labelsize = 10,
    pointsize = 1,
    font.family = font_family
  ) +
    theme_minimal() +
    theme(
      text = element_text(family = font_family),
      plot.title = element_blank()
    )

  for (i in seq_along(p$layers)) {
    lyr <- p$layers[[i]]
    if (inherits(lyr$geom, "GeomTextRepel")) {
      es_filas <- identical(lyr$aes_params$colour, "#D62728")
      lyr$aes_params$fontface      <- if (es_filas) "bold" else "plain"
      lyr$aes_params$segment.alpha <- 0.5
      lyr$aes_params$segment.size  <- 0.1
      p$layers[[i]] <- lyr
    }
  }
  list(plot = p, ca = ca_resultado)
}

run_ca_for_questions <- function(s, qs = paste0("q", 1:9), out_dir = paste0("plots/", year, "/", round_id, "/")) {
  ensure_dir(out_dir)
  stops <- setdiff(stopwords("spanish"), "no")

  for (q in qs) {
    message("Procesando ", q, "...")

    textos <- s[[q]]
    textos[is.na(textos)] <- ""

    corpus <- corpus(
      textos,
      docvars = data.frame(group = s$etiqueta)
    )

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
    if (ncol(matriz) == 0 || nrow(matriz) < 2) {
      warning(q, ": matriz insuficiente; se omite guardado.")
      next
    }
    print(matriz[, 1:min(10, ncol(matriz))])

    res <- build_ca_biplot(matriz, font_family = "montserrat")
    print(res$ca$eig)

    outfile <- paste0(out_dir, "/ca_r11_", q, ".png")
    ggsave(outfile, plot = res$plot, width = 12, height = 8, dpi = 300, bg = "white")
    message("Guardado: ", outfile)
  }
}

run_ca_for_questions(s)
