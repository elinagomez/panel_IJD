library(dplyr)
library(readr)
library(stringr)
library(quanteda)
library(FactoMineR)
library(factoextra)
library(ggrepel)
library(sysfonts)
library(showtext)

year <- 2026
round_id <- "R2"

font_family <- "montserrat"
tryCatch({
  font_add_google("Montserrat", "montserrat")
}, error = function(e) {
  message("No se pudo descargar Montserrat; se usa sans.")
  font_family <<- "sans"
})
showtext_auto()

path_new <- paste0("data/processed/transcriptions/output/", year, "/transcripcion_", round_id, "_2.csv")
path_base <- paste0("data/processed/transcriptions/output/", year, "/transcripcion_", round_id, ".csv")
path <- if (file.exists(path_new)) path_new else path_base
s <- read_csv(path)

ensure_dir <- function(path) if (!dir.exists(path)) dir.create(path, recursive = TRUE)

build_ca_biplot <- function(matriz, font_family = font_family) {
  ca_resultado <- CA(matriz, graph = FALSE)

  ndim_posible <- min(nrow(matriz) - 1, ncol(matriz) - 1)
  if (is.na(ndim_posible) || ndim_posible < 2) {
    warning("Biplot no disponible: menos de 2 dimensiones (", ndim_posible, ")")
    return(list(plot = NULL, ca = ca_resultado))
  }

  p <- tryCatch({
    fviz_ca_biplot(
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
      theme_minimal(base_family = font_family) +
      theme(
        text = element_text(family = font_family),
        axis.title = element_text(family = font_family),
        axis.text = element_text(family = font_family),
        plot.title = element_blank()
      )
  }, error = function(e) {
    warning("No se puede dibujar biplot: ", conditionMessage(e))
    NULL
  })

  if (!is.null(p)) {
    for (i in seq_along(p$layers)) {
      lyr <- p$layers[[i]]
      if (inherits(lyr$geom, "GeomTextRepel")) {
        es_filas <- identical(lyr$aes_params$colour, "#D62728")
        lyr$aes_params$fontface <- if (es_filas) "bold" else "plain"
        lyr$aes_params$segment.alpha <- 0.5
        lyr$aes_params$segment.size <- 0.1
        lyr$aes_params$family <- font_family
        p$layers[[i]] <- lyr
      }
    }
  }

  list(plot = p, ca = ca_resultado)
}

run_ca_for_questions <- function(s, qs = c("q2", "q3"), out_dir = paste0("plots/", year, "/", round_id, "/")) {
  ensure_dir(out_dir)
  stops <- setdiff(stopwords("spanish"), "no")

  qs_presentes <- intersect(qs, names(s))
  qs_faltantes <- setdiff(qs, names(s))
  if (length(qs_faltantes) > 0) {
    message("Omitiendo columnas inexistentes: ", paste(qs_faltantes, collapse = ", "))
  }

  for (q in qs_presentes) {
    message("Procesando ", q, "...")

    textos <- s[[q]]
    if (length(textos) == 0) {
      message(q, ": columna vacia; se omite.")
      next
    }
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

    dfm_agrupado <- dfm_group(dfm, groups = docvars(dfm, "group"))
    matriz <- as.matrix(dfm_agrupado)

    print(dim(matriz))
    if (ncol(matriz) < 2 || nrow(matriz) < 2) {
      warning(q, ": matriz insuficiente; se omite guardado.")
      next
    }
    print(matriz[, 1:min(10, ncol(matriz))])

    res <- build_ca_biplot(matriz, font_family = font_family)
    print(res$ca$eig)

    if (is.null(res$plot)) {
      warning(q, ": sin biplot (dimensiones insuficientes); no se guarda imagen.")
      next
    }

    outfile <- paste0(out_dir, "/ca_r2_", q, ".png")
    ggsave(outfile, plot = res$plot, width = 12, height = 8, dpi = 300, bg = "white")
    message("Guardado: ", outfile)
  }
}

run_ca_for_questions(s)
