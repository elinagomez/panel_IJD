library(readxl)
library(readr)
library(dplyr)
library(purrr)
library(tidyr)

config <- list(
  analysis_dir = file.path("data", "processed", "analysis", "2026", "R10"),
  base_xlsx = file.path("data", "processed", "analysis", "2026", "R10", "R10_codificada.xlsx"),
  output_csv = file.path("data", "processed", "analysis", "2026", "R10", "R10_muestra_verificacion_codificacion.csv"),
  seed = 20260502,
  sample_share = 0.2
)

questions <- c("q1", "q2", "q4", "q5", "q6", "q7", "q10", "q12")

split_codes <- function(x) {
  if (is.na(x) || !nzchar(trimws(x))) {
    return(character())
  }

  out <- unlist(strsplit(x, ";", fixed = TRUE), use.names = FALSE)
  out <- trimws(out)
  out[nzchar(out)]
}

read_codebook <- function(q) {
  read_csv(
    file.path(config$analysis_dir, paste0("codigos_", q, ".csv")),
    show_col_types = FALSE,
    col_types = cols(.default = "c")
  ) |>
    select(codigo, descripcion)
}

base <- read_excel(config$base_xlsx, sheet = 1, col_types = "text")
set.seed(config$seed)

muestra <- map_dfr(questions, function(q) {
  code_col <- paste0("codigos_", q)
  codebook <- read_codebook(q)

  universe <- base |>
    mutate(row_id = row_number()) |>
    transmute(
      row_id,
      numero,
      segmento,
      voto2,
      pregunta = q,
      respuesta = .data[[q]],
      codigos_asignados = .data[[code_col]]
    ) |>
    filter(!is.na(codigos_asignados), nzchar(trimws(codigos_asignados)))

  n_sample <- ceiling(nrow(universe) * config$sample_share)

  universe |>
    slice_sample(n = n_sample) |>
    arrange(row_id) |>
    mutate(
      codigos_vector = map(codigos_asignados, split_codes),
      definiciones_codigos = map_chr(codigos_vector, function(codes) {
        defs <- codebook$descripcion[match(codes, codebook$codigo)]
        paste(paste0(codes, " = ", defs), collapse = " | ")
      }),
      veredicto = NA_character_,
      observacion = NA_character_,
      codigos_sugeridos = NA_character_
    ) |>
    select(
      pregunta,
      row_id,
      numero,
      segmento,
      voto2,
      respuesta,
      codigos_asignados,
      definiciones_codigos,
      veredicto,
      observacion,
      codigos_sugeridos
    )
})

write_csv(muestra, config$output_csv, na = "")

cat("Archivo generado: ", config$output_csv, "\n", sep = "")
cat("Semilla: ", config$seed, "\n", sep = "")
cat("Muestra total: ", nrow(muestra), "\n", sep = "")
print(count(muestra, pregunta, name = "n_muestra"))
