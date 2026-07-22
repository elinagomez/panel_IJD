library(readxl)
library(readr)
library(dplyr)
library(purrr)
library(writexl)

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

config <- list(
  year = 2026,
  round = "R10",
  base_path = "data/processed",
  transcription_file = "data/processed/transcriptions/output/2026/transcripcion_R10_2.xlsx",
  output_name = "R10_codificada"
)

analysis_dir <- file.path(
  config$base_path, "analysis", config$year, config$round
)
transcription_file <- config$transcription_file
output_xlsx <- file.path(analysis_dir, paste0(config$output_name, ".xlsx"))
output_csv <- file.path(analysis_dir, paste0(config$output_name, ".csv"))

raw <- read_excel(transcription_file, sheet = 1, col_types = "text")

option_maps <- list(
  q3 = c(
    `1` = "Si, estoy muy informado",
    `2` = "Si, estoy algo informado",
    `3` = "Escuché del programa pero no sé de que se trata",
    `4` = "No he escuchado nada al respecto",
    `5` = "No contesta"
  ),
  q8 = c(
    "Si" = "Sí",
    "Sí" = "Sí",
    "Más o menos" = "Más o menos",
    "No" = "No",
    "No lo sé" = "No lo sé"
  ),
  q9 = c(
    "Si" = "Sí",
    "Sí" = "Sí",
    "Más o menos" = "Más o menos",
    "No" = "No",
    "No lo sé" = "No lo sé"
  ),
  q11 = c(
    "Muy de acuerdo" = "Muy de acuerdo",
    "De acuerdo" = "De acuerdo",
    "Ni acuerdo ni desacuerdo" = "Ni de acuerdo ni en desacuerdo",
    "Ni de acuerdo ni en desacuerdo" = "Ni de acuerdo ni en desacuerdo",
    "En desacuerdo" = "En desacuerdo",
    "Muy en desacuerdo" = "Muy en desacuerdo",
    "No sé" = "No sé"
  )
)

recode_option_values <- function(x, mapping) {
  x_chr <- as.character(x)
  x_trim <- trimws(x_chr)
  mapped <- unname(mapping[x_trim])
  ifelse(!is.na(mapped), mapped, x_chr)
}

if (!("numero" %in% names(raw))) {
  stop("La base original no contiene la columna 'numero'.")
}

if (anyDuplicated(raw$numero) > 0) {
  stop("La columna 'numero' no es unica en la base original.")
}

outputs <- list(
  q1 = list(
    file = file.path(analysis_dir, "educacion_publica_valoraciones_q1.csv"),
    cols = "codigos_q1"
  ),
  q2 = list(
    file = file.path(analysis_dir, "papel_educacion_q2.csv"),
    cols = "codigos_q2"
  ),
  q4 = list(
    file = file.path(analysis_dir, "conocimiento_mas_barrio_q4.csv"),
    cols = "codigos_q4"
  ),
  q5 = list(
    file = file.path(analysis_dir, "acciones_minterior_mas_barrio_q5.csv"),
    cols = "codigos_q5"
  ),
  q6 = list(
    file = file.path(analysis_dir, "riesgos_minterior_mas_barrio_q6.csv"),
    cols = "codigos_q6"
  ),
  q7 = list(
    file = file.path(analysis_dir, "opinion_mas_barrio_q7.csv"),
    cols = "codigos_q7"
  ),
  q10 = list(
    file = file.path(analysis_dir, "corrupcion_policial_seguridad_q10.csv"),
    cols = "codigos_q10"
  ),
  q12 = list(
    file = file.path(analysis_dir, "preocupaciones_tecnologias_seguridad_q12.csv"),
    cols = "codigos_q12"
  )
)

extra_cols_by_question <- map(outputs, "cols")

consolidada <- reduce(outputs, function(df, spec) {
  if (!file.exists(spec$file)) {
    stop("No se encontro el archivo de salida: ", spec$file)
  }

  codificada <- read_csv(spec$file, show_col_types = FALSE, col_types = cols(.default = "c"))

  faltantes <- setdiff(c("numero", spec$cols), names(codificada))
  if (length(faltantes) > 0) {
    stop(
      "Faltan columnas en ", basename(spec$file), ": ",
      paste(faltantes, collapse = ", ")
    )
  }

  if (anyDuplicated(codificada$numero) > 0) {
    stop("La columna 'numero' no es unica en ", basename(spec$file))
  }

  left_join(df, select(codificada, numero, all_of(spec$cols)), by = "numero")
}, .init = raw)

for (question in names(option_maps)) {
  if (question %in% names(consolidada)) {
    consolidada[[question]] <- recode_option_values(
      consolidada[[question]],
      option_maps[[question]]
    )
  }
}

question_cols <- paste0("q", 1:12)
pre_question_cols <- names(raw)[seq_len(match("q1", names(raw)) - 1L)]

ordered_cols <- pre_question_cols
for (q in question_cols) {
  ordered_cols <- c(
    ordered_cols,
    q,
    extra_cols_by_question[[q]] %||% character()
  )
}

missing_ordered <- setdiff(ordered_cols, names(consolidada))
if (length(missing_ordered) > 0) {
  stop(
    "Faltan columnas al reordenar la base consolidada: ",
    paste(missing_ordered, collapse = ", ")
  )
}

consolidada <- consolidada |>
  select(all_of(ordered_cols))

write_csv(consolidada, output_csv, na = "")
write_xlsx(
  x = setNames(list(consolidada), config$output_name),
  path = output_xlsx
)

codigo_cols <- unlist(extra_cols_by_question, use.names = FALSE)
faltantes_codigos <- colSums(is.na(consolidada[codigo_cols]) | consolidada[codigo_cols] == "")

cat("Archivos generados:\n")
cat("- ", output_csv, "\n", sep = "")
cat("- ", output_xlsx, "\n", sep = "")
cat("Filas: ", nrow(consolidada), "\n", sep = "")
cat("Columnas: ", ncol(consolidada), "\n\n", sep = "")
cat("Respuestas sin codigo por columna:\n")
print(faltantes_codigos)
