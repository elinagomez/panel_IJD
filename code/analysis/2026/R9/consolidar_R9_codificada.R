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
  round = "R9",
  base_path = "data/processed",
  output_name = "R9_codificada"
)

analysis_dir <- file.path(
  config$base_path, "analysis", config$year, config$round
)
transcription_file <- file.path(
  config$base_path, "transcriptions", "output", config$year,
  paste0("transcripcion_", config$round, ".xlsx")
)
output_xlsx <- file.path(analysis_dir, paste0(config$output_name, ".xlsx"))
output_csv <- file.path(analysis_dir, paste0(config$output_name, ".csv"))

raw <- read_excel(transcription_file, sheet = 1, col_types = "text")

option_maps <- list(
  q1 = c(
    "Ni uno, ni otro" = "Ni de acuerdo ni en desacuerdo"
  ),
  q5 = c(
    A = "SÍ, me informé bastante",
    B = "Sí, aunque manejo poca información",
    C = "NO",
    `1` = "SÍ, me informé bastante",
    `2` = "Sí, aunque manejo poca información",
    `3` = "NO"
  ),
  q7 = c(
    A = "SÍ, me informé bastante",
    B = "Sí, aunque manejo poca información",
    C = "NO",
    `1` = "SÍ, me informé bastante",
    `2` = "Sí, aunque manejo poca información",
    `3` = "NO"
  )
)

recode_option_letters <- function(x, mapping) {
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
  q2 = list(
    file = file.path(analysis_dir, "politica_desinteres_q2.csv"),
    cols = "codigos_q2"
  ),
  q4 = list(
    file = file.path(analysis_dir, "preocupacion_economica_q4.csv"),
    cols = "codigos_q4"
  ),
  q6 = list(
    file = file.path(analysis_dir, "empleo_priorizado_q6.csv"),
    cols = "codigos_q6"
  ),
  q8 = list(
    file = file.path(analysis_dir, "estrategia_situacion_calle_q8.csv"),
    cols = "codigos_q8"
  ),
  q10 = list(
    file = file.path(analysis_dir, "cooperacion_interinstitucional_q10.csv"),
    cols = "codigos_q10"
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
    consolidada[[question]] <- recode_option_letters(
      consolidada[[question]],
      option_maps[[question]]
    )
  }
}

question_cols <- paste0("q", 1:10)
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

cat("Archivos generados:\n")
cat("- ", output_csv, "\n", sep = "")
cat("- ", output_xlsx, "\n", sep = "")
cat("Filas: ", nrow(consolidada), "\n", sep = "")
cat("Columnas: ", ncol(consolidada), "\n", sep = "")
