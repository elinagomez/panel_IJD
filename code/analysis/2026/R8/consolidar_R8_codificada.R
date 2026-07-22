library(readr)
library(dplyr)
library(purrr)
library(writexl)

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

config <- list(
  year = 2026,
  round = "R8",
  base_path = "data/processed",
  output_name = "R8_codificada"
)

analysis_dir <- paste0(
  config$base_path, "/analysis/", config$year, "/", config$round
)
transcription_file <- paste0(
  config$base_path, "/transcriptions/output/", config$year,
  "/transcripcion_", config$round, ".csv"
)
output_file <- paste0(analysis_dir, "/", config$output_name, ".xlsx")

raw <- read_csv(transcription_file, show_col_types = FALSE)

option_maps <- list(
  q3 = c(
    A = "Está actuando de manera correcta",
    B = "No está actuando de manera correcta",
    C = "No tengo una opinión formada"
  ),
  q5 = c(
    A = "Me resultaron creíbles",
    B = "No me resultaron creíbles",
    C = "No sabría decir"
  ),
  q7 = c(
    A = "en la construcción de nuevas cárceles",
    B = "en la mejora de las cárceles ya existentes",
    C = "tanto en la construcción de cárceles como en la mejora de las ya existentes",
    D = "NO es prioritario que el Estado uruguayo invierta en cárceles."
  ),
  q9 = c(
    A = "Muy importante",
    B = "Algo importante",
    C = "Ni una cosa ni la otra",
    D = "Poco importante",
    E = "Nada importante",
    F = "No tengo opinión"
  ),
  q11 = c(
    A = "Muy importante",
    B = "Algo importante",
    C = "Ni una cosa ni la otra",
    D = "Poco importante",
    E = "Nada importante",
    F = "No tengo opinión"
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
  q1 = list(
    file = paste0(analysis_dir, "/interpelacion_recuerdos_q1.csv"),
    cols = c("nivel_exposicion_q1", "dimension_tematica_q1", "tono_q1")
  ),
  q2 = list(
    file = paste0(analysis_dir, "/debate_liberar_presos_q2.csv"),
    cols = c(
      "postura_general_q2",
      "percepcion_sistema_q2",
      "argumentos_temores_q2",
      "propuestas_solucion_q2"
    )
  ),
  q4 = list(
    file = paste0(analysis_dir, "/accionar_oposicion_q4.csv"),
    cols = c(
      "postura_debate_q4",
      "evaluacion_oposicion_q4",
      "argumentos_justificacion_q4",
      "percepcion_sistema_q4"
    )
  ),
  q6 = list(
    file = paste0(analysis_dir, "/credibilidad_cifras_q6.csv"),
    cols = c(
      "evaluacion_q6",
      "argumentos_desconfianza_q6",
      "argumentos_confianza_q6",
      "influencia_externa_q6",
      "contexto_otros_q6"
    )
  ),
  q8 = list(
    file = paste0(analysis_dir, "/inversion_carceles_q8.csv"),
    cols = c(
      "prioridad_inversion_q8",
      "justificacion_rehabilitacion_q8",
      "justificacion_punitivismo_q8",
      "gestion_recursos_q8"
    )
  ),
  q10 = list(
    file = paste0(analysis_dir, "/importancia_rehabilitacion_q10.csv"),
    cols = c("codigo_q10")
  ),
  q12 = list(
    file = paste0(analysis_dir, "/trabajo_estudio_carcel_q12.csv"),
    cols = c("codigo_q12")
  )
)

extra_cols_by_question <- map(outputs, "cols")

consolidada <- reduce(outputs, function(df, spec) {
  if (!file.exists(spec$file)) {
    stop("No se encontro el archivo de salida: ", spec$file)
  }

  codificada <- read_csv(spec$file, show_col_types = FALSE)

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

write_xlsx(
  x = setNames(list(consolidada), config$output_name),
  path = output_file
)

cat("Archivo generado: ", output_file, "\n", sep = "")
cat("Filas: ", nrow(consolidada), "\n", sep = "")
cat("Columnas: ", ncol(consolidada), "\n", sep = "")
