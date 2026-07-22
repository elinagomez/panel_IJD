library(readr)
library(readxl)
library(dplyr)
library(purrr)
library(ellmer)

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

texto_no_util <- function(texto) {
  if (is.null(texto) || length(texto) == 0 || all(is.na(texto))) {
    return(TRUE)
  }

  texto_limpio <- trimws(as.character(texto[[1]]))

  if (!nzchar(texto_limpio)) {
    return(TRUE)
  }

  texto_min <- tolower(texto_limpio)

  if (texto_min %in% c("na", "n/a", "...", "…")) {
    return(TRUE)
  }

  grepl("^[[:space:][:punct:]]+$", texto_limpio)
}

normalizar_respuesta_cerrada <- function(respuesta, mapa = NULL) {
  if (texto_no_util(respuesta)) {
    return(NULL)
  }

  valor <- trimws(as.character(respuesta[[1]]))

  if (!is.null(mapa)) {
    if (valor %in% names(mapa)) {
      return(unname(mapa[[valor]]))
    }

    valor_up <- toupper(valor)
    if (valor_up %in% names(mapa)) {
      return(unname(mapa[[valor_up]]))
    }
  }

  valor
}

colapsar_codigos <- function(codigos) {
  codigos <- unlist(codigos, use.names = FALSE)
  codigos <- unique(trimws(as.character(codigos)))
  codigos <- codigos[!is.na(codigos) & nzchar(codigos)]

  if (length(codigos) == 0) {
    return(NA_character_)
  }

  paste(codigos, collapse = "; ")
}

separar_codigos <- function(codigos_colapsados) {
  codigos <- unlist(strsplit(codigos_colapsados, "; ", fixed = TRUE), use.names = FALSE)
  codigos <- trimws(codigos)
  codigos[!is.na(codigos) & nzchar(codigos)]
}

resumir_codigos <- function(clasificacion_results, field_name) {
  valores <- clasificacion_results[[field_name]]
  valores <- valores[!is.na(valores)]

  if (length(valores) == 0) {
    return(list(
      por_codigo = tibble(codigo = character(), n = integer()),
      por_n_codigos = tibble(n_codigos = integer(), n_respuestas = integer())
    ))
  }

  codigos_por_respuesta <- map(valores, separar_codigos)

  por_codigo <- tibble(codigo = unlist(codigos_por_respuesta, use.names = FALSE)) |>
    count(codigo, name = "n", sort = TRUE)

  por_n_codigos <- tibble(n_codigos = map_int(codigos_por_respuesta, length)) |>
    count(n_codigos, name = "n_respuestas", sort = FALSE)

  list(
    por_codigo = por_codigo,
    por_n_codigos = por_n_codigos
  )
}

build_system_prompt <- function(config, codigos_df) {
  definiciones <- paste0(
    "- ", codigos_df$codigo, ": ", codigos_df$descripcion,
    collapse = "\n"
  )

  instrucciones_extra <- config$instrucciones_extra %||% character()

  instrucciones <- c(
    "Lee la respuesta completa antes de clasificar.",
    "Puedes asignar uno o mas codigos si la respuesta contiene mas de una idea sustantiva del codebook.",
    "No inventes codigos: usa exclusivamente valores listados en el codebook.",
    "Si una respuesta encaja en varias categorias, devuelve todas las categorias pertinentes.",
    "Si una respuesta tiene un eje dominante y una mencion secundaria clara, incluye ambas.",
    "Si la respuesta es evasiva, tautologica, no responde la pregunta o no aporta contenido clasificable, usa el codigo de no respuesta/no aplica/desconocimiento disponible en el codebook.",
    if (!is.null(config$pregunta_cerrada)) {
      paste0(
        "La respuesta cerrada previa se entrega como contexto. Usala para interpretar ",
        "respuestas abiertas breves o elipticas, pero los codigos deben clasificar la razon abierta."
      )
    },
    instrucciones_extra
  )

  instrucciones <- instrucciones[!is.na(instrucciones) & nzchar(instrucciones)]
  bloque_instrucciones <- paste0("- ", instrucciones, collapse = "\n")

  prompt_partes <- c(
    paste0("Developer: Eres un clasificador de ", config$contexto_abierto, "."),
    if (!is.null(config$contexto_cerrado)) {
      paste0("La pregunta cerrada previa relevante es: ", config$contexto_cerrado)
    },
    paste0(
      "Analiza cada comentario recibido y asignale todos los codigos pertinentes, ",
      "utilizando solo los siguientes codigos para los campos de salida:"
    ),
    definiciones,
    "## Instrucciones",
    bloque_instrucciones,
    paste0(
      "Responde unicamente con un objeto JSON. Incluye solo la clave 'codigos', ",
      "cuyo valor debe ser un array con uno o mas codigos listados."
    ),
    "## Formato de salida",
    paste0('{"codigos": ["', codigos_df$codigo[[1]], '"]}')
  )

  paste(prompt_partes[!is.na(prompt_partes)], collapse = "\n\n")
}

build_input_text <- function(config, texto_abierto, respuesta_cerrada = NULL) {
  partes <- character()

  if (!is.null(config$pregunta_cerrada)) {
    respuesta_cerrada_legible <- normalizar_respuesta_cerrada(
      respuesta_cerrada,
      config$closed_value_map %||% NULL
    )

    if (!is.null(respuesta_cerrada_legible) && nzchar(respuesta_cerrada_legible)) {
      partes <- c(
        partes,
        paste0("PREGUNTA CERRADA PREVIA: ", config$contexto_cerrado),
        paste0("RESPUESTA CERRADA SELECCIONADA: ", respuesta_cerrada_legible)
      )
    }
  }

  partes <- c(
    partes,
    paste0("PREGUNTA ABIERTA: ", config$contexto_abierto),
    paste0("RESPUESTA ABIERTA: ", trimws(as.character(texto_abierto[[1]])))
  )

  paste(partes, collapse = "\n\n")
}

run_multicode_coding <- function(config) {
  analysis_dir <- file.path(config$base_path, "analysis", config$year, config$round)
  dir.create(analysis_dir, recursive = TRUE, showWarnings = FALSE)

  infile <- file.path(
    config$base_path, "transcriptions", "output", config$year,
    paste0("transcripcion_", config$round, ".xlsx")
  )
  codebook_file <- file.path(
    analysis_dir,
    paste0("codigos_", config$pregunta, ".csv")
  )
  outfile <- file.path(
    analysis_dir,
    paste0(config$tema, "_", config$pregunta, ".csv")
  )
  error_file <- file.path(
    analysis_dir,
    paste0("errores_codificacion_", config$pregunta, ".txt")
  )

  raw <- read_excel(infile, sheet = config$sheet %||% 1, col_types = "text")
  codigos_df <- read_csv(codebook_file, show_col_types = FALSE)

  if (!identical(names(codigos_df), c("codigo", "descripcion"))) {
    stop(
      "El archivo ", basename(codebook_file),
      " debe tener exactamente las columnas: codigo, descripcion"
    )
  }

  if (!(config$pregunta %in% names(raw))) {
    stop("No existe la columna abierta ", config$pregunta, " en ", basename(infile))
  }

  if (!is.null(config$pregunta_cerrada) && !(config$pregunta_cerrada %in% names(raw))) {
    stop("No existe la columna cerrada ", config$pregunta_cerrada, " en ", basename(infile))
  }

  categorias <- codigos_df$codigo
  field_name <- config$campo_salida %||% paste0("codigos_", config$pregunta)
  system_prompt <- build_system_prompt(config, codigos_df)

  cat("=== PROMPT ", config$pregunta, " ===\n", sep = "")
  cat(system_prompt)
  cat("\n\n")

  api_key <- trimws(Sys.getenv("OPENAI_API_KEY"))
  if (!nzchar(api_key)) {
    stop("OPENAI_API_KEY no esta definida en esta sesion de R.")
  }

  model <- config$model %||% "gpt-5.4-mini"

  cat("modelo: ", model, "\n\n", sep = "")

  chat <- chat_openai(
    model = model,
    base_url = "https://api.openai.com/v1",
    api_key = api_key,
    system_prompt = system_prompt
  )

  errores <- new.env(parent = emptyenv())
  errores$mensajes <- character()

  registrar_error <- function(e) {
    errores$mensajes <- c(errores$mensajes, conditionMessage(e))
    NA_character_
  }

  clasificar_texto <- function(texto_abierto, respuesta_cerrada = NULL) {
    if (texto_no_util(texto_abierto)) {
      return(NA_character_)
    }

    entrada <- build_input_text(config, texto_abierto, respuesta_cerrada)

    tryCatch({
      output_type <- type_object(
        codigos = type_array(
          type_enum(values = categorias)
        )
      )

      resultado <- chat$chat_structured(
        entrada,
        type = output_type
      )

      colapsar_codigos(resultado$codigos)
    }, error = function(e) {
      registrar_error(e)
    })
  }

  bloques <- split(raw, (seq_len(nrow(raw)) - 1L) %/% (config$chunk_size %||% 20L))

  clasificacion_col <- map_dfr(bloques, function(bloque) {
    respuestas_cerradas <- if (!is.null(config$pregunta_cerrada)) {
      bloque[[config$pregunta_cerrada]]
    } else {
      rep(NA_character_, nrow(bloque))
    }

    tibble(
      !!field_name := map2_chr(
        bloque[[config$pregunta]],
        respuestas_cerradas,
        clasificar_texto,
        .progress = TRUE
      )
    )
  })

  clasificacion_results <- raw
  clasificacion_results[[field_name]] <- clasificacion_col[[field_name]]

  respuestas_utiles <- !map_lgl(raw[[config$pregunta]], texto_no_util)
  n_utiles <- sum(respuestas_utiles)
  n_fallidas <- sum(is.na(clasificacion_results[[field_name]]) & respuestas_utiles)

  if (length(errores$mensajes) > 0) {
    errores_unicos <- unique(errores$mensajes)
    writeLines(
      c(
        paste0("pregunta: ", config$pregunta),
        paste0("modelo: ", model),
        paste0("respuestas utiles fallidas: ", n_fallidas, " de ", n_utiles),
        "",
        "errores unicos:",
        paste0("- ", errores_unicos)
      ),
      error_file
    )
    cat("=== ERRORES DE CLASIFICACION ===\n")
    cat("respuestas utiles fallidas: ", n_fallidas, " de ", n_utiles, "\n", sep = "")
    cat("primeros errores unicos:\n")
    cat(paste0("- ", head(errores_unicos, 5), collapse = "\n"))
    cat("\n")
    cat("log guardado en: ", error_file, "\n\n", sep = "")
  }

  if (n_utiles > 0 && n_fallidas == n_utiles) {
    stop(
      "Todas las respuestas utiles fallaron durante la clasificacion. ",
      "Revisa los errores impresos arriba antes de usar el archivo de salida."
    )
  }

  codigos_generados <- clasificacion_results[[field_name]]
  codigos_generados <- codigos_generados[!is.na(codigos_generados)]
  codigos_generados <- unique(unlist(map(codigos_generados, separar_codigos), use.names = FALSE))
  codigos_invalidos <- setdiff(codigos_generados, categorias)

  if (length(codigos_invalidos) > 0) {
    stop(
      "Se generaron codigos fuera del codebook en ", field_name, ": ",
      paste(codigos_invalidos, collapse = ", ")
    )
  }

  write_csv(clasificacion_results, outfile, na = "")

  resumen <- resumir_codigos(clasificacion_results, field_name)

  cat("=== RESUMEN FINAL ===\n")
  cat("respuestas procesadas: ", nrow(clasificacion_results), "\n", sep = "")
  cat("respuestas utiles: ", n_utiles, "\n", sep = "")
  cat("respuestas sin codigo: ", sum(is.na(clasificacion_results[[field_name]])), "\n\n", sep = "")

  cat("frecuencia por codigo individual (", field_name, "):\n", sep = "")
  print(resumen$por_codigo, n = Inf)
  cat("\nfrecuencia por cantidad de codigos asignados:\n")
  print(resumen$por_n_codigos, n = Inf)
  cat("\n")
  cat("archivo guardado en: ", outfile, "\n", sep = "")
}
