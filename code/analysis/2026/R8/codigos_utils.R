library(readr)
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

build_system_prompt <- function(config, categoria, codigos_categoria_df) {
  definiciones <- paste0(
    "- ", codigos_categoria_df$codigo, ": ", codigos_categoria_df$descripcion,
    collapse = "\n"
  )

  instrucciones_generales <- config$instrucciones_generales %||% character()

  instrucciones_categoria <- character()
  if (!is.null(config$instrucciones_categoria) &&
      !is.null(config$instrucciones_categoria[[categoria]])) {
    instrucciones_categoria <- config$instrucciones_categoria[[categoria]]
  }

  instrucciones <- c(
    paste0("Tu tarea es clasificar solo la dimensión '", categoria, "'."),
    "No clasifiques otras dimensiones ni devuelvas mas de un codigo.",
    if (!is.null(config$pregunta_cerrada)) {
      paste0(
        "Cuando la entrada incluya la respuesta cerrada previa, usala solo como ",
        "contexto adicional para interpretar la respuesta abierta."
      )
    },
    instrucciones_generales,
    instrucciones_categoria,
    if (any(grepl("No aplica", codigos_categoria_df$codigo, fixed = TRUE))) {
      "Si la respuesta no menciona esta dimension de forma reconocible, usa el codigo de no aplica correspondiente."
    },
    if (any(grepl("No sabe", codigos_categoria_df$codigo, fixed = TRUE))) {
      "Si la respuesta es evasiva, no opina o no aporta una razon reconocible, usa el codigo de no sabe o no aplica disponible en esta dimension."
    }
  )

  instrucciones <- instrucciones[!is.na(instrucciones) & nzchar(instrucciones)]
  bloque_instrucciones <- paste0("- ", instrucciones, collapse = "\n")

  prompt_partes <- c(
    paste0("Developer: Eres un clasificador de ", config$contexto_abierto, "."),
    if (!is.null(config$contexto_cerrado)) {
      paste0("La pregunta cerrada previa relevante es: ", config$contexto_cerrado)
    },
    paste0(
      "Analiza cada comentario recibido y asignale exactamente una de las ",
      "siguientes categorias, utilizando ese valor como el unico posible para el ",
      "campo obligatorio 'codigo':"
    ),
    definiciones,
    "## Instrucciones",
    bloque_instrucciones,
    paste0(
      "Selecciona solo uno de estos valores y responde unicamente con un objeto JSON. ",
      "Asegurate de incluir solo la clave 'codigo' con uno de los valores listados."
    ),
    "## Formato de salida",
    paste0('{"codigo": "', codigos_categoria_df$codigo[[1]], '"}')
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

run_multicategory_coding <- function(config) {
  analysis_dir <- paste0(config$base_path, "/analysis/", config$year, "/", config$round)
  dir.create(analysis_dir, recursive = TRUE, showWarnings = FALSE)

  infile <- paste0(
    config$base_path, "/transcriptions/output/", config$year,
    "/transcripcion_", config$round, ".csv"
  )
  codebook_file <- paste0(
    config$base_path, "/analysis/", config$year, "/", config$round,
    "/codigos_", config$pregunta, ".csv"
  )
  outfile <- paste0(
    config$base_path, "/analysis/", config$year, "/", config$round, "/",
    config$tema, "_", config$pregunta, ".csv"
  )

  raw <- read_csv(infile, show_col_types = FALSE)
  codigos_df <- read_csv(codebook_file, show_col_types = FALSE)

  if (!identical(names(codigos_df), c("codigo", "descripcion", "categoria"))) {
    stop(
      "El archivo ", basename(codebook_file),
      " debe tener exactamente las columnas: codigo, descripcion, categoria"
    )
  }

  if (!(config$pregunta %in% names(raw))) {
    stop("No existe la columna abierta ", config$pregunta, " en ", basename(infile))
  }

  if (!is.null(config$pregunta_cerrada) && !(config$pregunta_cerrada %in% names(raw))) {
    stop("No existe la columna cerrada ", config$pregunta_cerrada, " en ", basename(infile))
  }

  categorias_orden <- unique(codigos_df$categoria)

  faltan_campos <- setdiff(categorias_orden, names(config$category_to_field))
  sobran_campos <- setdiff(names(config$category_to_field), categorias_orden)

  if (length(faltan_campos) > 0 || length(sobran_campos) > 0) {
    stop(
      "El mapeo categoria -> campo_salida no coincide con el codebook. ",
      "Faltan: ", paste(faltan_campos, collapse = ", "),
      ". Sobran: ", paste(sobran_campos, collapse = ", ")
    )
  }

  codigos_por_categoria <- split(codigos_df, codigos_df$categoria)
  codigos_por_categoria <- codigos_por_categoria[categorias_orden]

  prompts <- setNames(
    map(categorias_orden, ~ build_system_prompt(config, .x, codigos_por_categoria[[.x]])),
    categorias_orden
  )

  for (categoria in categorias_orden) {
    cat("=== PROMPT ", categoria, " ===\n", sep = "")
    cat(prompts[[categoria]])
    cat("\n\n")
  }

  clasificar_categoria <- function(texto_abierto, respuesta_cerrada, chat, categoria) {
    if (texto_no_util(texto_abierto)) {
      return(NA_character_)
    }

    entrada <- build_input_text(config, texto_abierto, respuesta_cerrada)
    categorias_validas <- codigos_por_categoria[[categoria]]$codigo

    tryCatch({
      resultado <- chat$chat_structured(
        entrada,
        type = type_object(codigo = type_enum(values = categorias_validas))
      )
      resultado$codigo
    }, error = function(e) {
      NA_character_
    })
  }

  bloques <- split(raw, (seq_len(nrow(raw)) - 1L) %/% (config$chunk_size %||% 20L))

  clasificacion_results <- raw

  for (categoria in categorias_orden) {
    field_name <- unname(config$category_to_field[[categoria]])

    cat("=== PROCESANDO CATEGORIA ", categoria, " -> ", field_name, " ===\n", sep = "")

    chat <- chat_openai(
      model = config$model %||% "gpt-5.4-mini",
      base_url = "https://api.openai.com/v1",
      api_key = Sys.getenv("OPENAI_API_KEY"),
      system_prompt = prompts[[categoria]]
    )

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
          function(texto_abierto, respuesta_cerrada) {
            clasificar_categoria(texto_abierto, respuesta_cerrada, chat, categoria)
          },
          .progress = TRUE
        )
      )
    })

    clasificacion_results[[field_name]] <- clasificacion_col[[field_name]]

    rm(chat)
    invisible(gc())
  }

  for (categoria in categorias_orden) {
    field_name <- unname(config$category_to_field[[categoria]])
    categorias_validas <- codigos_por_categoria[[categoria]]$codigo
    codigos_invalidos <- setdiff(
      unique(stats::na.omit(clasificacion_results[[field_name]])),
      categorias_validas
    )

    if (length(codigos_invalidos) > 0) {
      stop(
        "Se generaron codigos fuera del codebook en ", field_name, ": ",
        paste(codigos_invalidos, collapse = ", ")
      )
    }
  }

  write_csv(clasificacion_results, outfile)

  cat("=== RESUMEN FINAL ===\n")
  for (categoria in categorias_orden) {
    field_name <- unname(config$category_to_field[[categoria]])
    cat("distribucion de codigos (", field_name, "):\n", sep = "")
    print(table(clasificacion_results[[field_name]], useNA = "ifany"))
    cat("\n")
  }
  cat("archivo guardado en: ", outfile, "\n", sep = "")
}
