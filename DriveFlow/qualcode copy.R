library(readr)
library(readxl)
library(dplyr)
library(purrr)
library(ellmer)
library(googledrive)
library(googlesheets4)
library(tidyverse)
library(glue)
library(yaml)

# =========================================================
# UTILIDADES
# =========================================================

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# Combina project.yml (parámetros compartidos por todo el proyecto: cuenta
# de Drive, carpeta de "questions", modelo/proveedor por defecto) con el yml
# de la ronda puntual (DriveFlow/<ronda>.yml). Lo de la ronda tiene prioridad,
# así una ronda puede pisar el modelo/proveedor por defecto si hace falta.
load_config <- function(round_file, project_file = "project.yml") {

  project_cfg <- if (file.exists(project_file)) yaml::read_yaml(project_file) else list()
  round_cfg   <- yaml::read_yaml(round_file)

  base_cfg <- c(
    project_cfg$codificacion %||% list(),
    list(questions_url = project_cfg$drive$questions_url %||% NULL)
  )

  modifyList(base_cfg, round_cfg)
}

texto_no_util <- function(texto) {
  if (is.null(texto) || length(texto) == 0 || all(is.na(texto))) {
    return(TRUE)
  }

  texto_limpio <- trimws(as.character(texto[[1]]))

  if (!nzchar(texto_limpio)) return(TRUE)

  texto_min <- tolower(texto_limpio)

  if (texto_min %in% c("na", "n/a", "...", "…")) return(TRUE)

  grepl("^[[:space:][:punct:]]+$", texto_limpio)
}

colapsar_codigos <- function(codigos) {
  codigos <- unlist(codigos, use.names = FALSE)
  codigos <- unique(trimws(as.character(codigos)))
  codigos <- codigos[!is.na(codigos) & nzchar(codigos)]
  codigos <- head(codigos, 3)

  if (length(codigos) == 0) return(NA_character_)

  paste(codigos, collapse = "; ")
}

# =========================================================
# LECTURA DE SHEETS
# =========================================================

read_spreadsheet <- function(folder_url, file_name, sheet = NULL) {

  folder <- googledrive::drive_get(googledrive::as_id(folder_url))

  if (nrow(folder) == 0) {
    stop(glue("No se encontró la carpeta en Drive."))
  }

  ss <- googledrive::drive_ls(path = googledrive::as_id(folder_url)) |>
    filter(name == file_name)

  if (nrow(ss) == 0) {
    stop(glue("No se encontró '{file_name}' en Drive."))
  }

  if (nrow(ss) > 1) {
    stop(glue("Se encontraron varios archivos con el nombre '{file_name}'."))
  }

  ss_id <- ss$id[[1]]

  message(glue("Archivo encontrado: '{file_name}'"))

  if (is.null(sheet)) {
    df <- googlesheets4::read_sheet(googledrive::as_id(ss_id))
  } else {
    df <- googlesheets4::read_sheet(googledrive::as_id(ss_id), sheet = sheet)
  }

  message(glue("Archivo cargado: '{file_name}'"))
  df
}

# =========================================================
# CONFIGURACIÓN POR PREGUNTA
# =========================================================

make_codebook <- function(codebook, columna = pregunta) {
  codebook |>
    mutate(
      etiqueta = str_replace_all(etiqueta, '"', ""),
      etiqueta = str_replace_all(etiqueta, "[\\n\\r]", " "),
      etiqueta = str_squish(etiqueta),
      descripcion = str_squish(as.character(descripcion))
    ) |>
    group_by({{ columna }}) |>
    nest() |>
    deframe()
}

detect_open <- function(raw,
                        questions,
                        codebook,
                        col_id = Pregunta_id,
                        col_tipo = Tipo,
                        col_codebook = pregunta,
                        value = "Abierta") {

  preguntas_codebook <- codebook |>
    pull({{ col_codebook }}) |>
    as.character() |>
    unique()

  questions |>
    filter({{ col_tipo }} == value) |>
    pull({{ col_id }}) |>
    as.character() |>
    intersect(names(raw)) |>
    intersect(preguntas_codebook)
}

parse_categorias <- function(x) {

  if (is.na(x) || str_squish(x) == "") {
    return(character(0))
  }

  str_split(as.character(x), "\n|;")[[1]] |>
    str_squish() |>
    discard(~ .x == "") |>
    str_replace("^[A-Za-z0-9]+[:.=]\\s*(.*)$", "\\1")
}

make_cfg <- function(raw, questions, codebook) {

  dic <- make_codebook(codebook = codebook)

  open_q <- detect_open(
    raw = raw,
    questions = questions,
    codebook = codebook
  )

  config_q <- function(q) {

    meta <- questions |>
      filter(Pregunta_id == q)

    if (nrow(meta) != 1) {
      stop(glue("Pregunta {q} tiene {nrow(meta)} filas en questions"))
    }

    dict <- dic[[q]]

    if (is.null(dict)) {
      stop(glue("Pregunta {q} no tiene diccionario en codebook"))
    }

    dep <- meta$Dependencia[[1]]

    if (!is.na(dep) && str_squish(dep) != "") {

      meta_dep <- questions |>
        filter(Pregunta_id == dep)

      if (nrow(meta_dep) != 1) {
        stop(glue("La dependencia {dep} de {q} tiene {nrow(meta_dep)} filas"))
      }

      texto_dependencia <- meta_dep$Pregunta[[1]]
      categorias_dependencia <- parse_categorias(meta_dep$Categorias[[1]])

    } else {

      dep <- NA_character_
      texto_dependencia <- NA_character_
      categorias_dependencia <- character(0)
    }

    list(
      pregunta = q,
      tema = meta$Subtema[[1]],
      texto = meta$Pregunta[[1]],
      dependencia = dep,
      pregunta_cerrada = texto_dependencia,
      categorias_cerradas = categorias_dependencia,
      diccionario = dict,
      etiquetas = dict$etiqueta |> unique() |> discard(is.na)
    )
  }

  open_q |>
    set_names() |>
    map(config_q)
}

# =========================================================
# PROMPT FIJO POR PREGUNTA
# =========================================================

make_prompt <- function(cfg) {

  definiciones <- cfg$diccionario |>
    mutate(linea = glue("- {etiqueta}: {descripcion}")) |>
    pull(linea) |>
    paste(collapse = "\n")

  tiene_dependencia <- !is.null(cfg$dependencia) &&
    !is.na(cfg$dependencia) &&
    str_squish(cfg$dependencia) != ""

  categorias_txt <- if (length(cfg$categorias_cerradas) > 0) {
    paste(cfg$categorias_cerradas, collapse = "; ")
  } else {
    "No especificadas"
  }

  contexto_cerrada <- if (tiene_dependencia) {
    glue(
"Pregunta cerrada previa relevante:
{cfg$dependencia} - {cfg$pregunta_cerrada}

Categorías posibles de la pregunta cerrada previa:
{categorias_txt}

La respuesta cerrada previa será entregada junto con cada respuesta abierta. Úsala solo como contexto interpretativo."
    )
  } else {
    "No hay pregunta cerrada previa asociada."
  }

  instrucciones <- c(
    "Lee la respuesta completa antes de clasificar.",
    "Puedes asignar uno o más códigos si la respuesta contiene más de una idea sustantiva del codebook.",
    "No inventes códigos: usa exclusivamente valores listados en el codebook.",
    "Si una respuesta encaja en varias categorías, devuelve todas las categorías pertinentes.",
    "Si una respuesta tiene un eje dominante y una mención secundaria clara, incluye ambas.",
    "Asigna como máximo tres códigos.",
    "Ordena los códigos de mayor a menor relevancia.",
    "No repitas códigos.",
    "Si la respuesta es evasiva, tautológica, no responde la pregunta o no aporta contenido clasificable, usa el código de no respuesta/no aplica/desconocimiento disponible en el codebook.",
    if (tiene_dependencia) {
      "Usa la respuesta cerrada previa para interpretar respuestas abiertas breves o elípticas, pero clasifica la razón expresada en la respuesta abierta."
    }
  )

  bloque_instrucciones <- paste0("- ", instrucciones, collapse = "\n")

  glue(
"Developer: Eres un clasificador de respuestas abiertas de encuesta sobre {cfg$tema}.

Pregunta abierta:
{cfg$pregunta} - {cfg$texto}

Contexto:
{contexto_cerrada}

Analiza cada comentario recibido y asígnale todos los códigos pertinentes, utilizando solo los siguientes códigos:

{definiciones}

## Instrucciones
{bloque_instrucciones}

Responde únicamente con un objeto JSON. Incluye solo la clave 'codigos', cuyo valor debe ser un array con uno, dos o tres códigos listados.

## Formato de salida
{{\"codigos\": [\"{cfg$etiquetas[[1]]}\"]}}"
  )
}

# =========================================================
# INPUT VARIABLE POR RESPUESTA
# =========================================================

build_input_text <- function(cfg, texto_abierto, respuesta_cerrada = NULL) {

  partes <- character()

  tiene_dependencia <- !is.null(cfg$dependencia) &&
    !is.na(cfg$dependencia) &&
    str_squish(cfg$dependencia) != ""

  if (
    tiene_dependencia &&
    !is.null(respuesta_cerrada) &&
    !is.na(respuesta_cerrada) &&
    str_squish(as.character(respuesta_cerrada)) != ""
  ) {
    partes <- c(
      partes,
      paste0("PREGUNTA CERRADA PREVIA: ", cfg$dependencia, " - ", cfg$pregunta_cerrada),
      paste0("RESPUESTA CERRADA SELECCIONADA: ", str_squish(as.character(respuesta_cerrada)))
    )
  }

  partes <- c(
    partes,
    paste0("PREGUNTA ABIERTA: ", cfg$pregunta, " - ", cfg$texto),
    paste0("RESPUESTA ABIERTA: ", str_squish(as.character(texto_abierto)))
  )

  paste(partes, collapse = "\n\n")
}

# =========================================================
# CLASIFICACIÓN
# =========================================================

clasify <- function(chat, cfg, texto, respuesta_cerrada = NULL) {

  if (texto_no_util(texto)) {
    return(NA_character_)
  }

  entrada <- build_input_text(
    cfg = cfg,
    texto_abierto = texto,
    respuesta_cerrada = respuesta_cerrada
  )

  tryCatch({

    out <- chat$chat_structured(
      entrada,
      type = type_object(
        codigos = type_array(
          type_enum(values = cfg$etiquetas)
        )
      )
    )

    colapsar_codigos(out$codigos)

  }, error = function(e) {
    NA_character_
  })
}

build_chat <- function(system_prompt,
                       model,
                       provider = "openai",
                       api_key_env = "OPENAI_API_KEY",
                       ollama_base_url = Sys.getenv("OLLAMA_BASE_URL", "http://localhost:11434")) {

  switch(
    provider,

    "openai" = {
      api_key <- trimws(Sys.getenv(api_key_env))

      if (!nzchar(api_key)) {
        stop(glue("{api_key_env} no está definida."))
      }

      chat_openai(
        model = model,
        base_url = "https://api.openai.com/v1",
        api_key = api_key,
        system_prompt = system_prompt
      )
    },

    "ollama" = {
      # Requiere Ollama corriendo localmente (ollama serve) y el modelo
      # ya descargado, p.ej.: ollama pull qwen2.5:14b
      chat_ollama(
        model = model,
        base_url = ollama_base_url,
        system_prompt = system_prompt
      )
    },

    stop(glue("provider '{provider}' no soportado. Usa 'openai' u 'ollama'."))
  )
}

classify_question <- function(raw,
                              cfg,
                              chunk_size = 20L,
                              model = CONFIG$model,
                              provider = CONFIG$provider %||% "openai") {

  pregunta <- cfg$pregunta
  dependencia <- cfg$dependencia

  system_prompt <- make_prompt(cfg)

  chat <- build_chat(
    system_prompt = system_prompt,
    model = model,
    provider = provider,
    api_key_env = CONFIG$api_key_env %||% "OPENAI_API_KEY",
    ollama_base_url = CONFIG$ollama_base_url %||% Sys.getenv("OLLAMA_BASE_URL", "http://localhost:11434")
  )

  bloques <- split(
    raw,
    (seq_len(nrow(raw)) - 1L) %/% chunk_size
  )

  map_dfr(bloques, function(bloque) {

    respuestas_cerradas <- if (
      !is.na(dependencia) &&
      dependencia %in% names(bloque)
    ) {
      bloque[[dependencia]]
    } else {
      rep(NA_character_, nrow(bloque))
    }

    tibble(
      codigo_temp = map2_chr(
        bloque[[pregunta]],
        respuestas_cerradas,
        ~ clasify(
          chat = chat,
          cfg = cfg,
          texto = .x,
          respuesta_cerrada = .y
        ),
        .progress = TRUE
      )
    )
  }) |>
    pull(codigo_temp)
}




