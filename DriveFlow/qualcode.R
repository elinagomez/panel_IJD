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

`%||%` <- function(x, y) if (is.null(x)) y else x

# Combina project.yml (parámetros compartidos por todo el proyecto) con el
# yml de la ronda puntual. Lo de la ronda tiene prioridad. Ver DriveFlow/qualcode
# copy.R para la versión documentada de esta función.
load_config <- function(round_file, project_file = "project.yml") {

  project_cfg <- if (file.exists(project_file)) yaml::read_yaml(project_file) else list()
  round_cfg   <- yaml::read_yaml(round_file)

  base_cfg <- c(
    project_cfg$codificacion %||% list(),
    list(questions_url = project_cfg$drive$questions_url %||% NULL)
  )

  modifyList(base_cfg, round_cfg)
}

# CONFIG se sobreescribe en run.R con load_config(); estos valores son solo
# un fallback si se corre qualcode.R de forma aislada.
CONFIG <- list(
  round = "R9",
  year = "2026",
  folder_url = "https://drive.google.com/drive/u/0/folders/1xDMAbuDm3NlR_jCAMgwrNSKnbQ-96BTA",
  questions_url = "https://drive.google.com/drive/u/0/folders/1Cmr-xzG4noYTjz36dI3jQNhIuWovg3OR",
  model = "gpt-5-nano",
  key = Sys.getenv("")
)

# Funcion para leer sheets 
read_spreadsheet <- function(folder_url, file_name, sheet = NULL) {
  
  folder <- googledrive::drive_get(googledrive::as_id(folder_url))
    if (nrow(folder) == 0) {
      stop(glue::glue("No se encontró la carpeta en Drive."))
    }

  ss <- googledrive::drive_ls(path = googledrive::as_id(folder_url)) |>
    dplyr::filter(name == file_name)

  if (nrow(ss) == 0) {
    stop(glue::glue("No se encontró '{file_name}' en Drive."))
  }

  if (nrow(ss) > 1) {
    stop(glue::glue("Se encontraron varios archivos con el nombre '{file_name}'."))
  }

  ss_id <- ss$id[[1]]
  message(glue::glue("Archivo encontrado: '{file_name}'"))

  if (is.null(sheet)) {
    df <- googlesheets4::read_sheet(googledrive::as_id(ss_id))
  } else {
    df <- googlesheets4::read_sheet(googledrive::as_id(ss_id), sheet = sheet)
}

message(glue::glue("Archivo cargado: '{file_name}'"))
df
}


# Busca o crea el spreadsheet en la carpeta de Drive indicada


save_drive <- function(out, folder_url, spreadsheet, sheetname) {
  
  folder <- drive_get(as_id(folder_url))
  if (nrow(folder) == 0) stop(glue("No se encontró la carpeta en Drive."))
  
  ss <- drive_ls(path = as_id(folder_url)) |> 
    filter(name == spreadsheet)
  
  if (nrow(ss) > 1) {
    stop(glue("Se encontraron varios archivos llamados '{spreadsheet}'."))
  }
  
  if (nrow(ss) == 1) {
    ss_id <- ss$id[[1]]
    message(glue("Spreadsheet encontrado: '{spreadsheet}'"))
    
    existing <- sheet_names(ss_id)
    
    if (sheetname %in% existing) {
      sheet_delete(ss_id, sheet = sheetname)
      warning(glue("Sheet '{sheetname}' ya existía, se sobreescribió."))
    }
    
    sheet_add(ss_id, sheet = sheetname)
    
  } else {
    ss_new <- gs4_create(spreadsheet)
    drive_mv(ss_new, path = as_id(folder_url))
    
    ss_id <- googledrive::as_id(ss_new)
    
    sheet_rename(ss_id, sheet = 1, new_name = sheetname)
    message(glue("Spreadsheet creado: '{spreadsheet}'"))
  }
  
  write_sheet(data = out, ss = ss_id, sheet = sheetname)
  
  message(glue("Sheet '{sheetname}' guardado en '{spreadsheet}'."))
  invisible(out)
}

# funcion para hacer una lista de diccionario por pregunta
make_codebook <- function(codebook, columna = pregunta) {
  codebook |>
    dplyr::mutate(
      etiqueta = str_replace_all(etiqueta, '"', ""),
      etiqueta = str_replace_all(etiqueta, "[\\n\\r]", " "),
      etiqueta = str_squish(etiqueta)
    ) |>
    group_by({{ columna }}) |>
    nest() |>
    deframe()
}

# funcion para detectar preguntas abiertas de questions
detect_open <- function(raw, 
                        questions, 
                        codebook,
                        col_id = Pregunta_id,
                        col_tipo = Tipo,
                        col_codebook = pregunta,
                        value = "Abierta") {
  
  preguntas_codebook <- codebook |>
    pull({{col_codebook}}) |>
    as.character() |>
    unique()
  
  questions |>
    filter({{col_tipo}} == value) |>
    pull({{col_id}}) |>
    as.character() |>
    intersect(names(raw)) |>
    intersect(preguntas_codebook)
}

# funcion para parsear categorias de quesiotns
parse_categorias <- function(x) { 

  if(is.na(x) || str_squish(x) == "") {
    return(character(0))
  }

  str_split(x, "\n")[[1]] |>
    str_squish() |>
    str_replace("^[A-Za-z0-9]+[:.=]\\s*(.*)$", "\\1")
}


# Funcion para hacer configuracion por pregunta

make_cfg <- function(raw, questions, codebook) {

  dic <- make_codebook(codebook = codebook)

  open_q <- detect_open(raw = raw, questions = questions, codebook = codebook)

  config_q <- function(q) {

    meta <- questions |> 
      filter(Pregunta_id == q) 


    if (nrow(meta) != 1) {
      stop(glue::glue("Pregunta {q} tiene {nrow(meta)} filas en questions"))
    }
    dict <- dic[[q]]

    if (is.null(dict)) {
      stop(glue::glue("Pregunta {q} no tiene diccionario en codebook"))
    }

    dep <- meta$Dependencia[[1]]

    if (!is.na(dep)) {
      
      meta_dep <- questions |> 
        filter(Pregunta_id == dep)

      if (nrow(meta_dep) != 1) {
        stop(glue::glue("La dependencia {dep} de {q} tiene {nrow(meta_dep)} filas"))
      }

      texto_dependencia <- meta_dep$Pregunta[[1]]
      categorias_dependencia <- parse_categorias(meta_dep$Categorias[[1]])
      
    } else {

      texto_dependencia <- NA_character_
      categorias_dependencia <- character(0)
    }

    cfg <- list(
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


# Funcion para hacer prompts
make_prompt <- function(cfg, categoria_cerrada = NULL) {

  definiciones <- cfg$diccionario |>
    mutate(linea = glue::glue("- {etiqueta}: {descripcion}")) |>
    pull(linea) |>
    paste(collapse = "\n")

  tiene_dependencia <- !is.null(cfg$dependencia) &&
    !is.na(cfg$dependencia) &&
    stringr::str_squish(cfg$dependencia) != ""

  categorias_txt <- if (
    !is.null(cfg$categorias_cerradas) &&
    length(cfg$categorias_cerradas) > 0
  ) {
    paste(cfg$categorias_cerradas, collapse = "; ")
  } else {
    "No especificadas"
  }

  instrucciones <- c(
    "Lee la respuesta completa antes de clasificar.",
    "Puedes asignar uno o más códigos si la respuesta contiene más de una idea sustantiva del codebook.",
    "No inventes códigos: usa exclusivamente valores listados en el codebook.",
    "Si una respuesta encaja en varias categorías, devuelve todas las categorías pertinentes.",
    "Asigna como máximo tres códigos.",
    "Ordena los códigos de mayor a menor relevancia.",
    "No repitas códigos.",
    "Si la respuesta es evasiva, tautológica, no responde la pregunta o no aporta contenido clasificable, usa el código de no respuesta/no aplica/desconocimiento disponible en el codebook."
  )

  if (tiene_dependencia) {
    instrucciones <- c(
      instrucciones,
      "La respuesta cerrada previa se entrega como contexto. Úsala para interpretar respuestas abiertas breves o elípticas, pero clasifica la razón abierta."
    )
  }

  bloque_instrucciones <- paste0("- ", instrucciones, collapse = "\n")

  contexto_cerrada <- if (tiene_dependencia && !is.null(categoria_cerrada)) {
    glue::glue(
"Pregunta cerrada previa:
{cfg$dependencia} - {cfg$pregunta_cerrada}

Categorías posibles de la pregunta cerrada:
{categorias_txt}

La persona encuestada seleccionó en la pregunta cerrada previa:
{categoria_cerrada}
"
    )
  } else if (tiene_dependencia) {
    glue::glue(
"Pregunta cerrada previa:
{cfg$dependencia} - {cfg$pregunta_cerrada}

Categorías posibles de la pregunta cerrada:
{categorias_txt}
"
    )
  } else {
    ""
  }

  glue::glue(
"Developer: Eres un clasificador de respuestas abiertas de encuesta sobre {cfg$tema}.

Pregunta abierta:
{cfg$pregunta} - {cfg$texto}

{contexto_cerrada}

Analiza cada comentario recibido y asígnale todos los códigos pertinentes, utilizando solo los siguientes códigos:

{definiciones}

Instrucciones:
{bloque_instrucciones}

Responde únicamente con un objeto JSON siguiendo este formato:
{{\"codigos\": [\"codigo_1\", \"codigo_2\"]}}
"
  )
}

# Genera sub prompt por cada categoria de respuesta cerrada para las preguntas con dependencia.
get_prompt <- function(cfg, respuesta_cerrada = NA_character_){

  dep <- !is.null(cfg$dependencia) &&
    !is.na(cfg$dependencia) &&
    stringr::str_squish(cfg$dependencia) != ""

  if(dep){
    make_prompt(
      cfg,
      categoria_cerrada = respuesta_cerrada
    )
  } else {
    make_prompt(cfg = cfg)
  }
}

nomrstr <- function(x) {
  x |> 
    as.character() |> 
    str_to_lower() |> 
    str_squish()
}

build_chat <- function(system_prompt,
                       model,
                       provider = CONFIG$provider %||% "openai",
                       api_key_env = CONFIG$api_key_env %||% "OPENAI_API_KEY",
                       ollama_base_url = CONFIG$ollama_base_url %||% Sys.getenv("OLLAMA_BASE_URL", "http://localhost:11434")) {

  switch(
    provider,

    "openai" = chat_openai(
      model = model,
      base_url = "https://api.openai.com/v1",
      api_key = Sys.getenv(api_key_env),
      system_prompt = system_prompt
    ),

    "ollama" = chat_ollama(
      model = model,
      base_url = ollama_base_url,
      system_prompt = system_prompt
    ),

    stop(glue::glue("provider '{provider}' no soportado. Usa 'openai' u 'ollama'."))
  )
}

clasify <- function(cfg = cfg, texto, respuesta_cerrada, model = CONFIG$model, provider = CONFIG$provider %||% "openai") {

  if (is.na(texto) || stringr::str_squish(texto) == "") {
    return(NA_character_)
  }

  prompt <- get_prompt(
    cfg = cfg,
    respuesta_cerrada = respuesta_cerrada
  )

  chat <- build_chat(
    system_prompt = prompt,
    model = model,
    provider = provider
  )

  tryCatch({

    out <- chat$chat_structured(
      texto,
      type = type_object(
        codigos = type_array(
          type_enum(values = cfg$etiquetas)
        )
      )
    )
    
    out$codigos |>
      paste(collapse = "; ")

  }, error = function(e) {
    return(NA_character_)
  })
}

classify_question <- function(raw, cfg, chunk_size = 20L) {

  pregunta <- cfg$pregunta
  dependencia <- cfg$dependencia

  bloques <- split(
    raw,
    (seq_len(nrow(raw)) - 1L) %/% chunk_size
  )

  map_dfr(bloques, function(bloque) {

    bloque |>
      mutate(
        respuesta_abierta = .data[[pregunta]],
        respuesta_cerrada = if (
          !is.na(dependencia) && dependencia %in% names(bloque)
        ) {
          .data[[dependencia]]
        } else {
          NA_character_
        },
        codigo_temp = map2_chr(
          respuesta_abierta,
          respuesta_cerrada,
          ~ clasify(
            texto = .x,
            respuesta_cerrada = .y,
            cfg = cfg
          ),
          .progress = TRUE
        )
      ) |>
      select(codigo_temp)
  }) |>
    pull(codigo_temp)
}



