# =============================================================================
# DriveFlow/qualcode.R
# Codificacion de preguntas abiertas contra un codebook, con LLM local
# (Ollama + Qwen3) o con un proveedor por API.
#
# Adaptado del qualcode.R del panel de FOCUS. Cambios principales:
#   1. provider "ollama" con temperatura y seed fijas y "thinking" apagado
#   2. clasificacion en paralelo (parallel_chat_structured) con fallback
#      secuencial, en vez de una llamada por fila reconstruyendo el prompt
#   3. los insumos se pueden leer de archivos locales o de Drive
#   4. los errores del modelo se marcan "ERROR" y no se confunden con NA
#      (que significa "la persona no respondio")
#   5. la muestra de revision viene con columna vacia para codificar a mano,
#      y medir_acuerdo() compara esa columna contra el modelo
#
# No se ejecuta solo: se usa desde DriveFlow/run.R
# =============================================================================

library(readr)
library(readxl)
library(dplyr)
library(purrr)
library(stringr)
library(tibble)
library(tidyr)
library(ellmer)
library(glue)
library(yaml)

`%||%` <- function(x, y) if (is.null(x)) y else x

# ---- 1. Configuracion -------------------------------------------------------

#' Combina project.yml (comun al proyecto) con DriveFlow/<ronda>.yml
#' Lo de la ronda tiene prioridad.
load_config <- function(round_file, project_file = "project.yml") {
  project_cfg <- if (file.exists(project_file)) yaml::read_yaml(project_file) else list()
  round_cfg   <- yaml::read_yaml(round_file)

  base_cfg <- c(
    project_cfg$codificacion %||% list(),
    list(questions_url = project_cfg$drive$questions_url %||% "")
  )
  modifyList(base_cfg, round_cfg)
}

# ---- 2. Lectura de insumos (local o Drive) ----------------------------------

local_disponible <- function(x) !is.null(x) && nzchar(x) && file.exists(x)

#' Lee un spreadsheet de una carpeta de Drive (camino original del flujo)
read_spreadsheet <- function(folder_url, file_name, sheet = NULL) {
  stopifnot(requireNamespace("googledrive", quietly = TRUE),
            requireNamespace("googlesheets4", quietly = TRUE))

  ss <- googledrive::drive_ls(path = googledrive::as_id(folder_url)) |>
    dplyr::filter(name == file_name)

  if (nrow(ss) == 0) stop(glue("No se encontro '{file_name}' en Drive."))
  if (nrow(ss) > 1)  stop(glue("Hay varios archivos llamados '{file_name}'."))

  id <- googledrive::as_id(ss$id[[1]])
  if (is.null(sheet)) googlesheets4::read_sheet(id) else googlesheets4::read_sheet(id, sheet = sheet)
}

#' Guarda una tabla en un spreadsheet de Drive, sobreescribiendo la hoja
save_drive <- function(out, folder_url, spreadsheet, sheetname) {
  stopifnot(requireNamespace("googledrive", quietly = TRUE),
            requireNamespace("googlesheets4", quietly = TRUE))

  ss <- googledrive::drive_ls(path = googledrive::as_id(folder_url)) |>
    dplyr::filter(name == spreadsheet)

  if (nrow(ss) > 1) stop(glue("Hay varios archivos llamados '{spreadsheet}'."))

  if (nrow(ss) == 1) {
    id <- googledrive::as_id(ss$id[[1]])
    if (sheetname %in% googlesheets4::sheet_names(id)) {
      googlesheets4::sheet_delete(id, sheet = sheetname)
      warning(glue("La hoja '{sheetname}' ya existia, se sobreescribio."))
    }
    googlesheets4::sheet_add(id, sheet = sheetname)
  } else {
    nuevo <- googlesheets4::gs4_create(spreadsheet)
    googledrive::drive_mv(nuevo, path = googledrive::as_id(folder_url))
    id <- googledrive::as_id(nuevo)
    googlesheets4::sheet_rename(id, sheet = 1, new_name = sheetname)
  }

  googlesheets4::write_sheet(data = out, ss = id, sheet = sheetname)
  message(glue("Guardado '{sheetname}' en '{spreadsheet}'."))
  invisible(out)
}

#' Devuelve raw, questions y codebook, priorizando los archivos locales
leer_insumos <- function(CONFIG) {

  raw <- if (local_disponible(CONFIG$raw_file)) {
    message("raw: ", CONFIG$raw_file)
    readr::read_csv(CONFIG$raw_file, col_types = readr::cols(.default = "c"))
  } else {
    read_spreadsheet(CONFIG$folder_url, paste0("transcripcion_", CONFIG$round))
  }

  questions <- if (local_disponible(CONFIG$questions_file)) {
    message("questions: ", CONFIG$questions_file)
    readxl::read_excel(CONFIG$questions_file, col_types = "text")
  } else {
    read_spreadsheet(CONFIG$questions_url, "questions")
  }

  codebook <- if (local_disponible(CONFIG$codebook_file)) {
    message("codebook: ", CONFIG$codebook_file, " (hoja ", CONFIG$round, ")")
    readxl::read_excel(CONFIG$codebook_file, sheet = CONFIG$round, col_types = "text")
  } else {
    read_spreadsheet(CONFIG$folder_url, paste0("Book", CONFIG$round), sheet = CONFIG$round)
  }

  questions <- questions |> dplyr::filter(as.character(Ronda_id) == CONFIG$round)
  if (nrow(questions) == 0) stop("No hay preguntas para la ronda ", CONFIG$round, " en questions")

  list(raw = raw, questions = questions, codebook = codebook)
}

# ---- 3. Armado de la configuracion por pregunta -----------------------------

#' Codebook a lista nombrada por pregunta
make_codebook <- function(codebook, columna = pregunta) {
  codebook |>
    dplyr::mutate(
      etiqueta = str_replace_all(etiqueta, '"', ""),
      etiqueta = str_replace_all(etiqueta, "[\\n\\r]", " "),
      etiqueta = str_squish(etiqueta)
    ) |>
    dplyr::group_by({{ columna }}) |>
    tidyr::nest() |>
    tibble::deframe()
}

#' Preguntas abiertas que estan a la vez en la base y en el codebook
detect_open <- function(raw, questions, codebook,
                        col_id = Pregunta_id, col_tipo = Tipo,
                        col_codebook = pregunta, value = "Abierta") {

  preguntas_codebook <- codebook |> dplyr::pull({{ col_codebook }}) |> as.character() |> unique()

  declaradas <- questions |>
    dplyr::filter({{ col_tipo }} == value) |>
    dplyr::pull({{ col_id }}) |> as.character()

  sin_datos   <- setdiff(declaradas, names(raw))
  sin_codigos <- setdiff(intersect(declaradas, names(raw)), preguntas_codebook)

  if (length(sin_datos))   warning("abiertas declaradas que no estan en la base: ", paste(sin_datos, collapse = ", "))
  if (length(sin_codigos)) warning("abiertas sin codebook (se saltean): ", paste(sin_codigos, collapse = ", "))

  declaradas |> intersect(names(raw)) |> intersect(preguntas_codebook)
}

#' Parsea las opciones de una pregunta cerrada ("1: texto" -> "texto")
parse_categorias <- function(x) {
  if (is.null(x) || is.na(x) || str_squish(x) == "") return(character(0))
  str_split(x, "\n")[[1]] |>
    str_squish() |>
    str_replace("^[A-Za-z0-9]+[:.=]\\s*(.*)$", "\\1") |>
    (\(v) v[nzchar(v)])()
}

#' Una config por pregunta abierta: texto, dependencia, diccionario, etiquetas
make_cfg <- function(raw, questions, codebook) {

  dic    <- make_codebook(codebook = codebook)
  open_q <- detect_open(raw = raw, questions = questions, codebook = codebook)

  if (!length(open_q)) stop("No quedo ninguna pregunta abierta para codificar")

  config_q <- function(q) {
    meta <- questions |> dplyr::filter(Pregunta_id == q)
    if (nrow(meta) != 1) stop(glue("La pregunta {q} tiene {nrow(meta)} filas en questions"))

    dict <- dic[[q]]
    if (is.null(dict)) stop(glue("La pregunta {q} no tiene diccionario en el codebook"))

    dep <- meta$Dependencia[[1]]

    if (!is.na(dep) && nzchar(str_squish(dep))) {
      meta_dep <- questions |> dplyr::filter(Pregunta_id == dep)
      if (nrow(meta_dep) != 1) stop(glue("La dependencia {dep} de {q} tiene {nrow(meta_dep)} filas"))
      texto_dep <- meta_dep$Pregunta[[1]]
      cats_dep  <- parse_categorias(meta_dep$Categorias[[1]])
    } else {
      dep       <- NA_character_
      texto_dep <- NA_character_
      cats_dep  <- character(0)
    }

    list(
      pregunta            = q,
      tema                = meta$Subtema[[1]],
      texto               = meta$Pregunta[[1]],
      dependencia         = dep,
      pregunta_cerrada    = texto_dep,
      categorias_cerradas = cats_dep,
      diccionario         = dict,
      etiquetas           = dict$etiqueta |> unique() |> purrr::discard(is.na)
    )
  }

  open_q |> purrr::set_names() |> purrr::map(config_q)
}

# ---- 4. Prompt --------------------------------------------------------------

#' Texto legible de la opcion cerrada elegida ("2" -> "2 - El desarrollo...")
etiqueta_cerrada <- function(cfg, valor) {
  cats <- cfg$categorias_cerradas
  if (!length(cats) || is.na(valor)) return(valor)
  i <- suppressWarnings(as.integer(valor))
  if (!is.na(i) && i >= 1 && i <= length(cats)) paste0(valor, " - ", cats[[i]]) else valor
}

make_prompt <- function(cfg, categoria_cerrada = NULL) {

  definiciones <- cfg$diccionario |>
    dplyr::mutate(linea = glue("- {etiqueta}: {descripcion}")) |>
    dplyr::pull(linea) |>
    paste(collapse = "\n")

  tiene_dep <- !is.null(cfg$dependencia) && !is.na(cfg$dependencia) &&
    str_squish(cfg$dependencia) != ""

  categorias_txt <- if (length(cfg$categorias_cerradas)) {
    paste(cfg$categorias_cerradas, collapse = "; ")
  } else {
    "No especificadas"
  }

  instrucciones <- c(
    "Lee la respuesta completa antes de clasificar.",
    "Puedes asignar uno o mas codigos si la respuesta contiene mas de una idea sustantiva del codebook.",
    "No inventes codigos: usa exclusivamente valores listados en el codebook.",
    "Si una respuesta encaja en varias categorias, devuelve todas las pertinentes.",
    "Asigna como maximo tres codigos.",
    "Ordena los codigos de mayor a menor relevancia.",
    "No repitas codigos.",
    "Si la respuesta es evasiva, tautologica, no responde la pregunta o no aporta contenido clasificable, usa el codigo de no clasificable del codebook."
  )
  if (tiene_dep) {
    instrucciones <- c(instrucciones,
      "La respuesta cerrada previa se entrega solo como contexto para interpretar respuestas breves o elipticas: lo que clasificas es la respuesta abierta.")
  }
  bloque_instrucciones <- paste0("- ", instrucciones, collapse = "\n")

  contexto_cerrada <- if (tiene_dep) {
    seleccion <- if (!is.null(categoria_cerrada)) {
      glue("\nLa persona encuestada selecciono: {categoria_cerrada}\n")
    } else ""
    glue("
Pregunta cerrada previa:
{cfg$dependencia} - {cfg$pregunta_cerrada}

Opciones de la pregunta cerrada:
{categorias_txt}
{seleccion}")
  } else ""

  glue("
Eres un clasificador de respuestas abiertas de una encuesta sobre {cfg$tema}.

Pregunta abierta:
{cfg$pregunta} - {cfg$texto}
{contexto_cerrada}
Analiza la respuesta y asignale todos los codigos pertinentes, usando solo estos codigos:

{definiciones}

Instrucciones:
{bloque_instrucciones}

Responde unicamente con un objeto JSON con este formato:
{{\"codigos\": [\"codigo_1\", \"codigo_2\"]}}
")
}

# ---- 5. Conexion al modelo --------------------------------------------------

build_chat <- function(system_prompt, CONFIG) {

  provider <- CONFIG$provider %||% "ollama"

  parametros <- ellmer::params(
    temperature = CONFIG$temperature %||% 0,
    seed        = CONFIG$seed %||% NULL
  )

  if (identical(provider, "ollama")) {
    args <- list(
      model         = CONFIG$model %||% "qwen3:8b",
      base_url      = CONFIG$ollama_base_url %||% Sys.getenv("OLLAMA_BASE_URL", "http://localhost:11434"),
      system_prompt = system_prompt,
      params        = parametros
    )
    # Qwen3 razona por defecto: para clasificar contra una lista cerrada
    # es tiempo perdido y ademas ensucia la salida estructurada.
    if (isFALSE(CONFIG$think %||% FALSE)) args$api_args <- list(think = FALSE)
    return(do.call(ellmer::chat_ollama, args))
  }

  if (identical(provider, "openai")) {
    return(ellmer::chat_openai(
      model         = CONFIG$model,
      api_key       = Sys.getenv(CONFIG$api_key_env %||% "OPENAI_API_KEY"),
      system_prompt = system_prompt,
      params        = parametros
    ))
  }

  stop(glue("provider '{provider}' no soportado. Usa 'ollama' u 'openai'."))
}

#' Esquema de salida: un array de etiquetas, restringido al codebook
tipo_salida <- function(etiquetas) {
  ellmer::type_object(
    codigos = ellmer::type_array(ellmer::type_enum(values = etiquetas))
  )
}

# ---- 6. Clasificacion -------------------------------------------------------

#' Pasa la salida estructurada a un vector "codigo_1; codigo_2"
#' Devuelve "ERROR" en las posiciones donde el modelo fallo.
normalizar_codigos <- function(res, n) {

  extraer <- function(x) {
    if (is.null(x)) return("ERROR")
    v <- unlist(x, use.names = FALSE)
    v <- v[!is.na(v) & nzchar(v)]
    if (!length(v)) return("ERROR")
    paste(unique(v), collapse = "; ")
  }

  vals <- if (is.data.frame(res)) {
    if (!"codigos" %in% names(res)) stop("la salida del modelo no trae columna 'codigos'")
    res$codigos
  } else {
    lapply(res, function(x) if (is.list(x) && !is.null(x$codigos)) x$codigos else x)
  }

  if (length(vals) != n) stop("el modelo devolvio ", length(vals), " filas y esperaba ", n)
  vapply(vals, extraer, character(1), USE.NAMES = FALSE)
}

#' Clasifica un lote de textos que comparten el mismo prompt de sistema.
#' Intenta en paralelo; si el proveedor no lo soporta, va secuencial.
codificar_lote <- function(prompt_sistema, textos, etiquetas, CONFIG) {

  tipo <- tipo_salida(etiquetas)
  n    <- length(textos)
  if (n == 0) return(character(0))

  paralelo <- tryCatch({
    chat <- build_chat(prompt_sistema, CONFIG)
    res <- ellmer::parallel_chat_structured(
      chat,
      as.list(textos),
      tipo,
      max_active = CONFIG$max_active %||% 4L,
      on_error   = "continue"
    )
    normalizar_codigos(res, n)
  }, error = function(e) {
    message("    sin paralelo (", conditionMessage(e), "), voy secuencial")
    NULL
  })

  if (!is.null(paralelo)) return(paralelo)

  vapply(textos, function(txt) {
    tryCatch({
      chat <- build_chat(prompt_sistema, CONFIG)
      r <- chat$chat_structured(txt, type = tipo)
      v <- unlist(r$codigos, use.names = FALSE)
      v <- v[!is.na(v) & nzchar(v)]
      if (!length(v)) "ERROR" else paste(unique(v), collapse = "; ")
    }, error = function(e) "ERROR")
  }, character(1), USE.NAMES = FALSE)
}

#' Codifica una pregunta abierta completa.
#' Si tiene dependencia, agrupa por la respuesta cerrada y arma un prompt por
#' grupo (asi el contexto entra una vez por grupo y no una vez por fila).
codificar_pregunta <- function(raw, cfg, CONFIG) {

  n      <- nrow(raw)
  salida <- rep(NA_character_, n)
  texto  <- as.character(raw[[cfg$pregunta]])
  vacio  <- is.na(texto) | str_squish(texto) == ""

  con_dep <- !is.na(cfg$dependencia) && cfg$dependencia %in% names(raw)

  grupo <- if (con_dep) as.character(raw[[cfg$dependencia]]) else rep("__todos__", n)
  grupo[is.na(grupo)] <- "__sin_dato__"

  for (g in unique(grupo[!vacio])) {

    idx <- which(!vacio & grupo == g)

    categoria <- if (con_dep && g != "__sin_dato__") etiqueta_cerrada(cfg, g) else NULL
    prompt    <- make_prompt(cfg, categoria_cerrada = categoria)

    if (con_dep) message("    grupo ", g, ": ", length(idx), " respuestas")

    salida[idx] <- codificar_lote(prompt, texto[idx], cfg$etiquetas, CONFIG)
  }

  salida
}

# ---- 7. Control de calidad --------------------------------------------------

#' Muestra aleatoria para revisar a mano.
#' Agrega una columna vacia codigo_<q>_rev para escribir la codificacion humana.
muestra_revision <- function(out, cfg, prop = 0.20, seed = 20260810) {

  set.seed(seed)
  qs   <- names(cfg)
  cods <- paste0("codigo_", qs)

  m <- out |>
    dplyr::select(dplyr::any_of(c("numero", "caso_id", qs, cods))) |>
    dplyr::slice_sample(prop = prop)

  for (q in qs) {
    cc <- paste0("codigo_", q)
    if (all(c(q, cc) %in% names(m))) {
      m[[paste0(cc, "_rev")]] <- NA_character_
      m <- m |> dplyr::relocate(dplyr::all_of(c(cc, paste0(cc, "_rev"))), .after = dplyr::all_of(q))
    }
  }
  m
}

#' Acuerdo entre el modelo y la revision humana, por pregunta.
#' Se corre despues de completar a mano las columnas codigo_<q>_rev.
medir_acuerdo <- function(revisado) {

  cols <- grep("^codigo_.*_rev$", names(revisado), value = TRUE)
  if (!length(cols)) stop("no hay columnas codigo_<q>_rev en la tabla revisada")

  limpiar <- function(x) {
    v <- trimws(strsplit(ifelse(is.na(x), "", x), "\\s*;\\s*")[[1]])
    unique(v[nzchar(v)])
  }

  purrr::map_dfr(cols, function(cr) {

    cm <- sub("_rev$", "", cr)
    a  <- revisado[[cm]]
    b  <- revisado[[cr]]
    ok <- !is.na(b) & nzchar(trimws(b))

    if (!any(ok)) {
      return(tibble(pregunta = sub("^codigo_", "", cm), n = 0L,
                    exacto = NA_real_, algun_codigo = NA_real_, jaccard = NA_real_))
    }

    sa <- lapply(a[ok], limpiar)
    sb <- lapply(b[ok], limpiar)

    exacto  <- mean(mapply(setequal, sa, sb))
    alguno  <- mean(mapply(function(x, y) length(intersect(x, y)) > 0, sa, sb))
    jaccard <- mean(mapply(function(x, y) {
      u <- length(union(x, y))
      if (u == 0) 1 else length(intersect(x, y)) / u
    }, sa, sb))

    tibble(
      pregunta     = sub("^codigo_", "", cm),
      n            = sum(ok),
      exacto       = round(100 * exacto, 1),
      algun_codigo = round(100 * alguno, 1),
      jaccard      = round(jaccard, 2)
    )
  })
}

#' Distribucion de codigos de una pregunta (para mirar si el codebook sirve)
frecuencia_codigos <- function(out, q) {
  cc <- paste0("codigo_", q)
  out[[cc]] |>
    strsplit("\\s*;\\s*") |>
    unlist() |>
    trimws() |>
    (\(v) v[nzchar(v)])() |>
    table() |>
    sort(decreasing = TRUE) |>
    as.data.frame() |>
    setNames(c("codigo", "n"))
}
