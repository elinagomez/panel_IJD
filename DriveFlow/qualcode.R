# =============================================================================
# DriveFlow/qualcode.R
# Codificacion de preguntas abiertas contra un codebook, con LLM local
# (Ollama + Qwen3) o con un proveedor por API.
#
# Los insumos editables por el equipo (la hoja de preguntas y el codebook de la
# ronda) viven en Drive. En cada corrida se guarda una copia congelada junto a
# los resultados, para poder reconstruir con que version se codifico.
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
    list(drive = project_cfg$drive %||% list())
  )
  modifyList(base_cfg, round_cfg)
}

# ---- 2. Normalizacion de nombres -------------------------------------------

#' Compara nombres de columna ignorando mayusculas, espacios, guiones y tildes.
#' Sirve para que el equipo pueda escribir "RondaID", "Ronda_id" o "ronda id".
clave_columna <- function(x) {
  x |>
    as.character() |>
    iconv(to = "ASCII//TRANSLIT") |>
    tolower() |>
    str_replace_all("[^a-z0-9]", "")
}

#' Renombra las columnas de df a los nombres canonicos que usa el flujo.
#' @param esperados lista canonico = vector de alias aceptados
renombrar_columnas <- function(df, esperados) {
  claves <- clave_columna(names(df))
  for (canonico in names(esperados)) {
    alias <- clave_columna(esperados[[canonico]])
    i <- which(claves %in% alias)
    if (length(i) >= 1) names(df)[i[1]] <- canonico
  }
  df
}

COLUMNAS_QUESTIONS <- list(
  Ronda_id    = c("Ronda_id", "RondaID", "Ronda"),
  Pregunta_id = c("Pregunta_id", "PreguntaId", "Pregunta_ID"),
  Pregunta    = c("Pregunta", "Texto"),
  Tipo        = c("Tipo"),
  Tema        = c("Tema", "TEMA"),
  Subtema     = c("Subtema", "SubTema"),
  Dependencia = c("Dependencia", "Depende_de"),
  Categorias  = c("Categorias", "CategoriasCerrada", "Categorias_cerrada")
)

COLUMNAS_CODEBOOK <- list(
  pregunta    = c("pregunta", "Pregunta_id", "PreguntaId"),
  etiqueta    = c("etiqueta", "codigo", "Etiqueta"),
  descripcion = c("descripcion", "definicion", "Descripcion")
)

#' Id de pregunta normalizado, para que "Q1" y "q1" sean lo mismo
normalizar_id <- function(x) tolower(str_squish(as.character(x)))

# ---- 3. Lectura de insumos (Drive o local) ----------------------------------

#' Lee un spreadsheet identificado por url o id.
#' Si se pide una hoja que no existe: usa la unica que haya (caso tipico de un
#' archivo recien importado), o corta listando las hojas disponibles. No cae en
#' silencio a la primera hoja, porque en un libro con una hoja por ronda eso
#' significaria leer la ronda equivocada.
leer_sheet <- function(x, sheet = NULL) {
  stopifnot(requireNamespace("googlesheets4", quietly = TRUE))
  id    <- googlesheets4::as_sheets_id(x)
  hojas <- googlesheets4::sheet_names(id)

  hoja <- if (is.null(sheet) || !nzchar(sheet)) {
    hojas[1]
  } else if (sheet %in% hojas) {
    sheet
  } else if (length(hojas) == 1) {
    warning(glue("No hay una hoja '{sheet}'; uso la unica que existe ('{hojas[1]}'). ",
                 "Conviene renombrarla a '{sheet}'."), call. = FALSE)
    hojas[1]
  } else {
    stop(glue("No existe la hoja '{sheet}'. Hojas disponibles: {paste(hojas, collapse = ', ')}"),
         call. = FALSE)
  }

  googlesheets4::read_sheet(id, sheet = hoja, col_types = "c")
}

#' Busca un spreadsheet por nombre dentro de una carpeta de Drive
buscar_en_carpeta <- function(folder_id, nombre) {
  stopifnot(requireNamespace("googledrive", quietly = TRUE))
  ss <- googledrive::drive_ls(googledrive::as_id(folder_id)) |>
    dplyr::filter(name == nombre)

  if (nrow(ss) == 0) stop(glue("No encontre '{nombre}' en la carpeta de Drive."))
  if (nrow(ss) > 1)  stop(glue("Hay {nrow(ss)} archivos llamados '{nombre}' en la carpeta."))
  ss$id[[1]]
}

#' Encuentra o crea una subcarpeta dentro de una carpeta de Drive
carpeta_ronda <- function(folder_id, nombre, crear = FALSE) {
  stopifnot(requireNamespace("googledrive", quietly = TRUE))
  hijos <- googledrive::drive_ls(googledrive::as_id(folder_id), type = "folder")
  hit <- hijos |> dplyr::filter(name == nombre)

  if (nrow(hit) == 1) return(hit$id[[1]])
  if (nrow(hit) > 1)  stop(glue("Hay varias carpetas '{nombre}' dentro de la carpeta indicada."))
  if (!crear) stop(glue("No existe la carpeta '{nombre}'. Crearla en Drive o usar crear = TRUE."))

  nueva <- googledrive::drive_mkdir(nombre, path = googledrive::as_id(folder_id))
  nueva$id[[1]]
}

#' Guarda una tabla en un spreadsheet de Drive, sobreescribiendo la hoja
save_drive <- function(out, folder_id, spreadsheet, sheetname) {
  stopifnot(requireNamespace("googledrive", quietly = TRUE),
            requireNamespace("googlesheets4", quietly = TRUE))

  ss <- googledrive::drive_ls(googledrive::as_id(folder_id)) |>
    dplyr::filter(name == spreadsheet)

  if (nrow(ss) > 1) stop(glue("Hay varios archivos llamados '{spreadsheet}'."))

  if (nrow(ss) == 1) {
    id <- googlesheets4::as_sheets_id(ss$id[[1]])
    if (sheetname %in% googlesheets4::sheet_names(id)) {
      googlesheets4::sheet_delete(id, sheet = sheetname)
    }
    googlesheets4::sheet_add(id, sheet = sheetname)
  } else {
    nuevo <- googlesheets4::gs4_create(spreadsheet)
    googledrive::drive_mv(nuevo, path = googledrive::as_id(folder_id))
    id <- googlesheets4::as_sheets_id(nuevo)
    googlesheets4::sheet_rename(id, sheet = 1, new_name = sheetname)
  }

  googlesheets4::write_sheet(data = out, ss = id, sheet = sheetname)
  message(glue("Drive: guardado '{sheetname}' en '{spreadsheet}'."))
  invisible(out)
}

#' Autenticacion de Drive y Sheets con la misma cuenta.
#' googlesheets4 necesita su propio auth: con drive_auth() solo no alcanza.
autenticar <- function(CONFIG) {
  email <- CONFIG$drive$account_email %||% ""
  if (!nzchar(email)) stop("Falta drive$account_email en project.yml")
  googledrive::drive_auth(email = email)
  googlesheets4::gs4_auth(token = googledrive::drive_token())
  invisible(email)
}

#' Devuelve raw, questions y codebook, ya con columnas canonicas.
#' CONFIG$insumos decide de donde se leen: "drive" o "local".
leer_insumos <- function(CONFIG) {

  origen <- CONFIG$insumos %||% "drive"
  ronda  <- CONFIG$round

  # la base transcrita siempre es local: es el output del paso anterior
  if (!file.exists(CONFIG$raw_file)) stop("No existe la base transcrita: ", CONFIG$raw_file)
  raw <- readr::read_csv(CONFIG$raw_file, col_types = readr::cols(.default = "c"))
  message("raw:       ", CONFIG$raw_file, " (", nrow(raw), " filas)")

  # el libro de codigos es uno solo para todo el proyecto, con una hoja por ronda
  nombre_book <- CONFIG$codebook_name %||% "LibroCodigos"
  if (!nzchar(nombre_book)) nombre_book <- "LibroCodigos"
  hoja_book <- CONFIG$codebook_tab %||% ""
  if (!nzchar(hoja_book)) hoja_book <- ronda

  if (identical(origen, "drive")) {

    autenticar(CONFIG)

    questions <- leer_sheet(CONFIG$drive$questions_sheet, CONFIG$questions_sheet_tab %||% "")
    message("questions: Drive · hoja de preguntas del proyecto")

    id_book  <- buscar_en_carpeta(CONFIG$drive$folder_analisis_id, nombre_book)
    codebook <- leer_sheet(id_book, hoja_book)
    message("codebook:  Drive · ", nombre_book, " / hoja ", hoja_book)

  } else {

    questions <- readxl::read_excel(CONFIG$questions_file, col_types = "text")

    hojas <- readxl::excel_sheets(CONFIG$codebook_file)
    if (!hoja_book %in% hojas) {
      if (length(hojas) == 1) {
        warning(glue("El libro local no tiene hoja '{hoja_book}'; uso '{hojas[1]}'."), call. = FALSE)
        hoja_book <- hojas[1]
      } else {
        stop(glue("El libro local no tiene hoja '{hoja_book}'. Hojas: {paste(hojas, collapse = ', ')}"),
             call. = FALSE)
      }
    }
    codebook <- readxl::read_excel(CONFIG$codebook_file, sheet = hoja_book, col_types = "text")
    message("questions: ", CONFIG$questions_file, "\ncodebook:  ", CONFIG$codebook_file, " / hoja ", hoja_book)
  }

  questions <- renombrar_columnas(questions, COLUMNAS_QUESTIONS)
  codebook  <- renombrar_columnas(codebook,  COLUMNAS_CODEBOOK)

  faltan_q <- setdiff(c("Ronda_id", "Pregunta_id", "Pregunta", "Tipo"), names(questions))
  faltan_c <- setdiff(c("pregunta", "etiqueta", "descripcion"), names(codebook))
  if (length(faltan_q)) stop("A la hoja de preguntas le faltan columnas: ", paste(faltan_q, collapse = ", "))
  if (length(faltan_c)) stop("Al codebook le faltan columnas: ", paste(faltan_c, collapse = ", "))

  for (col in c("Tema", "Subtema", "Dependencia", "Categorias")) {
    if (!col %in% names(questions)) questions[[col]] <- NA_character_
  }

  # filtrar la ronda; si el codebook trae columna de ronda, tambien
  questions <- questions |> dplyr::filter(normalizar_id(Ronda_id) == normalizar_id(ronda))
  if (nrow(questions) == 0) stop("La hoja de preguntas no tiene filas para la ronda ", ronda)

  if ("ronda" %in% clave_columna(names(codebook))) {
    col_ronda <- names(codebook)[clave_columna(names(codebook)) == "ronda"][1]
    codebook <- codebook |> dplyr::filter(normalizar_id(.data[[col_ronda]]) == normalizar_id(ronda))
  }

  codebook <- codebook |>
    dplyr::filter(!is.na(pregunta), !is.na(etiqueta), str_squish(etiqueta) != "")

  list(raw = raw, questions = questions, codebook = codebook)
}

#' Copia congelada de los insumos, al lado de los resultados.
#' Sin esto no se puede reconstruir con que codebook se codifico una ronda.
guardar_snapshot <- function(ins, CONFIG) {
  dir_snap <- file.path(CONFIG$out_dir, "insumos")
  dir.create(dir_snap, recursive = TRUE, showWarnings = FALSE)
  sello <- format(Sys.time(), "%Y%m%d_%H%M")

  readr::write_csv(ins$questions, file.path(dir_snap, glue("questions_{CONFIG$round}_{sello}.csv")))
  readr::write_csv(ins$codebook,  file.path(dir_snap, glue("codebook_{CONFIG$round}_{sello}.csv")))
  message("snapshot de insumos en ", dir_snap)
  invisible(dir_snap)
}

# ---- 4. Validacion de los insumos ------------------------------------------

#' Revisa la hoja de preguntas y el codebook antes de gastar tiempo de modelo.
#' Devuelve invisible(TRUE) si esta todo bien; corta con error en lo grave y
#' avisa con warning en lo sospechoso.
validar_insumos <- function(ins, CONFIG) {

  questions <- ins$questions
  codebook  <- ins$codebook
  raw       <- ins$raw

  problemas <- character(0)
  avisos    <- character(0)

  ids_q <- normalizar_id(questions$Pregunta_id)
  if (any(duplicated(ids_q))) {
    problemas <- c(problemas, paste0("ids de pregunta duplicados en la hoja: ",
                                     paste(unique(ids_q[duplicated(ids_q)]), collapse = ", ")))
  }

  cols_raw  <- normalizar_id(names(raw))
  abiertas  <- ids_q[questions$Tipo == "Abierta"]
  cerradas  <- ids_q[questions$Tipo == "Cerrada"]
  ids_book  <- unique(normalizar_id(codebook$pregunta))

  sin_datos <- setdiff(abiertas, cols_raw)
  if (length(sin_datos)) {
    avisos <- c(avisos, paste0("abiertas declaradas que no estan en la base: ", paste(sin_datos, collapse = ", ")))
  }

  sin_codebook <- setdiff(intersect(abiertas, cols_raw), ids_book)
  if (length(sin_codebook)) {
    avisos <- c(avisos, paste0("abiertas sin codebook, no se van a codificar: ", paste(sin_codebook, collapse = ", ")))
  }

  huerfanas <- setdiff(ids_book, ids_q)
  if (length(huerfanas)) {
    avisos <- c(avisos, paste0("preguntas del codebook que no existen en la hoja: ", paste(huerfanas, collapse = ", ")))
  }

  # etiquetas duplicadas dentro de una misma pregunta
  dup <- codebook |>
    dplyr::mutate(.p = normalizar_id(pregunta), .e = clave_columna(etiqueta)) |>
    dplyr::count(.p, .e) |>
    dplyr::filter(n > 1)
  if (nrow(dup)) {
    problemas <- c(problemas, paste0("etiquetas duplicadas: ",
                                     paste(unique(dup$.p), collapse = ", ")))
  }

  # cada pregunta necesita una categoria de no clasificable
  patron_nc <- CONFIG$patron_no_clasificable %||% "no_clasific|no_aplica|no_respuesta|ns_nc"
  faltan_nc <- codebook |>
    dplyr::mutate(.p = normalizar_id(pregunta)) |>
    dplyr::group_by(.p) |>
    dplyr::summarise(tiene = any(str_detect(etiqueta, patron_nc)), .groups = "drop") |>
    dplyr::filter(!tiene) |>
    dplyr::pull(.p)
  if (length(faltan_nc)) {
    avisos <- c(avisos, paste0("sin categoria de no clasificable: ", paste(faltan_nc, collapse = ", "),
                               " (el modelo va a forzar una categoria sustantiva)"))
  }

  # dependencias
  dep_norm <- normalizar_id(questions$Dependencia)
  con_dep  <- !is.na(questions$Dependencia) & str_squish(questions$Dependencia) != ""

  rotas <- setdiff(dep_norm[con_dep], ids_q)
  if (length(rotas)) {
    problemas <- c(problemas, paste0("dependencias que apuntan a preguntas inexistentes: ",
                                     paste(rotas, collapse = ", ")))
  }

  # una cerrada con dependencia casi siempre es la fila equivocada
  cerrada_con_dep <- ids_q[con_dep & questions$Tipo == "Cerrada"]
  if (length(cerrada_con_dep)) {
    avisos <- c(avisos, paste0("preguntas CERRADAS con dependencia: ", paste(cerrada_con_dep, collapse = ", "),
                               ". La dependencia deberia estar en la abierta que se apoya en ellas."))
  }

  # abiertas que por su texto parecen depender de una cerrada y no la declaran
  pistas <- "esa opcion|esa opción|por que elegiste|por qué elegiste|la opcion elegida"
  sospechosas <- ids_q[!con_dep & questions$Tipo == "Abierta" &
                         str_detect(clave_columna(questions$Pregunta), clave_columna(pistas))]
  if (length(sospechosas)) {
    avisos <- c(avisos, paste0("abiertas que parecen depender de una cerrada y no lo declaran: ",
                               paste(sospechosas, collapse = ", ")))
  }

  # si una abierta depende de una cerrada, esa cerrada necesita categorias
  for (i in which(con_dep & questions$Tipo == "Abierta")) {
    d <- dep_norm[i]
    j <- which(ids_q == d)
    if (length(j) == 1) {
      cats <- parse_categorias(questions$Categorias[[j]])
      if (!length(cats)) {
        avisos <- c(avisos, paste0(ids_q[i], " depende de ", d,
                                   ", pero ", d, " no tiene categorias cargadas"))
      }
    }
  }

  # temas vacios: el prompt dice "encuesta sobre NA"
  if (all(is.na(questions$Tema)) && all(is.na(questions$Subtema))) {
    avisos <- c(avisos, "las columnas TEMA y SubTema estan vacias: se usa tema_default de project.yml")
  }

  for (a in avisos) warning(a, call. = FALSE)

  if (length(problemas)) {
    stop("Los insumos tienen problemas que hay que arreglar:\n- ",
         paste(problemas, collapse = "\n- "), call. = FALSE)
  }

  message("validacion de insumos: ", length(avisos), " avisos, 0 errores")
  invisible(TRUE)
}

# ---- 5. Armado de la configuracion por pregunta -----------------------------

make_codebook <- function(codebook) {
  codebook |>
    dplyr::mutate(
      .p       = normalizar_id(pregunta),
      etiqueta = str_replace_all(etiqueta, '"', ""),
      etiqueta = str_replace_all(etiqueta, "[\\n\\r]", " "),
      etiqueta = str_squish(etiqueta),
      descripcion = str_squish(ifelse(is.na(descripcion), "", descripcion))
    ) |>
    dplyr::select(.p, etiqueta, descripcion) |>
    dplyr::group_by(.p) |>
    tidyr::nest() |>
    tibble::deframe()
}

#' Parsea las opciones de una cerrada. Tolera saltos de linea o numeracion
#' corrida en una sola celda: "1. uno 2. dos 3. tres".
parse_categorias <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x) || str_squish(x) == "") return(character(0))

  txt   <- str_squish(as.character(x))
  partes <- str_split(txt, "\n")[[1]]

  if (length(partes) < 2) {
    partes <- str_split(txt, "(?=(?:^|\\s)[0-9]+\\s*[.):-])")[[1]]
  }

  partes |>
    str_squish() |>
    str_replace("^[A-Za-z0-9]+\\s*[.):=-]\\s*", "") |>
    (\(v) v[nzchar(v)])()
}

#' Una config por pregunta abierta. Las claves son los nombres reales de las
#' columnas de la base (q1, q2...), aunque la hoja los escriba en mayuscula.
make_cfg <- function(raw, questions, codebook, CONFIG = list()) {

  dic <- make_codebook(codebook)

  mapa_raw <- setNames(names(raw), normalizar_id(names(raw)))

  ids_q    <- normalizar_id(questions$Pregunta_id)
  abiertas <- ids_q[questions$Tipo == "Abierta"]

  usables <- abiertas |> intersect(names(mapa_raw)) |> intersect(names(dic))
  if (!length(usables)) stop("No quedo ninguna pregunta abierta con datos y codebook")

  config_q <- function(id_norm) {

    i    <- which(ids_q == id_norm)
    meta <- questions[i, ]
    dict <- dic[[id_norm]]

    dep_valor <- meta$Dependencia[[1]]
    tiene_dep <- !is.na(dep_valor) && str_squish(dep_valor) != ""

    if (tiene_dep) {
      dep_norm <- normalizar_id(dep_valor)
      j <- which(ids_q == dep_norm)
      dep_col   <- mapa_raw[[dep_norm]] %||% NA_character_
      texto_dep <- if (length(j) == 1) questions$Pregunta[[j]] else NA_character_
      cats_dep  <- if (length(j) == 1) parse_categorias(questions$Categorias[[j]]) else character(0)
    } else {
      dep_col   <- NA_character_
      texto_dep <- NA_character_
      cats_dep  <- character(0)
    }

    tema <- c(meta$Tema[[1]], meta$Subtema[[1]], CONFIG$tema_default %||% "la encuesta")
    tema <- tema[!is.na(tema) & str_squish(tema) != ""][1]

    list(
      pregunta            = mapa_raw[[id_norm]],
      id_hoja             = meta$Pregunta_id[[1]],
      tema                = tema,
      texto               = meta$Pregunta[[1]],
      dependencia         = dep_col,
      pregunta_cerrada    = texto_dep,
      categorias_cerradas = cats_dep,
      diccionario         = dict,
      etiquetas           = dict$etiqueta |> unique() |> purrr::discard(is.na)
    )
  }

  cfgs <- purrr::map(usables, config_q)
  purrr::set_names(cfgs, purrr::map_chr(cfgs, "pregunta"))
}

# ---- 6. Prompt --------------------------------------------------------------

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

  tiene_dep <- !is.null(cfg$dependencia) && !is.na(cfg$dependencia)

  categorias_txt <- if (length(cfg$categorias_cerradas)) {
    paste(cfg$categorias_cerradas, collapse = "; ")
  } else "No especificadas"

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
{cfg$pregunta_cerrada}

Opciones de la pregunta cerrada:
{categorias_txt}
{seleccion}")
  } else ""

  glue("
Eres un clasificador de respuestas abiertas de una encuesta sobre {cfg$tema}.

Pregunta abierta:
{cfg$texto}
{contexto_cerrada}
Analiza la respuesta y asignale todos los codigos pertinentes, usando solo estos codigos:

{definiciones}

Instrucciones:
{bloque_instrucciones}

Responde unicamente con un objeto JSON con este formato:
{{\"codigos\": [\"codigo_1\", \"codigo_2\"]}}
")
}

# ---- 7. Conexion al modelo --------------------------------------------------

build_chat <- function(system_prompt, CONFIG) {

  provider <- CONFIG$provider %||% "ollama"

  # max_tokens acotado: la respuesta esperada es un JSON de tres etiquetas, no
  # hace falta reservar espacio de generacion largo.
  parametros <- ellmer::params(
    temperature = CONFIG$temperature %||% 0,
    seed        = CONFIG$seed %||% NULL,
    max_tokens  = CONFIG$max_tokens %||% NULL
  )

  if (identical(provider, "ollama")) {
    args <- list(
      model         = CONFIG$model %||% "qwen3:8b",
      base_url      = CONFIG$ollama_base_url %||% Sys.getenv("OLLAMA_BASE_URL", "http://localhost:11434"),
      system_prompt = system_prompt,
      params        = parametros
    )
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

tipo_salida <- function(etiquetas) {
  ellmer::type_object(
    codigos = ellmer::type_array(ellmer::type_enum(values = etiquetas))
  )
}

#' Chequeo previo: versiones de paquetes y si el servidor del modelo responde.
#' Conviene correrlo antes de codificar: la mayoria de los fallos masivos no son
#' del modelo sino de la instalacion o de que el servidor no esta levantado.
diagnostico_modelo <- function(CONFIG) {

  # curl >= 6.3.0 es obligatorio: httr2 construye las URLs con
  # curl::curl_modify_url(), que recien existe a partir de esa version.
  minimos <- c(curl = "6.3.0", httr2 = "1.0.0", ellmer = "0.2.0", jsonlite = "1.8.0")

  message("paquetes:")
  problemas <- character(0)

  for (p in names(minimos)) {

    instalada <- tryCatch(as.character(utils::packageVersion(p)), error = function(e) NA_character_)
    cargada   <- if (p %in% loadedNamespaces()) as.character(getNamespaceVersion(p)) else NA_character_

    if (is.na(instalada)) {
      message("  ", p, ": NO INSTALADO")
      problemas <- c(problemas, glue("falta instalar {p}: install.packages('{p}')"))
      next
    }

    detalle <- glue("  {p}: {instalada}")
    if (!is.na(cargada) && cargada != instalada) {
      detalle <- glue("{detalle} instalada / {cargada} CARGADA EN MEMORIA")
      problemas <- c(problemas, glue("reiniciar R: hay una version vieja de {p} cargada en memoria"))
    }

    vieja <- utils::compareVersion(instalada, minimos[[p]]) < 0
    if (vieja) {
      detalle <- glue("{detalle}  <-- DESACTUALIZADO, requiere >= {minimos[[p]]}")
      problemas <- c(problemas, glue("actualizar {p} a >= {minimos[[p]]}"))
    }
    message(detalle)
  }

  if (length(problemas)) {
    message("\nhay que resolver esto antes de codificar:")
    for (p in unique(problemas)) message("  - ", p)
    message("  en Windows, si install.packages() no logra reemplazar el paquete, cerrar")
    message("  todas las sesiones de R y reinstalar desde una sesion nueva y limpia")
  }

  if (!identical(CONFIG$provider %||% "ollama", "ollama")) {
    return(invisible(length(problemas) == 0))
  }

  base <- CONFIG$ollama_base_url %||% "http://localhost:11434"

  ok <- tryCatch({
    resp <- httr2::request(paste0(base, "/api/tags")) |>
      httr2::req_timeout(10) |>
      httr2::req_perform()
    modelos <- vapply(httr2::resp_body_json(resp)$models,
                      function(m) as.character(m$name), character(1))
    message("ollama responde en ", base)
    message("  modelos disponibles: ", paste(modelos, collapse = ", "))

    pedido <- CONFIG$model %||% "qwen3:8b"
    if (!any(startsWith(modelos, sub(":.*$", "", pedido)))) {
      warning("el modelo '", pedido, "' no aparece descargado: correr  ollama pull ", pedido,
              call. = FALSE)
    }
    TRUE
  }, error = function(e) {
    message("ollama NO responde en ", base)
    message("  ", conditionMessage(e))
    message("  1) instalarlo desde https://ollama.com/download si no esta")
    message("  2) dejarlo corriendo:  ollama serve   (una terminal aparte, no cerrarla)")
    message("  3) descargar el modelo: ollama pull ", CONFIG$model %||% "qwen3:8b")
    FALSE
  })

  invisible(isTRUE(ok) && length(problemas) == 0)
}

# ---- 8. Clasificacion -------------------------------------------------------

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

#' Clasifica un lote que comparte prompt de sistema.
#' Intenta en paralelo; si no se puede, va secuencial.
codificar_lote <- function(prompt_sistema, textos, etiquetas, CONFIG) {

  tipo <- tipo_salida(etiquetas)
  n    <- length(textos)
  if (n == 0) return(character(0))

  paralelo <- tryCatch({
    chat <- build_chat(prompt_sistema, CONFIG)
    res <- ellmer::parallel_chat_structured(
      chat, as.list(textos), tipo,
      max_active = CONFIG$max_active %||% 4L,
      on_error   = "continue"
    )
    normalizar_codigos(res, n)
  }, error = function(e) {
    message("    sin paralelo (", conditionMessage(e), "), voy secuencial")
    NULL
  })

  if (!is.null(paralelo)) return(paralelo)

  # Camino secuencial. Los errores se registran y se reportan: si algo esta mal
  # configurado, el mensaje del modelo importa mas que la cuenta de fallos.
  fallos <- character(0)

  salida <- vapply(textos, function(txt) {
    tryCatch({
      chat <- build_chat(prompt_sistema, CONFIG)
      r <- chat$chat_structured(txt, type = tipo)
      v <- unlist(r$codigos, use.names = FALSE)
      v <- v[!is.na(v) & nzchar(v)]
      if (!length(v)) "ERROR" else paste(unique(v), collapse = "; ")
    }, error = function(e) {
      fallos <<- c(fallos, conditionMessage(e))
      "ERROR"
    })
  }, character(1), USE.NAMES = FALSE)

  if (length(fallos)) {
    tipos <- table(substr(fallos, 1, 160))
    message("    ", length(fallos), " de ", n, " fallaron. Mensajes del modelo:")
    for (i in seq_along(tipos)) {
      message("      ", tipos[[i]], "x  ", names(tipos)[i])
    }
  }

  salida
}

#' Codifica una pregunta abierta completa.
#' Con dependencia: agrupa por la respuesta cerrada, un prompt por grupo.
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

# ---- 9. Control de calidad --------------------------------------------------

muestra_revision <- function(out, cfg, prop = 0.20, seed = 1234) {

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

    tibble(
      pregunta     = sub("^codigo_", "", cm),
      n            = sum(ok),
      exacto       = round(100 * mean(mapply(setequal, sa, sb)), 1),
      algun_codigo = round(100 * mean(mapply(function(x, y) length(intersect(x, y)) > 0, sa, sb)), 1),
      jaccard      = round(mean(mapply(function(x, y) {
        u <- length(union(x, y)); if (u == 0) 1 else length(intersect(x, y)) / u
      }, sa, sb)), 2)
    )
  })
}

#' Distribucion de codigos de una pregunta.
#' Se arma a mano y no con as.data.frame(table(...)) porque cuando hay un solo
#' codigo distinto la tabla pierde la dimension y queda de una sola columna.
frecuencia_codigos <- function(out, q) {

  cc <- paste0("codigo_", q)
  if (!cc %in% names(out)) stop("no existe la columna ", cc)

  v <- unlist(strsplit(as.character(out[[cc]]), "\\s*;\\s*"), use.names = FALSE)
  v <- trimws(v)
  v <- v[!is.na(v) & nzchar(v)]

  if (!length(v)) return(tibble(codigo = character(0), n = integer(0), pct = numeric(0)))

  tb <- table(v)
  tibble(
    codigo = names(tb),
    n      = as.integer(tb),
    pct    = round(100 * as.integer(tb) / sum(!is.na(out[[cc]])), 1)
  ) |>
    dplyr::arrange(dplyr::desc(n))
}
