# ==============================================================================
# api_download.R  —  Export de la plataforma (previo al paso 1)
#
# Descarga el reporte de analytics (tipo lista) de WCX para la ronda vigente:
# autentica contra la API, inicia la exportación, espera a que termine y baja
# el CSV resultante. Es el "Export de la plataforma" que code/matching.R usa
# como input.
#
# Input : project.yml (ronda$campaign) + credenciales en .Renviron
#         (ver .Renviron.example)
# Output: data/raw/campaigns_wcx/<campaign>_casos.csv
#
# Correr desde la raíz del proyecto:  Rscript code/api_download.R
# ==============================================================================

library(httr2)
library(jsonlite)
library(yaml)

# ---- config ------------------------------------------------------------

if (file.exists(".Renviron")) readRenviron(".Renviron")

base_url <- "https://api.wcx.cloud"
api_key  <- Sys.getenv("WCX_API_KEY")
user     <- Sys.getenv("WCX_USER")

if (!nzchar(api_key) || !nzchar(user)) {
  stop(
    "Debes tener WCX_API_KEY y WCX_USER en .Renviron del proyecto",
    call. = FALSE
  )
}

cfg      <- yaml::read_yaml("project.yml")
campaign <- cfg$ronda$campaign   # base del nombre de archivo, ej. "r1_20260807"
outfile  <- paste0("data/raw/campaigns_wcx/", campaign, ".csv")

# tiempo de espera y reintentos (para evitar 504)
http_timeout   <- 60   # segundos totales por solicitud
http_max_tries <- 5    # número máximo de reintentos para 408/429/5xx

# rango de fechas para filtrar casos (ajustable)

start_date <- "2026-08-07 00:00:00"
end_date   <- "2026-08-12 23:59:59"

# reporte de analytics (tipo lista) a exportar y forma de descarga
report_id       <- "238012" # Se debe cambiar manual desde analytics 
export_columns  <- "all"   # "all" | "only_visible" | NULL (usa lo configurado en el reporte)
export_group_by <- NULL    # "d" | "h" | "m" | NULL (usa lo configurado en el reporte)
poll_interval   <- 5       # segundos entre consultas de estado
poll_timeout    <- 600     # segundos máximos de espera de la exportación

`%||%` <- function(x, y) if (is.null(x)) y else x                          # default si es NULL
stop_if_empty <- function(x, msg) if (!nzchar(x)) stop(msg, call. = FALSE)  # corta si vino vacío

# ---- funciones -----------------------------------------------------------

# arma el request base: headers, user-agent, timeout y reintentos para toda llamada a la API
wcx_req_raw <- function(path) {
  request(base_url) |>
    req_url_path_append(path) |>
    req_headers(
      "x-api-key" = api_key,
      Accept      = "application/json"
    ) |>
    req_user_agent("focus-r-client/0.6") |>
    req_timeout(http_timeout) |>
    req_retry(max_tries = http_max_tries) |>
    req_error(
      is_error = \(r) r$status_code >= 400,
      body     = \(r) paste("HTTP", r$status_code, "–", resp_body_string(r))
    )
}

# agrega el JWT como header Authorization a un request ya armado
token_hdr <- function(req, tok) {
  req |> req_headers(Authorization = paste("Bearer", tok))
}

# login: intercambia x-api-key + user por un JWT (válido 1h, hay que renovarlo si expira)
wcx_auth <- function() {
  message("🔑  Autenticando…")
  resp <- wcx_req_raw(c("core", "v1", "authenticate")) |>
    req_url_query(user = user) |>
    req_perform()
  j     <- resp_body_json(resp, simplifyVector = TRUE)
  token <- j$token %||% j$accessToken
  stop_if_empty(token, "token vacío")
  message("✅  Token listo")
  token
}

# pide iniciar la exportación de un reporte ya guardado en WiseCX (tipo lista, no dashboard);
# devuelve un export_id que hay que consultar aparte, la exportación corre async del lado del server
wcx_analytics_export_start <- function(report_id, token, date_from, date_to,
                                        columns = NULL, group_by = NULL) {
  message("📊  Iniciando exportación del reporte ", report_id, "…")

  # date_from/date_to sobreescriben el rango de fechas configurado en el reporte
  body <- list(filter = list(date_from = date_from, date_to = date_to))
  if (!is.null(columns))  body$columns  <- columns
  if (!is.null(group_by)) body$group_by <- group_by

  resp <- wcx_req_raw(c("core", "v1", "analytics", "export", report_id)) |>
    token_hdr(token) |>
    req_body_json(body) |>
    req_perform()

  j         <- resp_body_json(resp, simplifyVector = TRUE)
  export_id <- j$export_id
  stop_if_empty(export_id, "export_id vacío")
  message("✅  export_id: ", export_id)
  export_id
}

# una sola consulta de estado: pending -> processing -> completed (con report_url) | error
wcx_analytics_export_status <- function(export_id, token) {
  resp <- wcx_req_raw(c("core", "v1", "analytics", "export", export_id, "status")) |>
    token_hdr(token) |>
    req_perform()
  resp_body_json(resp, simplifyVector = TRUE)
}

# hace polling del estado hasta que termine (o falle, o se pase el timeout) y
# devuelve la URL firmada de S3 desde donde bajar el CSV
wcx_analytics_export_wait <- function(export_id, token,
                                       interval = poll_interval, timeout = poll_timeout) {
  start_t <- Sys.time()
  repeat {
    st <- wcx_analytics_export_status(export_id, token)
    message("   estado: ", st$status, " (", st$progress %||% 0, "%)")

    if (identical(st$status, "completed")) return(st$report_url)
    if (identical(st$status, "error"))     stop("La exportación falló (status = error)", call. = FALSE)

    elapsed <- as.numeric(difftime(Sys.time(), start_t, units = "secs"))
    if (elapsed > timeout) stop("Timeout esperando la exportación del reporte", call. = FALSE)

    Sys.sleep(interval)
  }
}

# ---- flujo principal -------------------------------------------------

token <- wcx_auth()

# la API pide ISO 8601 con "T"; start_date/end_date arriba están en formato "yyyy-mm-dd HH:MM:SS"
date_from <- format(as.POSIXct(start_date), "%Y-%m-%dT%H:%M:%S")
date_to   <- format(as.POSIXct(end_date),   "%Y-%m-%dT%H:%M:%S")

# 1) inicia la exportación (async) y 2) espera a que termine para tener la URL de descarga
export_id <- wcx_analytics_export_start(
  report_id = report_id,
  token     = token,
  date_from = date_from,
  date_to   = date_to,
  columns   = export_columns,
  group_by  = export_group_by
)

report_url <- wcx_analytics_export_wait(export_id, token)

# report_url ya es una URL firmada de S3: no necesita headers de auth
dir.create(dirname(outfile), recursive = TRUE, showWarnings = FALSE)

tmp <- tempfile()
download.file(report_url, destfile = tmp, mode = "wb", quiet = TRUE)

# WCX entrega el reporte comprimido en .zip aunque la URL no lo diga: si no se
# descomprime, queda un .csv que en realidad son bytes de un zip
es_zip <- identical(readBin(tmp, "raw", n = 2), as.raw(c(0x50, 0x4B)))
if (es_zip) {
  csv_en_zip <- unzip(tmp, list = TRUE)$Name[1]
  unzip(tmp, files = csv_en_zip, exdir = dirname(outfile), junkpaths = TRUE)
  file.rename(file.path(dirname(outfile), basename(csv_en_zip)), outfile)
  file.remove(tmp)
} else {
  file.rename(tmp, outfile)
}

message(sprintf("descargado: %s", outfile))



