  library(httr2)
  library(jsonlite)
  library(dplyr)
  library(purrr)
  library(tidyr)
  library(readr)


# configuración
base_url <- "https://api.wcx.cloud"
api_key  <- Sys.getenv("WCX_API_KEY")
user     <- Sys.getenv("WCX_USER")

# tiempo de espera y reintentos (para evitar 504)
http_timeout   <- 60   # segundos totales por solicitud
http_max_tries <- 5    # número máximo de reintentos para 408/429/5xx

# rango de fechas para filtrar casos (ajustable)
start_date <- "2026-05-26 00:00:00"
end_date   <- "2026-05-28 23:59:59"

# salida(s)
# outfile_conv    <- "conversaciones_nitro.csv"
# outfile_encuesta<- "data/encuesta.csv"
outfile_wide    <- "data/raw/campaigns_wcx/r14_20260526_api.csv"

dir.create("data", showWarnings = FALSE, recursive = TRUE)

`%||%` <- function(x, y) if (is.null(x)) y else x
stop_if_empty <- function(x, msg) if (!nzchar(x)) stop(msg, call. = FALSE)

# --- autenticación ---
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

token_hdr <- function(req, tok) {
  req |> req_headers(Authorization = paste("Bearer", tok))
}

wcx_auth <- function() {
  message("🔑  Autenticando…")
  resp <- wcx_req_raw(c("core","v1","authenticate")) |>
    req_url_query(user = user) |>
    req_perform()
  j     <- resp_body_json(resp, simplifyVector = TRUE)
  token <- j$token %||% j$accessToken
  stop_if_empty(token, "token vacío")
  message("✅  Token listo")
  token
}

# --- descarga ---
make_filter <- function(ini, fin) {
  toJSON(
    list(
      list(field = "case.created_at", operator = "GREATER EQUAL", value = ini),
      list(field = "case.created_at", operator = "LOWER EQUAL",   value = fin)
    ),
    auto_unbox = TRUE
  )
}

get_cases_page <- function(ini, fin, token, page = 1, limit = 100) {
  resp <- wcx_req_raw(c("core","v1","cases")) |>
    token_hdr(token) |>
    req_url_query(
      page      = page,
      limit     = limit,
      fields    = "id,number,status,contact_id,created_at,tags",
      filtering = make_filter(ini, fin)
    ) |>
    req_perform()
  resp_body_json(resp, simplifyVector = TRUE)
}

collect_all_cases_by_date <- function(ini, fin, token) {
  page <- 1
  out  <- list()
  repeat {
    message("📄  Casos, página ", page, "…")
    j <- get_cases_page(ini, fin, token, page)
    if (length(j$data) == 0) break
    out[[page]] <- as_tibble(j$data)
    if (nrow(out[[page]]) < j$paging$limit[[1]]) break
    page <- page + 1
  }
  bind_rows(out)
}

get_activities_case <- function(case_id, token, limit = 200) {
  message("   → Actividades caso ", case_id, "…")

  page <- 1
  out  <- list()
  repeat {
    # paginamos para evitar respuestas muy grandes que puedan causar 504
    resp <- wcx_req_raw(c("core","v1","cases", case_id, "activities")) |>
      token_hdr(token) |>
      req_url_query(
        page   = page,
        limit  = limit,
        fields = paste(
          c("id","case_id","type","user_id","channel","content",
            "contact_from","contacts_to","attachments","recordings",
            "created_at","sending_status"),
          collapse = ","
        )
      ) |>
      req_perform()

    j <- resp_body_json(resp, simplifyVector = TRUE)

    # soportar dos formas de respuesta: con/ sin clave `data`
    items <- if (is.list(j) && !is.null(j$data)) j$data else j

    # si no hay elementos, salimos
    if (is.null(items) || length(items) == 0) break

    df <- tryCatch(as_tibble(items), error = function(e) tibble())
    if (nrow(df) == 0) break

    out[[page]] <- df

    # si la página regresó menos del límite, ya no hay más
    if (nrow(df) < limit) break

    page <- page + 1
  }

  if (length(out) == 0) return(tibble())
  bind_rows(out)
}

collect_all_activities <- function(case_ids, token) {
  # Pequeño espacio entre requests para no saturar el endpoint
  fn <- function(id) {
    res <- get_activities_case(id, token = token)
    Sys.sleep(0.2)
    res
  }
  purrr::map_dfr(case_ids, fn)
}

message("🔍  Buscando casos entre “", start_date, "” y “", end_date, "”…")
token <- wcx_auth()
cases <- collect_all_cases_by_date(start_date, end_date, token)
if (nrow(cases) == 0) stop("No se encontraron casos en ese rango.")
message("   ➜ ", nrow(cases), " casos encontrados")

message("📥  Descargando actividades de cada caso…")
activities_raw <- collect_all_activities(cases$id, token)

activities <- activities_raw %>%
  left_join(
    cases %>% rename(case_id = id) %>% select(case_id, case_number = number, case_status = status),
    by = "case_id"
  )

# write_csv(activities, outfile_conv, na = "")
# message("✅  Conversaciones guardadas en ‘", outfile_conv, "’")

# procesamiento
activities_flat <- activities %>%
  unnest_wider(contact_from, names_sep = "_") %>%
  unnest_wider(contacts_to,   names_sep = "_") %>%
  mutate(
    attachment_links = map_chr(attachments, function(att) {
      if (is.null(att) || (is.data.frame(att) && nrow(att) == 0)) {
        NA_character_
      } else {
        paste0(att$url, collapse = ";")
      }
    })
  ) %>%
  select(
    id,
    case_id,
    case_number,
    case_status,
    user_id,
    channel,
    content,
    contact_from_id,
    contact_from_name,
    contact_from_phone,
    contact_from_email,
    contacts_to_id,
    contacts_to_name,
    contacts_to_phone,
    contacts_to_email,
    attachment_links,
    created_at,
    sending_status
  )

activities2_flat <- activities_flat %>%
  filter(!is.na(contact_from_phone)) %>%
  filter(!sending_status %in% c("opened", "delivered"))

encuesta <- activities2_flat %>%
  select(case_number, content, contact_from_phone, attachment_links, created_at) %>%
  rename(
    caso   = case_number,
    texto  = content,
    numero = contact_from_phone,
    adjuntos = attachment_links,
    fecha  = created_at
  ) %>%
  mutate(
    texto = if_else(texto == "Ha enviado un Audio", adjuntos, texto),
    fecha = readr::parse_datetime(fecha)
  ) %>%
  select(-adjuntos)

# write_csv(encuesta, outfile_encuesta)
# message("✅  Encuesta guardada en ‘", outfile_encuesta, "’")

# --- reshape ---
df_indexado <- encuesta %>%
  arrange(numero, fecha) %>%
  group_by(numero) %>%
  mutate(
    nro_preg = row_number() - 1,
    preg     = paste0("q", nro_preg)
  ) %>%
  ungroup()

df_wide <- df_indexado %>%
  pivot_wider(
    id_cols     = numero,
    names_from  = preg,
    values_from = texto,
    values_fn   = list(texto = ~ paste(.x, collapse = " | ")),
    values_fill = list(texto = NA_character_)
  )

write_csv(df_wide, outfile_wide)
message("✅  Reshape guardado en ‘", outfile_wide, "’")
