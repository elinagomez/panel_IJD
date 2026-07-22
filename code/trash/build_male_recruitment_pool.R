#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(readr)
  library(readxl)
  library(dplyr)
  library(purrr)
  library(stringr)
  library(tibble)
})

postulantes_dir <- "/Users/simonherrera/Documents/git/whatpanel/data/clean/postulantes/data"
active_path <- "data/processed/acumulada/base_acumulada.csv"
contacts_dir <- "data/raw/contacts"
latest_contacts_path <- file.path(contacts_dir, "20260309.csv")
marzo_path <- "/Users/simonherrera/Downloads/primer_contacto_nuevos_marzo.csv"
extra_xlsx_paths <- c(
  "/Users/simonherrera/Documents/Focus/projects/gd-minterior/area_metropolitana.xlsx",
  "/Users/simonherrera/Documents/Focus/projects/gd-minterior/resto_pais.xlsx"
)
out_dir <- file.path("output", "spreadsheet")
today_tag <- format(Sys.Date(), "%Y%m%d")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

normalize_phone <- function(x) {
  x <- str_replace_all(as.character(x), "[oO]", "0")
  x <- gsub("[^0-9]", "", as.character(x))
  x[x == ""] <- NA_character_
  case_when(
    !is.na(x) & nchar(x) == 12 & str_starts(x, "598") ~ x,
    !is.na(x) & nchar(x) == 9 & str_starts(x, "0") ~ paste0("598", str_sub(x, 2)),
    !is.na(x) & nchar(x) == 8 ~ paste0("598", x),
    TRUE ~ x
  )
}

age_band <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  case_when(
    is.na(x) ~ NA_character_,
    x <= 30 ~ "30 y menos",
    x <= 59 ~ "31 a 59",
    TRUE ~ "60 y más"
  )
}

get_col <- function(x, name) {
  if (name %in% names(x)) as.character(x[[name]]) else rep(NA_character_, nrow(x))
}

segment_order <- c(
  "Canelones",
  "Interior Coalición",
  "Interior Frente Amplio",
  "Montevideo"
)

contact_cap_override <- c(
  "Montevideo" = 8L
)

target_hombres_por_segmento <- 20L

fa_candidates <- c("Yamandú Orsi", "Carolina Cosse", "Mario Bergara")
coal_candidates <- c(
  "Álvaro Delgado", "Laura Raffo", "Andrés Ojeda", "Robert Silva",
  "Manini Rios", "Gabriel Gurmendez", "GANDINI", "Rafael Fernandez", "Lima"
)

read_postulante_file <- function(path) {
  env <- new.env()
  load(path, envir = env)
  obj_name <- ls(env)[1]
  df <- env[[obj_name]]

  if (!"contact" %in% names(df) && "celular" %in% names(df)) {
    df <- rename(df, contact = celular)
  }

  tibble(
    nombre = get_col(df, "nombre"),
    edad_raw = get_col(df, "edad"),
    genero_raw = get_col(df, "genero"),
    numero = normalize_phone(get_col(df, "contact")),
    departamento = str_to_title(str_to_lower(get_col(df, "depto_residencia"))),
    oficio = get_col(df, "actividad_laboral"),
    identificacion_partido = get_col(df, "identificacion_partido"),
    intencion_voto_oct2024 = get_col(df, "intencion_voto_oct2024"),
    sector_fa = get_col(df, "sector_fa"),
    espectro_politico = get_col(df, "espectro_politico"),
    archivo_origen = basename(path)
  )
}

read_excel_source <- function(path) {
  read_xlsx(path) %>%
    transmute(
      nombre = as.character(nombre),
      edad_raw = as.character(edad),
      genero_raw = as.character(genero),
      numero = normalize_phone(numero),
      departamento = str_to_title(str_to_lower(as.character(departamento))),
      oficio = NA_character_,
      identificacion_partido = as.character(voto),
      intencion_voto_oct2024 = NA_character_,
      sector_fa = NA_character_,
      espectro_politico = NA_character_,
      archivo_origen = basename(path),
      n_educativo = NA_character_,
      grupo_pauta = as.character(grupo_pauta),
      inse_num = as.character(inse_num),
      inse_cinco = as.character(inse_cinco),
      fuente = "focus_xlsx"
    )
}

postulantes_files <- list.files(postulantes_dir, pattern = "Rdata$", full.names = TRUE)

postulantes <- map_dfr(postulantes_files, read_postulante_file) %>%
  mutate(
    n_educativo = NA_character_,
    grupo_pauta = NA_character_,
    inse_num = NA_character_,
    inse_cinco = NA_character_,
    fuente = "postulantes_2024"
  ) %>%
  mutate(
    nombre = str_squish(str_to_title(str_to_lower(nombre))),
    genero = case_when(
      genero_raw == "Varón" ~ "Hombre",
      genero_raw == "Mujer" ~ "Mujer",
      TRUE ~ genero_raw
    ),
    edad = age_band(edad_raw),
    edad_num = suppressWarnings(as.numeric(edad_raw)),
    partido_limpio = str_squish(coalesce(identificacion_partido, "")),
    intencion_limpia = str_squish(coalesce(intencion_voto_oct2024, "")),
    sector_fa_limpio = str_squish(coalesce(sector_fa, "")),
    espectro_limpio = str_squish(coalesce(espectro_politico, "")),
    es_fa_alta = str_detect(str_to_lower(partido_limpio), "frente") |
      sector_fa_limpio != "" |
      intencion_limpia %in% fa_candidates,
    es_coal_alta = (
      partido_limpio != "" &
        partido_limpio != "Prefiero no responder" &
        !str_detect(str_to_lower(partido_limpio), "frente")
    ) | intencion_limpia %in% coal_candidates,
    es_fa_media = !es_fa_alta & !es_coal_alta &
      espectro_limpio %in% c("Izquierda", "Centro - izquierda"),
    es_coal_media = !es_fa_alta & !es_coal_alta &
      espectro_limpio %in% c("Derecha", "Centro - derecha"),
    segmento = case_when(
      departamento == "Montevideo" ~ "Montevideo",
      departamento == "Canelones" ~ "Canelones",
      departamento != "Montevideo" & departamento != "Canelones" & es_fa_alta ~ "Interior Frente Amplio",
      departamento != "Montevideo" & departamento != "Canelones" & es_coal_alta ~ "Interior Coalición",
      departamento != "Montevideo" & departamento != "Canelones" & es_fa_media ~ "Interior Frente Amplio",
      departamento != "Montevideo" & departamento != "Canelones" & es_coal_media ~ "Interior Coalición",
      TRUE ~ NA_character_
    ),
    voto = case_when(
      segmento == "Interior Frente Amplio" ~ "Frente Amplio",
      segmento == "Interior Coalición" ~ "Coalición",
      TRUE ~ NA_character_
    ),
    voto2 = case_when(
      segmento == "Interior Frente Amplio" ~ "FA",
      segmento == "Interior Coalición" ~ "CM",
      TRUE ~ NA_character_
    ),
    segmentacion_confianza = case_when(
      departamento %in% c("Canelones", "Montevideo") ~ "alta",
      es_fa_alta | es_coal_alta ~ "alta",
      es_fa_media | es_coal_media ~ "media",
      TRUE ~ NA_character_
    ),
    criterio_segmentacion = case_when(
      departamento == "Canelones" ~ "departamento",
      departamento == "Montevideo" ~ "departamento",
      es_fa_alta & str_detect(str_to_lower(partido_limpio), "frente") ~ "partido_frente_amplio",
      es_fa_alta & sector_fa_limpio != "" ~ "sector_fa",
      es_fa_alta & intencion_limpia %in% fa_candidates ~ "candidato_fa",
      es_coal_alta & intencion_limpia %in% coal_candidates ~ "candidato_coalicion",
      es_coal_alta & partido_limpio != "" ~ "partido_no_fa",
      es_fa_media ~ "espectro_izquierda",
      es_coal_media ~ "espectro_derecha",
      TRUE ~ NA_character_
    )
  )

focus_xlsx <- map_dfr(extra_xlsx_paths, read_excel_source) %>%
  mutate(
    nombre = str_squish(str_to_title(str_to_lower(nombre))),
    genero = case_when(
      genero_raw == "Varón" ~ "Hombre",
      genero_raw == "Mujer" ~ "Mujer",
      TRUE ~ genero_raw
    ),
    edad = age_band(edad_raw),
    edad_num = suppressWarnings(as.numeric(edad_raw)),
    partido_limpio = str_squish(coalesce(identificacion_partido, "")),
    intencion_limpia = str_squish(coalesce(intencion_voto_oct2024, "")),
    sector_fa_limpio = str_squish(coalesce(sector_fa, "")),
    espectro_limpio = str_squish(coalesce(espectro_politico, "")),
    es_fa_alta = partido_limpio == "Frente Amplio",
    es_coal_alta = partido_limpio != "" &
      !partido_limpio %in% c("Frente Amplio", "Prefiero no decirlo"),
    es_fa_media = FALSE,
    es_coal_media = FALSE,
    segmento = case_when(
      departamento == "Montevideo" ~ "Montevideo",
      departamento == "Canelones" ~ "Canelones",
      departamento != "Montevideo" & departamento != "Canelones" & es_fa_alta ~ "Interior Frente Amplio",
      departamento != "Montevideo" & departamento != "Canelones" & es_coal_alta ~ "Interior Coalición",
      TRUE ~ NA_character_
    ),
    voto = case_when(
      segmento == "Interior Frente Amplio" ~ "Frente Amplio",
      segmento == "Interior Coalición" ~ "Coalición",
      TRUE ~ NA_character_
    ),
    voto2 = case_when(
      segmento == "Interior Frente Amplio" ~ "FA",
      segmento == "Interior Coalición" ~ "CM",
      TRUE ~ NA_character_
    ),
    segmentacion_confianza = case_when(
      departamento %in% c("Canelones", "Montevideo") ~ "alta",
      partido_limpio == "Frente Amplio" ~ "alta",
      es_coal_alta ~ "alta",
      TRUE ~ NA_character_
    ),
    criterio_segmentacion = case_when(
      departamento == "Canelones" ~ "departamento",
      departamento == "Montevideo" ~ "departamento",
      partido_limpio == "Frente Amplio" ~ "voto_focus_fa",
      es_coal_alta ~ "voto_focus_no_fa",
      TRUE ~ NA_character_
    )
  )

all_sources <- bind_rows(postulantes, focus_xlsx)

active <- read_csv(active_path, show_col_types = FALSE) %>%
  mutate(numero = normalize_phone(numero))

contacts_files <- list.files(contacts_dir, pattern = "\\.csv$", full.names = TRUE)
contact_numbers <- unlist(
  map(contacts_files, function(path) {
    df <- read_csv(path, show_col_types = FALSE, progress = FALSE)
    normalize_phone(df$numero)
  }),
  use.names = FALSE
)

marzo <- read_csv(marzo_path, show_col_types = FALSE) %>%
  mutate(numero = normalize_phone(`Teléfono de Envío`))

exclude_numbers <- unique(c(active$numero, contact_numbers, marzo$numero))

male_pool_all <- all_sources %>%
  filter(
    genero == "Hombre",
    !is.na(numero),
    !numero %in% exclude_numbers,
    is.na(edad_num) | edad_num >= 18
  ) %>%
  distinct(numero, .keep_all = TRUE)

male_pool_segmented <- male_pool_all %>%
  filter(!is.na(segmento)) %>%
  mutate(
    etiqueta = NA_character_,
    origen = case_when(
      fuente == "focus_xlsx" ~ "focus_xlsx_nuevos",
      TRUE ~ "postulantes_2024_nuevos"
    )
  )

male_pool_unclassified <- male_pool_all %>%
  filter(is.na(segmento)) %>%
  transmute(
    nombre,
    edad = age_band(edad_raw),
    genero = "Hombre",
    numero,
    departamento,
    oficio,
    identificacion_partido,
    intencion_voto_oct2024,
    sector_fa,
    espectro_politico,
    archivo_origen,
    motivo_exclusion = "sin_segmento_politico_claro"
  ) %>%
  arrange(departamento, nombre)

participant_counts <- active %>%
  mutate(
    segmento = str_squish(segmento),
    genero = str_squish(genero)
  ) %>%
  count(segmento, genero, name = "n") %>%
  filter(segmento %in% segment_order)

latest_contacts <- read_csv(latest_contacts_path, show_col_types = FALSE) %>%
  mutate(
    numero = normalize_phone(numero),
    origen = paste0("contacts_", tools::file_path_sans_ext(basename(latest_contacts_path)))
  )

latest_male_counts <- latest_contacts %>%
  filter(genero == "Hombre") %>%
  count(segmento, name = "hombres_latest_contacts")

available_counts <- male_pool_segmented %>%
  count(segmento, name = "hombres_nuevos_disponibles")

summary_tbl <- tibble(segmento = segment_order) %>%
  left_join(
    participant_counts %>%
      filter(genero == "Hombre") %>%
      select(segmento, hombres_participantes = n),
    by = "segmento"
  ) %>%
  left_join(
    participant_counts %>%
      filter(genero == "Mujer") %>%
      select(segmento, mujeres_participantes = n),
    by = "segmento"
  ) %>%
  left_join(latest_male_counts, by = "segmento") %>%
  left_join(available_counts, by = "segmento") %>%
  mutate(
    hombres_participantes = coalesce(hombres_participantes, 0L),
    mujeres_participantes = coalesce(mujeres_participantes, 0L),
    hombres_latest_contacts = coalesce(hombres_latest_contacts, 0L),
    hombres_nuevos_disponibles = coalesce(hombres_nuevos_disponibles, 0L),
    deficit_hombres_participantes = pmax(20L - hombres_participantes, 0L),
    deficit_hombres_latest_contacts = pmax(20L - hombres_latest_contacts, 0L),
    sugeridos_contactar_primero = case_when(
      deficit_hombres_participantes > 0L ~ pmin(
        hombres_nuevos_disponibles,
        pmax(deficit_hombres_participantes * 3L, deficit_hombres_latest_contacts + 2L, 8L)
      ),
      deficit_hombres_latest_contacts > 0L ~ pmin(
        hombres_nuevos_disponibles,
        pmax(deficit_hombres_latest_contacts + 4L, 8L)
      ),
      TRUE ~ pmin(hombres_nuevos_disponibles, 8L)
    ),
    alcanza_objetivo_20_hombres = hombres_participantes + hombres_nuevos_disponibles >= 20L,
    alcanza_20_hombres_en_contacts_merged = hombres_latest_contacts + hombres_nuevos_disponibles >= 20L
  )

male_pool_final <- male_pool_segmented %>%
  mutate(
    segmento = factor(segmento, levels = segment_order),
    confidence_rank = case_when(
      segmentacion_confianza == "alta" ~ 1L,
      segmentacion_confianza == "media" ~ 2L,
      TRUE ~ 3L
    )
  ) %>%
  arrange(segmento, confidence_rank, nombre) %>%
  group_by(segmento) %>%
  mutate(prioridad_segmento = row_number()) %>%
  ungroup() %>%
  mutate(segmento = as.character(segmento)) %>%
  left_join(
    summary_tbl %>%
      select(
        segmento,
        deficit_hombres_participantes,
        deficit_hombres_latest_contacts,
        sugeridos_contactar_primero
      ),
    by = "segmento"
  ) %>%
  mutate(
    contactar_primero = prioridad_segmento <= sugeridos_contactar_primero
  ) %>%
  select(
    nombre,
    edad,
    genero,
    numero,
    departamento,
    n_educativo,
    oficio,
    grupo_pauta,
    inse_num,
    inse_cinco,
    voto,
    segmento,
    voto2,
    etiqueta,
    origen,
    fuente,
    archivo_origen,
    identificacion_partido,
    intencion_voto_oct2024,
    sector_fa,
    espectro_politico,
    segmentacion_confianza,
    criterio_segmentacion,
    prioridad_segmento,
    contactar_primero,
    deficit_hombres_participantes,
    deficit_hombres_latest_contacts
  )

male_pool_balanced <- male_pool_final %>%
  group_by(segmento) %>%
  mutate(
    cap_contacto = if_else(
      segmento %in% names(contact_cap_override),
      unname(contact_cap_override[segmento]),
      n()
    ),
    contacto_balanceado = prioridad_segmento <= cap_contacto
  ) %>%
  ungroup() %>%
  filter(contacto_balanceado) %>%
  select(-cap_contacto, -contacto_balanceado)

merged_contacts <- latest_contacts %>%
  mutate(
    archivo_origen = basename(latest_contacts_path),
    grupo_pauta = NA_character_,
    inse_num = NA_character_,
    inse_cinco = NA_character_,
    identificacion_partido = NA_character_,
    intencion_voto_oct2024 = NA_character_,
    sector_fa = NA_character_,
    espectro_politico = NA_character_,
    segmentacion_confianza = "ya_existente",
    criterio_segmentacion = "ya_existente",
    prioridad_segmento = NA_integer_,
    contactar_primero = FALSE,
    deficit_hombres_participantes = NA_integer_,
    deficit_hombres_latest_contacts = NA_integer_,
    fuente = "contacts_latest"
  ) %>%
  select(names(male_pool_final)) %>%
  bind_rows(male_pool_final) %>%
  distinct(numero, .keep_all = TRUE)

target_new_males <- male_pool_final %>%
  left_join(
    summary_tbl %>%
      select(segmento, hombres_latest_contacts),
    by = "segmento"
  ) %>%
  group_by(segmento) %>%
  mutate(
    necesarios_para_target = pmax(target_hombres_por_segmento - first(hombres_latest_contacts), 0),
    seleccionado_target20 = prioridad_segmento <= necesarios_para_target
  ) %>%
  ungroup()

merged_target20 <- latest_contacts %>%
  mutate(
    archivo_origen = basename(latest_contacts_path),
    grupo_pauta = NA_character_,
    inse_num = NA_character_,
    inse_cinco = NA_character_,
    identificacion_partido = NA_character_,
    intencion_voto_oct2024 = NA_character_,
    sector_fa = NA_character_,
    espectro_politico = NA_character_,
    segmentacion_confianza = "ya_existente",
    criterio_segmentacion = "ya_existente",
    prioridad_segmento = NA_integer_,
    contactar_primero = FALSE,
    deficit_hombres_participantes = NA_integer_,
    deficit_hombres_latest_contacts = NA_integer_,
    fuente = "contacts_latest"
  ) %>%
  select(names(male_pool_final)) %>%
  bind_rows(
    target_new_males %>%
      filter(seleccionado_target20) %>%
      select(names(male_pool_final))
  ) %>%
  distinct(numero, .keep_all = TRUE)

male_pool_path <- file.path(out_dir, paste0("contacts_masculinos_nuevos_", today_tag, ".csv"))
merged_path <- file.path(out_dir, paste0("contacts_merged_with_latest_", today_tag, ".csv"))
balanced_path <- file.path(out_dir, paste0("contacts_masculinos_balanceados_", today_tag, ".csv"))
target20_path <- file.path(out_dir, paste0("contacts_merged_target20_hombres_", today_tag, ".csv"))
summary_path <- file.path(out_dir, paste0("resumen_balance_hombres_", today_tag, ".csv"))
unclassified_path <- file.path(out_dir, paste0("hombres_interior_sin_segmento_claro_", today_tag, ".csv"))

write_csv(male_pool_final, male_pool_path, na = "")
write_csv(merged_contacts, merged_path, na = "")
write_csv(male_pool_balanced, balanced_path, na = "")
write_csv(merged_target20, target20_path, na = "")
write_csv(summary_tbl, summary_path, na = "")
write_csv(male_pool_unclassified, unclassified_path, na = "")

cat("male_pool:", male_pool_path, "\n")
cat("merged:", merged_path, "\n")
cat("balanced:", balanced_path, "\n")
cat("target20:", target20_path, "\n")
cat("summary:", summary_path, "\n")
cat("unclassified:", unclassified_path, "\n\n")
print(summary_tbl)
