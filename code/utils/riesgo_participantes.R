# =============================================================================
# DETECCIÓN DE PANELISTAS EN RIESGO DE ABANDONO
# Ejecutar mensualmente. Parámetros en la sección de configuración.
# =============================================================================

if(!require(dplyr)) install.packages("dplyr")
if(!require(tidyr)) install.packages("tidyr")
if(!require(readr)) install.packages("readr")
if(!require(stringr)) install.packages("stringr")
if(!require(glue)) install.packages("glue")
if(!require(googlesheets4)) install.packages("googlesheets4")
if(!require(googledrive)) install.packages("googledrive")

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(glue)
library(googlesheets4)
library(googledrive)


# =============================================================================
# CONFIGURACION
# =============================================================================

CONFIG <- list(
  year       = "2026",                                                                         # Año del panel 
  last_round = 8,                                                                             # número de la última ronda publicada  ← ACTUALIZAR
  window     = 3,                                                                              # rondas hacia atrás a evaluar
  min_nchar  = 15,                                                                             # umbral: respuesta "muy corta"
  max_absent = 2,                                                                              # umbral: rondas ausentes 
  base_cols  = c("edad", "departamento", "voto2"),                                             # columnas de interes participantes
  drv_folder = "https://drive.google.com/drive/u/0/folders/1zpB1VmQ_uZtJ179ZQXnywzE0oFAOeqih", #  URL de la carpeta raíz en Drive
  drv_spreadsheet = "Riesgo - 2026",                                                           #  nombre del spreadsheet destino <- ACTUALIZAR ANUALMENTE
  drv_sheet = "Rondas 5 a 8"                                                                          # nombre del sheet ← ACTUALIZAR
)

# -----------------------------------------------------------------------------
# FUNCIONES
# -----------------------------------------------------------------------------
load_data <- function(year) {
  path <- file.path("data", "processed", "acumulada", year, "base_acumulada.csv")
  
  if (!file.exists(path)) {
    stop(glue("No se encuentra el archivo: {path}"))
  }
  
  dat <- read_csv(path) 
  
  required_cols <- c("numero", "nombre")
  missing <- setdiff(required_cols, names(dat))
  if (length(missing) > 0) {
    stop(glue("Columnas faltantes en el CSV: {paste(missing, collapse = ', ')}"))
  }
  
  dat
}


# Convierte la base wide (una columna por pregunta) a formato largo
# Output: numero | round | question | answer | participa
 
format_data <- function(data) {
  data |> 
    mutate(across(starts_with("r"), as.character)) |> 
    select(numero, starts_with("r")) |> 
    pivot_longer(-numero, values_to = "answer") |> 
    separate_wider_delim(
      name,
      delim = "_",
      names = c("round", "question"),
      too_many = "merge"
    ) |> 
    mutate(
      round = as.integer(str_extract(round, pattern = "(\\d+)")),  
      question = as.integer(str_extract(question, pattern = "(\\d+)")),
      answer = str_trim(str_to_lower(answer))
    ) |>
    group_by(numero, round) |> 
    mutate(participa = any(!is.na(answer))) |> 
    ungroup()     
}

# Calcula por panelista en la ventana de rondas:
# n_absent, n_present y longitud promedio de respuesta

summarize_participation <- function(part_df, last, window) {
  part_df |> 
    filter(between(round, last - window, last)) |> 
    group_by(numero, round) |>
    summarize(
      participa  = first(participa),
      mean_nchar = mean(nchar(na.omit(answer)), na.rm = TRUE),
      .groups = "drop"
    ) |>
    group_by(numero) |>
    summarize(
      n_absent  = sum(!participa),    
      n_present = sum(participa),    
      mean_nchar = round(mean(mean_nchar, na.rm = TRUE)),
      .groups = "drop"
    ) 
}

flag_risk <- function(summary_df, absent_treshold, nchar_treshold) {
  summary_df |> 
    mutate(
      risk_abandono = n_absent > absent_treshold,       
      risk_nchar    = mean_nchar < nchar_treshold,       
      risk   = risk_abandono | risk_nchar
    )  
}

# Cruza panelistas en riesgo con datos sociodemográficos
# y genera el campo 'razon' con texto legible para contacto

build_contact_list <- function(risk_df, base_df, base_cols) {
  
  vars <- c("nombre", "numero", base_cols)
  

  participants_data <- base_df |> 
    select(nombre, numero, all_of(base_cols)) |>
    distinct(nombre, numero, .keep_all = TRUE)
  
  risk_df |> 
    filter(risk) |>
    mutate(
      razon = case_when(
        risk_abandono & risk_nchar ~ glue::glue("Baja participación ({n_absent} rondas ausente) y respuestas muy cortas ({round(mean_nchar, 1)} chars)"),
        risk_abandono              ~ glue::glue("Baja participación ({n_absent} rondas ausente)"),
        risk_nchar                 ~ glue::glue("Respuestas muy cortas ({round(mean_nchar, 1)} caracteres promedio)")
      )
    ) |> 
    select(numero, razon) |> 
    left_join(participants_data, by = "numero") |> 
    relocate(all_of(vars))
}
  

# Busca o crea el spreadsheet en la carpeta de Drive indicada
# y agrega un sheet nuevo con los resultados del mes

save_drive <- function(contact_df, folder_url, spreadsheet, sheetname) {
  
  folder <- drive_get(as_id(folder_url))
  if (nrow(folder) == 0) stop(glue("No se encontró la carpeta en Drive."))
  
  ss <- drive_ls(path = as_id(folder_url)) |> filter(name == spreadsheet)
  
  if (nrow(ss) > 0) {
    ss_id <- ss$id[[1]]
    message(glue("Spreadsheet encontrado: '{spreadsheet}'"))
    
    # Sobreescribir sheet si ya existe
    existing <- sheet_names(ss_id)
    if (sheetname %in% existing) {
      sheet_delete(ss_id, sheetname)
      warning(glue("Sheet '{sheetname}' ya existía, se sobreescribió."))
    }
    sheet_add(ss_id, sheet = sheetname)
    
  } else {
    # Crear spreadsheet y renombrar la hoja por defecto en vez de agregar una nueva
    ss_new <- gs4_create(spreadsheet)
    drive_mv(ss_new, path = as_id(folder_url))
    ss_id <- as.character(ss_new)
    sheet_rename(ss_id, sheet = 1, new_name = sheetname)  # renombra "Sheet1"
    message(glue("Spreadsheet creado: '{spreadsheet}'"))
  }
  
  write_sheet(data = contact_df, ss = ss_id, sheet = sheetname)
  
  message(glue("Sheet '{sheetname}' guardado en '{spreadsheet}'."))
  invisible(contact_df)
}

# -----------------------------------------------------------------------------
# EJECUCION
# -----------------------------------------------------------------------------

execute <- function(cfg = CONFIG) {
  acumulada <- load_data(cfg$year)
  
  contact <- acumulada |>
    format_data() |> 
    summarize_participation(cfg$last_round, cfg$window) |>
    flag_risk(cfg$max_absent, cfg$min_nchar) |>
    build_contact_list(acumulada, cfg$base_cols) |>
    save_drive(cfg$drv_folder, cfg$drv_spreadsheet, cfg$drv_sheet)
}

execute()