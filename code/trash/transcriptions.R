# ------------------------------------------------------------------------------
# SCRIPT R COMPLETO Y CORREGIDO PARA:
# - Descargar solo celdas que contienen links a archivos .ogg
# - Subirlos a Drive para que Colab los procese con WhisperX
# - Descargar .txt con transcripciones desde Drive
# - Insertar las transcripciones en el CSV original reemplazando los links
# ------------------------------------------------------------------------------

# 0) CARGAR LIBRERÍAS Y AUTENTICAR EN GOOGLE DRIVE
# ---------------------------------------------------
library(googledrive)
library(readr)
library(dplyr)
library(httr)

# Autenticación con la cuenta usada en Colab
drive_auth(email = "soyfocusuy@gmail.com")


# 1) DEFINIR RUTAS Y CONFIGURACIONES
# ----------------------------------
base_local <- "data/transcriptions"
round_id <- "R5"
ruta_voz <- file.path(base_local, "Q", round_id, "voice")
ruta_textos <- file.path(base_local, round_id, "text")

# Crear carpetas locales
dir.create(ruta_voz, recursive = TRUE, showWarnings = FALSE)
dir.create(ruta_textos, recursive = TRUE, showWarnings = FALSE)

# CSV original con links a .ogg
csv_input_path <- paste0("data/matched/", round_id, ".csv")
df_original <- read_csv(csv_input_path, col_types = cols(.default = "c"))


# 2) FUNCIONES AUXILIARES
# ------------------------

# Limpiar nombres para usarlos como nombre de archivo
sanear_nombre <- function(nombre) {
  nombre %>%
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>%
    gsub("[^A-Za-z0-9]", "_", .) %>%
    gsub("_+", "_", .) %>%
    gsub("^_|_$", "", .)
}

# Descargar un .ogg
descargar_ogg <- function(link, carpeta_destino, nombre_archivo) {
  destino <- file.path(carpeta_destino, nombre_archivo)
  if (file.exists(destino)) return(destino)
  resp <- httr::GET(link, httr::write_disk(destino, overwrite = TRUE))
  httr::stop_for_status(resp)
  return(destino)
}

# Subir .ogg a Drive
upload_gd_ogg <- function(
    path0 = "data/transcriptions/Q/",       # carpeta local base
    round                    # id de ronda (p.ej. "R3")
) {
  dir_local  <- file.path(path0, round, "voice")
  drive_path <- paste0("A/", round, "/")
  
  archivos <- list.files(dir_local, full.names = TRUE, pattern = "\\.ogg$")
  if (length(archivos) == 0) {
    message("No se encontraron archivos .ogg en: ", dir_local)
    return(invisible(NULL))
  }
  for (p in archivos) {
    drive_upload(p, path = drive_path)
    message(sprintf("Subido a Drive: %s → %s", basename(p), drive_path))
  }
}



# 3) DETECTAR CELDAS CON .ogg Y DESCARGAR
# ---------------------------------------
for (fila_idx in seq_len(nrow(df_original))) {
  for (col in names(df_original)) {
    enlace <- df_original[[col]][fila_idx]
    if (!is.na(enlace) && grepl("\\.ogg$", enlace, ignore.case = TRUE)) {
      col_saneada <- sanear_nombre(col)
      nombre_local <- paste0(col_saneada, "_fila", fila_idx, ".ogg")
      tryCatch({
        descargar_ogg(enlace, ruta_voz, nombre_local)
        message(sprintf("Descargado %s (fila %d, columna %s)", nombre_local, fila_idx, col))
      }, error = function(e) {
        warning(sprintf("Error al descargar '%s' (fila %d, columna '%s'): %s", enlace, fila_idx, col, e$message))
      })
    }
  }
}


# 4) SUBIR LOS .ogg A DRIVE PARA COLAB
# ------------------------------------
upload_gd_ogg(path0 = "data/transcriptions/Q/", round = round_id)
message("Archivos .ogg subidos. Ejecuta el Colab y luego vuelve para continuar.")


# 5) DESCARGAR LOS .TXT DE TRANSCRIPCIONES DESDE DRIVE
# -----------------------------------------------------
archivos_drive <- drive_ls(path = paste0("B/", round_id), pattern = "\\.txt$")
for (i in seq_len(nrow(archivos_drive))) {
  id_drive    <- archivos_drive$id[i]
  nombre_txt  <- archivos_drive$name[i]
  destino_loc <- file.path(ruta_textos, nombre_txt)
  drive_download(as_id(id_drive), path = destino_loc, overwrite = TRUE)
  message(sprintf("Descargado TXT: %s", nombre_txt))
}


# 6) LEER LOS .TXT Y MAPEARLOS A FILA Y COLUMNA ORIGINAL
# ------------------------------------------------------
lista_txt <- list.files(ruta_textos, pattern = "\\.txt$", full.names = TRUE)
datos_txt <- tibble(
  nombre_txt = basename(lista_txt),
  contenido  = sapply(lista_txt, function(f) paste0(readLines(f, encoding = "UTF-8"), collapse = " "))
) %>%
  mutate(
    base    = tools::file_path_sans_ext(nombre_txt),
    columna_saneada = sub("_fila\\d+$", "", base),
    fila            = as.integer(sub("^.*_fila", "", base))
  )

# Mapeo: columna saneada -> columna original
mapa_columnas <- tibble(
  original = names(df_original),
  saneada  = sapply(names(df_original), sanear_nombre, USE.NAMES = FALSE)
)

datos_txt <- datos_txt %>%
  left_join(mapa_columnas, by = c("columna_saneada" = "saneada")) %>%
  rename(columna = original) %>%
  select(columna, fila, contenido)


# 7) INSERTAR TRANSCRIPCIONES EN EL CSV ORIGINAL
# ----------------------------------------------
df_transcripciones <- df_original
for (i in seq_len(nrow(datos_txt))) {
  col_actual <- datos_txt$columna[i]
  fila_actual <- datos_txt$fila[i]
  texto_ogg   <- datos_txt$contenido[i]
  df_transcripciones[fila_actual, col_actual] <- texto_ogg
}


# 8) GUARDAR CSV FINAL
# ---------------------
write_csv(df_transcripciones, paste0("data/processed/transcriptions/output/transcripcion_", round_id, ".csv"))
