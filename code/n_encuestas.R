##############################
# n_encuestas.R
#
# Objetivo: contar encuestas por contacto en un rango de rondas,
# unir con la última base de participantes y exportar:
#  - n_encuestas_<fecha>.csv (segmento, nombre_completo, ci, numero, n_encuestas)
#  - n_encuestas_faltantes_<fecha>.csv (mismos campos para números sin datos de participante)
##############################

# Cargar librerías necesarias
library(readr)
library(dplyr)
library(purrr)

# ==================
# Parámetros de entrada
# ==================

# Ajusta aquí el rango de rondas a procesar (por ejemplo 10:13 procesa R10-R13)
rondas <- 7:10
# Alternativa canónica (mezcla soportada): c("2025/R27", "2025/R28", "R29")
round_refs <- NULL
year_por_defecto <- 2026

# Umbral mínimo de encuestas para habilitar pago.
# Regla vigente: se paga a partir de 3 encuestas respondidas.
minimo_por_defecto <- 3
minimos_por_segmento <- c()

# Directorios
dir_participantes   <- file.path("data", "processed", "participantes")
dir_salida          <- file.path("data", "processed", "n_encuestas")

# Crear carpeta de salida si no existe
if (!dir.exists(dir_salida)) dir.create(dir_salida, recursive = TRUE, showWarnings = FALSE)

# ==================
# Funciones auxiliares
# ==================

# Función para limpiar y estandarizar datos mínimos necesarios
limpiar_datos <- function(df, archivo_origen) {
  # Columnas de interés para el conteo
  cols_interes <- c("segmento", "nombre", "numero")
  
  # Para archivos R0 y R1 que pueden tener estructura diferente
  if (archivo_origen %in% c("R0", "R1")) {
    df_clean <- df %>%
      select(
        numero = any_of(c("numero", "Teléfono de Envío")),
        nombre = any_of(c("nombre", "Contacto Nombre"))
      ) %>%
      mutate(segmento = NA_character_) %>%
      select(all_of(cols_interes))
  } else {
    # Otros archivos con estructura estándar (R2 en adelante)
    df_clean <- df %>% select(any_of(cols_interes))
    for (col in cols_interes) {
      if (!col %in% names(df_clean)) df_clean[[col]] <- NA_character_
    }
    df_clean <- df_clean %>% select(all_of(cols_interes))
  }
  
  # Agregar columna de origen
  df_clean$archivo_origen <- archivo_origen
  
  df_clean
}

# Función para eliminar duplicados por número de teléfono dentro de cada archivo
eliminar_duplicados <- function(df) {
  df %>%
    mutate(numero = as.character(numero)) %>%
    filter(!is.na(numero) & numero != "") %>%
    distinct(numero, .keep_all = TRUE)
}

# ==================
# Recolectar archivos de transcripción según rondas
# ==================

refs_rondas <- tibble(
  year = integer(),
  round_id = character(),
  label = character()
)

if (!is.null(rondas) && length(rondas) > 0) {
  refs_rondas <- bind_rows(
    refs_rondas,
    tibble(
      year = rep(as.integer(year_por_defecto), length(rondas)),
      round_id = paste0("R", as.integer(rondas)),
      label = paste0(as.integer(year_por_defecto), "/", paste0("R", as.integer(rondas)))
    )
  )
}

if (!is.null(round_refs) && length(round_refs) > 0) {
  refs_rondas_extra <- map_dfr(round_refs, function(x) {
    x_chr <- as.character(x)

    if (grepl("^\\d{4}/R\\d+([_][[:alnum:]]+)?$", x_chr, perl = TRUE)) {
      partes <- strsplit(x_chr, "/", fixed = TRUE)[[1]]
      return(tibble(
        year = as.integer(partes[1]),
        round_id = partes[2],
        label = x_chr
      ))
    }

    if (grepl("^R\\d+([_][[:alnum:]]+)?$", x_chr, perl = TRUE)) {
      return(tibble(
        year = as.integer(year_por_defecto),
        round_id = x_chr,
        label = paste0(as.integer(year_por_defecto), "/", x_chr)
      ))
    }

    stop("Formato de round_ref no soportado: ", x_chr)
  })

  refs_rondas <- bind_rows(refs_rondas, refs_rondas_extra)
}

if (nrow(refs_rondas) == 0) {
  stop("Debes definir 'rondas' (legacy) o 'round_refs' (canónico).")
}

refs_rondas <- distinct(refs_rondas, year, round_id, .keep_all = TRUE)

archivos_tbl <- refs_rondas |>
  mutate(
    archivo = file.path(
      "data",
      "processed",
      "transcriptions",
      "output",
      as.character(year),
      paste0("transcripcion_", round_id, ".csv")
    )
  ) |>
  filter(!is.na(archivo) & file.exists(archivo))

if (nrow(archivos_tbl) == 0) {
  stop("No se encontraron archivos de transcripción para las rondas seleccionadas.")
}

total_rondas_periodo <- nrow(archivos_tbl)

cat("Rondas a procesar:", paste(archivos_tbl$label, collapse = ", "), "\n")

# ==================
# Procesamiento de transcripciones
# ==================

datos_procesados <- pmap(
  list(archivos_tbl$archivo, archivos_tbl$round_id, archivos_tbl$label),
  function(archivo, nombre, label) {
  cat("Procesando", archivo, "...\n")
  df <- read_csv(archivo, locale = locale(encoding = "UTF-8"))
  df_limpio <- limpiar_datos(df, nombre)
  df_sin_duplicados <- eliminar_duplicados(df_limpio)
  df_sin_duplicados$ronda_resuelta <- label
  cat("  - Filas originales:", nrow(df), "\n")
  cat("  - Filas después de limpiar:", nrow(df_limpio), "\n")
  cat("  - Filas después de eliminar duplicados:", nrow(df_sin_duplicados), "\n\n")
  df_sin_duplicados
})

base_unificada <- bind_rows(datos_procesados)
cat("Total de registros combinados:", nrow(base_unificada), "\n")

# Conteo por número
conteos <- base_unificada %>%
  group_by(numero) %>%
  summarise(
    segmento = first(na.omit(segmento)),
    n_encuestas = n(),
    .groups = "drop"
  ) %>%
  mutate(
    segmento = ifelse(is.na(segmento) | segmento == "", NA_character_, segmento)
  )

cat("Contactos únicos en conteos:", nrow(conteos), "\n")

# ==================
# Preparar mínimos por segmento
# ==================

segmentos_presentes <- sort(unique(na.omit(conteos$segmento)))
minima_usada <- setNames(rep(minimo_por_defecto, length(segmentos_presentes)), segmentos_presentes)
if (length(minimos_por_segmento)) {
  comunes <- intersect(names(minimos_por_segmento), names(minima_usada))
  minima_usada[comunes] <- minimos_por_segmento[comunes]
}
minimos_tbl <- tibble(segmento = names(minima_usada), minimo_segmento = as.integer(minima_usada))

# ==================
# Cargar el archivo más reciente de participantes
# ==================

archivos_part <- list.files(dir_participantes, pattern = "\\.csv$", full.names = TRUE)
if (length(archivos_part) == 0) {
  stop("No se encontraron archivos en data/processed/participantes")
}
info_part <- file.info(archivos_part)
participantes_path <- rownames(info_part)[which.max(info_part$mtime)]
cat("Usando participantes:", participantes_path, "\n")

participantes <- read_csv(participantes_path, locale = locale(encoding = "UTF-8")) %>%
  mutate(
    numero = as.character(numero),
    ci = as.character(ci)
  ) %>%
  select(any_of(c("nombre_completo", "ci", "numero")))

# ==================
# Unir y exportar resultados
# ==================

# Archivo principal: todos los participantes (con n_encuestas, 0 si no aparece)
resultado <- participantes %>%
  left_join(conteos, by = "numero") %>%
  mutate(n_encuestas = ifelse(is.na(n_encuestas), 0L, n_encuestas)) %>%
  # Excluir participantes que no contestaron ninguna
  filter(n_encuestas > 0) %>%
  # Unir mínimos por segmento y aplicar filtro de pago
  left_join(minimos_tbl, by = "segmento") %>%
  mutate(minimo_segmento = ifelse(is.na(minimo_segmento), minimo_por_defecto, minimo_segmento)) %>%
  filter(n_encuestas >= minimo_segmento) %>%
  select(segmento, nombre_completo, ci, numero, n_encuestas) %>%
  arrange(desc(is.na(ci) | ci == ""), segmento, desc(n_encuestas), nombre_completo)

# Archivo faltantes: números en conteos que no están en participantes
faltantes <- conteos %>%
  anti_join(participantes %>% select(numero), by = "numero") %>%
  # Unir mínimos por segmento para priorizar a quienes podrían cobrar
  left_join(minimos_tbl, by = "segmento") %>%
  mutate(minimo_segmento = ifelse(is.na(minimo_segmento), minimo_por_defecto, minimo_segmento)) %>%
  filter(n_encuestas >= minimo_segmento) %>%
  transmute(
    segmento = segmento,
    nombre_completo = NA_character_,
    ci = NA_character_,
    numero = numero,
    n_encuestas = n_encuestas
  ) %>%
  arrange(desc(is.na(ci) | ci == ""), segmento, desc(n_encuestas), nombre_completo)

fecha_tag <- format(Sys.Date(), "%Y%m%d")
out_ok <- file.path(dir_salida, sprintf("n_encuestas_%s.csv", fecha_tag))
out_missing <- file.path(dir_salida, sprintf("n_encuestas_faltantes_%s.csv", fecha_tag))

resultado <- resultado |> 
  mutate(
    monto = case_when(
      n_encuestas == total_rondas_periodo ~ 350,
      n_encuestas == 3 ~ 210,
      n_encuestas >= 3 ~ 290,
    ),
    monto_comision = case_when(
      n_encuestas == total_rondas_periodo ~ 350 + 60,
      n_encuestas == 3 ~ 210 + 60,
      n_encuestas >= 3 ~ 290 + 60,
    )
  )

faltantes <- faltantes |> 
  mutate(
    monto = case_when(
      n_encuestas == total_rondas_periodo ~ 350,
      n_encuestas == 3 ~ 210,
      n_encuestas >= 3 ~ 290,
    ),    
    monto_comision = case_when(
      n_encuestas == total_rondas_periodo ~ 350 + 60,
      n_encuestas == 3 ~ 210 + 60,
      n_encuestas >= 3 ~ 290 + 60,
    )
  )

write_csv(resultado, out_ok, na = "")
write_csv(faltantes, out_missing, na = "")

cat("\nArchivos generados:\n")
cat("- ", out_ok, " (", nrow(resultado), " filas)\n", sep = "")
cat("- ", out_missing, " (", nrow(faltantes), " filas)\n", sep = "")
monto_total_con_comision <- sum(resultado$monto_comision, na.rm = TRUE) + sum(faltantes$monto_comision, na.rm = TRUE)
cat("- Monto total con comisión: $", format(monto_total_con_comision, big.mark = ","), "\n", sep = "")
# Exportar a xlsx
library(writexl)

write_xlsx(resultado, sub("\\.csv$", ".xlsx", out_ok))
write_xlsx(faltantes, sub("\\.csv$", ".xlsx", out_missing))

cat("- ", sub("\\.csv$", ".xlsx", out_ok), " (", nrow(resultado), " filas)\n", sep = "")
cat("- ", sub("\\.csv$", ".xlsx", out_missing), " (", nrow(faltantes), " filas)\n", sep = "")
