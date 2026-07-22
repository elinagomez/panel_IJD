library(dplyr)
library(readr)
library(purrr)
library(stringr)

# funcion auxiliar para generar nombres vectorizados con formato r<round>_q<numero>
limpiar_nombre_columna <- function(colnames, round_num) {
  # extraer identificador 'q#' de cada nombre
  q <- str_extract(colnames, "q\\d+")
  # si alguna extracción falla, detener con mensaje
  if (any(is.na(q))) {
    malos <- colnames[is.na(q)]
    stop(paste("no se pudo extraer número de pregunta de:", paste(malos, collapse = ", ")))
  }
  # construir nombres nuevos
  paste0("r", round_num, "_", q)
}

# procesa un unico round y renombra columnas q1, q2, ... a r<round>_q#
procesar_round <- function(round_num, ruta_base = "data/raw/acumulada/rounds") {
  archivo <- paste0(ruta_base, "/r", round_num, ".csv")
  if (!file.exists(archivo)) {
    warning("no existe ", archivo, " → se salta r", round_num)
    return(NULL)
  }
  datos <- read_csv(archivo, show_col_types = FALSE)
  
  # detectar solo columnas q1, q2, etc.
  cols_resp <- names(datos)[str_detect(names(datos), "^q\\d+$")]
  
  # renombrar respuestas con prefijo del round
  datos %>%
    rename_with(~ limpiar_nombre_columna(.x, round_num), all_of(cols_resp))
}

# crea la base acumulada a partir de varios rounds
crear_base_acumulada <- function(rounds = 1:11,
                                 ruta_base     = "data/raw/acumulada/rounds",
                                 archivo_salida = "data/processed/acumulada/base_acumulada.csv") {
  message("iniciando creación de base acumulada...")
  
  # procesar cada round y eliminar nulls
  procesados <- map(rounds, ~ procesar_round(.x, ruta_base))
  names(procesados) <- paste0("r", rounds)
  procesados <- compact(procesados)
  
  if (length(procesados) == 0) {
    stop("no se encontraron archivos de rounds válidos.")
  }
  
  # columnas de datos de participantes
  cols_part <- c("segmento","nombre","edad","genero","numero",
                 "departamento","voto", "voto2", "etiqueta")
  
  # extraer participantes únicos
  base_part <- rev(procesados) %>%
    map(~ select(.x, any_of(cols_part))) %>%
    bind_rows() %>%
    distinct(numero, .keep_all = TRUE)
  
  # extraer y unir respuestas de cada round
  respuestas <- imap(procesados, ~ {
    df <- .x
    r   <- str_remove(.y, "^r")
    cols_q <- names(df)[str_detect(names(df), paste0("^r", r, "_q\\d+$"))]
    select(df, numero, all_of(cols_q))
  }) %>%
    reduce(full_join, by = "numero")
  
  # combinar participantes con respuestas
  base_acumulada <- left_join(base_part, respuestas, by = "numero")
  
  # crear carpeta de salida y guardar csv
  dir.create(dirname(archivo_salida), recursive = TRUE, showWarnings = FALSE)
  write_csv(base_acumulada, archivo_salida)
  
  message("base acumulada creada en: ", archivo_salida)
  message("rounds procesados: ", length(procesados))
  message("participantes: ", nrow(base_acumulada))
  message("variables totales: ", ncol(base_acumulada))
  
  invisible(base_acumulada)
}

# agrega un nuevo round a la base existente
agregar_nuevo_round <- function(nuevo_round,
                               archivo_base = "data/processed/acumulada/base_acumulada.csv",
                               ruta_base    = "data/raw/acumulada/rounds") {
  message("agregando r", nuevo_round, " a la base existente...")
  
  if (!file.exists(archivo_base)) {
    stop("no se encontró la base existente en: ", archivo_base)
  }
  base_existente <- read_csv(archivo_base, show_col_types = FALSE)
  
  datos_nuevo <- procesar_round(nuevo_round, ruta_base)
  if (is.null(datos_nuevo)) {
    stop("no se pudo procesar r", nuevo_round)
  }
  
  # extraer respuestas del nuevo round
  cols_q <- names(datos_nuevo)[str_detect(names(datos_nuevo), paste0("^r", nuevo_round, "_q\\d+$"))]
  resp_nuevo <- select(datos_nuevo, numero, all_of(cols_q))
  
  # combinar con base existente
  base_actualizada <- full_join(base_existente, resp_nuevo, by = "numero")
  
  write_csv(base_actualizada, archivo_base)
  message("round r", nuevo_round, " agregado correctamente.")
  message("total variables ahora: ", ncol(base_actualizada))
  
  invisible(base_actualizada)
}

# verifica la estructura de la base acumulada
verificar_estructura <- function(archivo_base = "data/processed/acumulada/base_acumulada.csv") {
  if (!file.exists(archivo_base)) {
    message("no existe la base acumulada.")
    return(NULL)
  }
  datos <- read_csv(archivo_base, show_col_types = FALSE)
  cat("=== estructura de la base acumulada ===\n")
  cat("dimensiones:", nrow(datos), "filas x", ncol(datos), "columnas\n\n")
  
  nombres <- names(datos)
  cols_part <- c("segmento","nombre","edad","genero","numero",
                 "departamento","voto", "voto2", "etiqueta")
  cat("columnas de participantes (", length(cols_part), "):\n",
      paste(cols_part, collapse = ", "), "\n\n")
  
  rounds_detectados <- unique(str_extract(nombres, "^r\\d+")) %>% na.omit() %>%
    map_chr(~ str_remove(., "r")) %>%
    as.numeric() %>%
    sort() %>%
    paste0("r", .)
  
  for (r in rounds_detectados) {
    qs <- grep(paste0("^", r, "_q\\d+$"), nombres, value = TRUE)
    cat(r, "→", length(qs), "preguntas:", paste(qs, collapse = ", "), "\n")
  }
  invisible(datos)
}

# creo base acumulada
base <- crear_base_acumulada(1:11,
                             ruta_base = "data/raw/acumulada/rounds",
                             archivo_salida = "data/processed/acumulada/base_acumulada.csv")

base <- base %>%
  distinct(numero, .keep_all = TRUE)

base <- base %>%
  mutate(edad = case_when(
    str_detect(edad, "17 - 24|17-24|25-30") ~ "30 y menos",
    str_detect(edad, "31 - 59|31 a 59|31-59") ~ "31 a 59",
    str_detect(edad, "60 y más|60\\+|más 60") ~ "60 y más",
    TRUE ~ edad
  )) %>% 
  mutate(genero = case_when(
    genero == "Varón" ~ "Hombre",
    TRUE ~ genero
  )) %>%
  write_csv("data/processed/acumulada/base_acumulada.csv")

verificar_estructura("data/processed/acumulada/base_acumulada.csv")

base2 <- agregar_nuevo_round(12,
                             archivo_base = "data/processed/acumulada/base_acumulada.csv",
                             ruta_base = "data/raw/acumulada/rounds")
