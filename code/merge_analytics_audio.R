library(tidyverse)


# ------------------------------------------------------------------------------
# CONFIGURACIÓN
# ------------------------------------------------------------------------------

campaign_api <- "r14_20260526_api"
campaign_wise <- "r14_20260526_analytics"
campaign_output <- "r14_20260526"
audio_txt <- "Ha enviado un Audio"

# ------------------------------------------------------------------------------
# CARGA DE DATOS
# ------------------------------------------------------------------------------

# Base original de la API (Puede tener columnas corriidas)
apisurvey <- read_csv(paste0("data/raw/campaigns_wcx/", campaign_api, ".csv")) 

# Base 'verdad' con estructura de preguntas correcta
wisesurvey <- read_csv(paste0("data/processed/matched/", campaign_wise, ".csv")) 


# ------------------------------------------------------------------------------
# DEFINICION DE COLUMNAS PREGUNTA
# ------------------------------------------------------------------------------

q_cols_api  <- names(apisurvey)[str_detect(names(apisurvey), "^q\\d+$")]
q_cols_wise <- names(wisesurvey)[str_detect(names(wisesurvey), "^q\\d+$")]

# Crea un indice de preguntas
q_index <- tibble(
  pregunta = union(q_cols_wise, q_cols_api)
) |>
  mutate(pos = readr::parse_number(pregunta)) |>
  arrange(pos)

# ------------------------------------------------------------------------------
# IDENTIFICACION DE AUDIOS
# ------------------------------------------------------------------------------


# audios de la base wise
audios_wise <- wisesurvey |>
  select(numero, all_of(q_cols_wise)) |>
  pivot_longer(
    cols = all_of(q_cols_wise),
    names_to = "pregunta_wise",
    values_to = "valor_wise",
    values_transform = list(valor_wise = as.character)
  ) |>
  filter(valor_wise == audio_txt) |>
  left_join(q_index, by = c("pregunta_wise" = "pregunta")) |>
  rename(pos_wise = pos) |>
  arrange(numero, pos_wise)


# links de la base de api
ogg_api <- apisurvey |>
  semi_join(audios_wise |> distinct(numero), by = "numero") |>
  select(numero, any_of(q_cols_api)) |>
  pivot_longer(
    cols = -numero,
    names_to = "pregunta_api",
    values_to = "link_ogg",
    values_transform = list(link_ogg = as.character)
  ) |>
  filter(
    !is.na(link_ogg),
    str_detect(link_ogg, "\\.ogg")
  ) |>
  left_join(q_index, by = c("pregunta_api" = "pregunta")) |>
  rename(pos_api = pos) |>
  arrange(numero, pos_api)

# ------------------------------------------------------------------------------
# FUNCIONES
# ------------------------------------------------------------------------------

# FUNCION DE ASIGNACION:
# Para cada audio esperado en WISE:
# - busca el .ogg más cercano hacia la derecha (pos_api >= pos_wise)
# - si no hay, usa el primero disponible (fallback)
# - nunca reutiliza el mismo .ogg

asignar_link <- function(pos_audio, oggs) {
  usados <- rep(FALSE, nrow(oggs))

  map_chr(pos_audio, function(p) {
    i <- which(!usados & oggs$pos_api >= p)[1]

    # fallback: si no hay hacia la derecha, toma el primer .ogg libre
    if (is.na(i)) {
      i <- which(!usados)[1]
    }

    if (is.na(i)) {
      return(NA_character_)
    }

    usados[i] <<- TRUE
    oggs$link_ogg[i]
  })
}


# FUNCION QUE CORRIGE COLUMNAS UNDEFINED. 
# Detecta columnas de preguntas (qX) que en WISE están completamente "undefined"
# (o NA), lo que indica un error en la configuración del bot.
#
# Para cada una de esas columnas:
# - muestra una muestra aleatoria de valores desde API (para inspección manual)
# - pregunta interactivamente si se desea reemplazarla
# - si se acepta, reemplaza la columna en `resultado` con valores de API
#   (filtrando links .ogg para evitar contaminar con audios sin procesar)


check_undefined <- function(resultado, wisesurvey, apisurvey, id_col = "numero", sample = 20) {
  
  q_cols_wise <- names(wisesurvey)[str_detect(names(wisesurvey), "^q\\d+$")]
  
  cols_undefined <- wisesurvey |>
    summarise(across(
      all_of(q_cols_wise),
      ~ all(is.na(.x) | .x == "undefined")
    )) |>
    pivot_longer(
      everything(),
      names_to = "pregunta",
      values_to = "all_undefined"
    ) |>
    filter(all_undefined) |>
    pull(pregunta)
  
  if (length(cols_undefined) == 0) {
    message("No hay columnas completamente undefined.")
    return(resultado)
  }
  
  resultado_out <- resultado
  
  for (col in cols_undefined) {
    
    cat("\nColumna detectada como undefined:", col, "\n")
    
    # Mostrar preview de API
    preview <- apisurvey |>
      select(all_of(id_col), all_of(col)) |>
      filter(!is.na(.data[[col]])) |> 
      slice_sample(n = sample)
    
    print(preview)
    
    resp <- readline(prompt = paste0("¿Reemplazar '", col, "' desde API? (y/n): "))
    
    if (tolower(resp) == "y") {
      
      api_vals <- apisurvey |>
        select(all_of(id_col), all_of(col)) |>
        mutate(
          !!col := if_else(
            str_detect(.data[[col]], "\\.ogg"),
            NA_character_,
            as.character(.data[[col]])
          )
        )
      
      resultado_out <- resultado_out |>
        select(-any_of(col)) |>
        left_join(api_vals, by = id_col)
      
      message("Columna ", col, " reemplazada.")
      
    } else {
      message("Columna ", col, " mantenida.")
    }
  }
  
  # Restaurar orden original
  resultado_out |>
    select(any_of(names(resultado)), everything()) 
}

# ------------------------------------------------------------------------------
# EMPAREJAR posición real en WISE + .ogg por orden en API
# ------------------------------------------------------------------------------

reemplazos <- audios_wise |>
  group_by(numero) |>
  group_modify(\(.x, .y) {
    oggs <- ogg_api |>
      filter(numero == .y$numero) |>
      arrange(pos_api)

    .x |>
      arrange(pos_wise) |>
      mutate(link_ogg = asignar_link(pos_wise, oggs))
  }) |>
  ungroup()

# ------------------------------------------------------------------------------
# RESULTADO Reemplazar en WISE usando numero + pregunta_wise
# ------------------------------------------------------------------------------

resultado <- wisesurvey |>
  pivot_longer(
    cols = all_of(q_cols_wise),
    names_to = "pregunta_wise",
    values_to = "valor",
    values_transform = list(valor = as.character)
  ) |>
  left_join(
    reemplazos |> select(numero, pregunta_wise, link_ogg),
    by = c("numero", "pregunta_wise")
  ) |>
  mutate(
    valor = if_else(valor == audio_txt & !is.na(link_ogg), link_ogg, valor)
  ) |>
  select(-link_ogg) |>
  pivot_wider(
    names_from = pregunta_wise,
    values_from = valor
  ) |> 
  check_undefined(wisesurvey, apisurvey)


# ------------------------------------------------------------------------------
# GUARDAR
# ------------------------------------------------------------------------------


csv_output_path <- paste0("data/processed/matched/", campaign_output, ".csv")

readr::write_csv(
    resultado,
    csv_output_path
  )

