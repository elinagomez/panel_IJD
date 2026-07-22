
# Cargar librerias 

library(dplyr)
library(readr)

# Cargar base 
year <- 2025
round_id <- "R21"
path <- file.path("data", "processed", "transcriptions", "output", as.character(year), paste0("transcripcion_", round_id, ".csv"))
data <- read_csv(path, col_types = cols(.default = "c"))

# Pre procesamiento 
data <- data |> 
  # Definir preguntas
  rename(
  INSE_edu = "q1",
  INSE_perceptores = "q2",
  INSE_aire_acon = "q3",
  INSE_salud = "q4" 
) |>
  # Recodificacion
  mutate(

    INSE_edu = case_when(
      INSE_edu == "Sí" ~ 8L, 
      INSE_edu == "No" ~ 0L,
      TRUE ~ NA_integer_),
    
    INSE_perceptores = case_when(
      INSE_perceptores == "1"       ~ 0L,
      INSE_perceptores == "2"       ~ 8L,
      INSE_perceptores == "3"       ~ 11L,
      INSE_perceptores == "4 o más" ~ 15L,
      TRUE ~ NA_integer_
    ),

    INSE_aire_acon = case_when(
      INSE_aire_acon == "Ninguno" ~ 0L,
      INSE_aire_acon == "1"       ~ 4L,
      INSE_aire_acon == "2"       ~ 7L,
      INSE_aire_acon == "3 o más" ~ 9L,
      TRUE ~ NA_integer_
    ),

    INSE_salud = case_when(
      INSE_salud == "Sí" ~ 0L, 
      INSE_salud == "No" ~ 4L,
      TRUE ~ NA_integer_)
  ) 


# Base de datos INSE
data_INSE <- data |> 
  mutate(
    INSE_num = as.integer(rowSums(across(starts_with("INSE")), na.rm = TRUE)),
    INSE_tres = case_when(
      between(INSE_num, 0, 12)  ~ "Bajo",
      between(INSE_num, 13, 24) ~ "Medio",
      between(INSE_num, 25, 36) ~ "Alto"
    ),
    INSE_cinco = case_when(
      between(INSE_num, 0, 8)   ~ "Bajo",
      between(INSE_num, 9, 14)  ~ "Medio-Bajo",
      between(INSE_num, 15, 22) ~ "Medio",
      between(INSE_num, 23, 29) ~ "Medio-Alto",
      between(INSE_num, 30, 36) ~ "Alto"
    )
  ) |>  
  select(numero, INSE_num, INSE_tres, INSE_cinco)


# write_csv(
#   data_INSE,
#   paste0("", round_id, ".csv")
# )

      
