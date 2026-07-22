library(readr)
library(dplyr)
library(stringr)

# función para agregar el prefijo 598
PREFIX_FACTOR <- 598 * 10^8
add_uy_prefix <- function(nums) {
  if_else(nums >= PREFIX_FACTOR, nums, nums + PREFIX_FACTOR)
}

# función para normalizar los números de teléfono
normalizar_num <- function(df) {
  df %>% 
    mutate(
      numero = str_remove_all(numero, "\\D+") |> as.numeric(),
      numero = add_uy_prefix(numero)
    )
}
# cargar numeros y normalizar
numeros <- normalizar_num(read_csv('/Users/simonherrera/Downloads/ACTIVO CANELONES CONOCIDOS (Respuestas) - Respuestas de formulario 1.csv'))

# eliminar duplicados
numeros <- numeros |> 
  distinct(numero, .keep_all = TRUE)

# limpieza
numeros <- numeros |> 
  mutate(edad = as.integer(edad))

# cargo contactos de junio
junio <- read_csv(file.path("data", "raw", "contacts", "20250922.csv"))

# me quedo con los contactos de junio que no están en julio
numeros_junio <- junio %>%
  filter(!numero %in% numeros$numero)

segmento <- junio |> select(segmento, numero)

# agrego segmento a los números de julio
numeros <- numeros %>%
  left_join(segmento, by = "numero")

# codifico edad en tres grupos (30 y menos, 31 a 59 y 60 y más)
numeros <- numeros %>%
  mutate(
    edad = case_when(
      edad <= 30 ~ "30 y menos",
      edad <= 59 ~ "31 a 59",
      TRUE ~ "60 y más"
    )
  )

# convierto el número a character
numeros <- numeros |> mutate(numero = as.character(numero))

# corrijo segmentos
numeros <- numeros |> 
  mutate(segmento = case_when(
    departamento == "Canelones" ~ "Canelones",
    departamento == "Montevideo" ~ "Montevideo",
    
    # corrijo segmento segun voto, si no es montevideo ni canelones
    departamento != "Canelones" & departamento != "Montevideo" & voto == "Frente Amplio" ~ "Interior Frente Amplio",
    departamento != "Canelones" & departamento != "Montevideo" & voto != "Frente Amplio" ~ "Interior Coalición",
    TRUE ~ segmento
  ))

# creo la variable voto2
numeros <- numeros |> mutate(voto2 = ifelse(voto == "Frente Amplio", "FA", "CM"))

# etiq <- read_csv("/Users/simonherrera/Downloads/base_analisisjul - base_acumulada.csv") |> 
#   select(numero, ETIQ) |> 
#   rename(etiqueta = ETIQ) |> 
#   mutate(numero = as.character(numero))

# etiq <- etiq |>
#   mutate(
#     etiqueta = case_when(
#       etiqueta %in% c("oficial_claro", "oficialista_claro", "oficialista_acerrimo") ~ "oficialista_acerrimo",
#       etiqueta %in% c("oficial_abierto", "oficialista_abierto") ~ "oficialista_abierto",
#       etiqueta %in% c("difuso", "dialoguista") ~ "dialoguista",
#       etiqueta %in% c("oposicion_clara", "oposicion_cerril") ~ "oposicion_cerril",
#       etiqueta ==  "oposicion_abierta" ~ "oposicion_abierta",
#       etiqueta %in% c("desinf", "desinformadas") ~ "desinformadas",
#       etiqueta %in% c("desc", "descreidas", "descreidas/alejadas") ~ "descreidas/alejadas",
#       TRUE ~ etiqueta
#     )
#   )

# numeros <- numeros |> 
#   left_join(etiq, by = "numero")

# guardo los números
dir.create(file.path("data", "processed", "contacts"), recursive = TRUE, showWarnings = FALSE)
write_csv(numeros, file.path("data", "processed", "contacts", "072025.csv"))
