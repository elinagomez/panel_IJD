library(readr)
library(dplyr)
library(stringr)

# Normalización de números (agrega prefijo +598 si falta)
PREFIX_FACTOR <- 598 * 10^8
add_uy_prefix <- function(nums) {
  if_else(nums >= PREFIX_FACTOR, nums, nums + PREFIX_FACTOR)
}

normalizar_num <- function(df) {
  df %>%
    mutate(
      numero = str_remove_all(as.character(numero), "\\D+") |> as.numeric(),
      numero = add_uy_prefix(numero),
      numero = as.character(numero)
    )
}

# Carga
viejo <- read_csv("data/raw/contacts/20250922.csv", show_col_types = FALSE) |> normalizar_num()
nuevo <- read_csv("data/raw/contacts/20251004_solo_nuevos.csv", show_col_types = FALSE) |> normalizar_num()

# Alinear columnas opcionales
if (!"etiqueta" %in% names(nuevo)) {
  nuevo <- nuevo %>% mutate(etiqueta = NA_character_)
}

# Recalcular voto2 para consistencia (FA vs CM)
recalc_voto2 <- function(df) {
  if ("voto" %in% names(df)) {
    df %>% mutate(voto2 = if_else(voto == "Frente Amplio", "FA", "CM"))
  } else df
}

viejo <- recalc_voto2(viejo)
nuevo <- recalc_voto2(nuevo)

# Unir (prioriza 'nuevo' ante duplicados) y deduplicar por numero
contactos <- bind_rows(nuevo, viejo) %>%
  distinct(numero, .keep_all = TRUE)

# Guardar como nuevo corte
write_csv(contactos, "data/raw/contacts/20251004.csv")

