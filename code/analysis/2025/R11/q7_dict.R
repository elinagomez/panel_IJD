library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(quanteda)
library(stringi)
year <- 2025
round_id <- "R11"


norm_txt <- function(x) {
  x <- ifelse(is.na(x), "", x)
  x |>
    stringi::stri_trans_general("Latin-ASCII") |>
    tolower() |>
    stringr::str_squish()
}

dict_q7 <- dictionary(list(
  infraestructura_condiciones = c(
    "infraestruct*", "edilici*", "edific*", "condicion*", "mantenim*",
    "locales", "aulas", "baños", "baño", "salon*", phrase("más escuelas"),
    "escuela*", "liceo*", "centro*"
  ),
  docentes_rrhh = c(
    "salari*", "sueldo*", "formación", "formacion", "capacit*",
    "actualizaci*", "actualizar", phrase("brindar una buena educación"),
    "docent*", "maestr*", "pedagóg*", "psico*", "equipo*"
  ),
  utiles_tecnologia = c(
    "material*", "útiles", "utiles", "libro*", "mochila", "túnica", "tunica", "calzado",
    "tecnolog*", "computador*", "internet", "conectivid*", "digital", "robot*"
  ),
  alimentecion_comedores = c(
    "aliment*", "comedor*", "merender*", "comida", "desayuno", "merienda"
  ),
  jornada_extendida = c(
    phrase("tiempo completo"), phrase("doble horario"), phrase("horario extendido"),
    phrase("maestras comunitarias")
  ),
  acceso_equidad = c(
    "beca*", phrase("apoyo económico"), phrase("ayuda económica"),
    "acceso", "interior", "transporte", "lugares", "cerc*", "barrios", "contexto",
    "oportunidad*", "vulnera*", "infan*", "inicial", "necesitados"
  ),
  oferta_terciaria = c(
    "utu", "técnic*", "tecnic*", "terciari*", "universidad*", "universidades", "voca*",
    "migr*"
  ),
  asistencia_permanencia = c(
    "falta*", "asisten", "asistencia", "control*", "volver", "clase", "clases", "repet*",
    "deser*"
  )
))

encuesta <- readxl::read_excel(paste0("data/processed/analysis/", year, "/", round_id, "/r11_q6.xlsx")) |>
  mutate(q7_norm = norm_txt(q7))

toks <- tokens(encuesta$q7_norm, remove_punct = TRUE, remove_numbers = TRUE)

mat <- dfm(toks) |>
  dfm_lookup(dict_q7, valuetype = "glob", case_insensitive = TRUE)

cats <- names(dict_q7)

tags <- convert(mat, to = "data.frame") |>
  tibble::as_tibble() |>
  select(any_of(cats))

out <- encuesta |>
  bind_cols(tags) |>
  rowwise() |>
  mutate(
    etiquetas = {
      v <- c_across(all_of(cats))
      paste(cats[!is.na(v) & v > 0], collapse = "; ")
    }
  ) |>
  ungroup()

prioridad <- names(dict_q7)

out <- out |>
  rowwise() |>
  mutate(
    etiqueta_principal = {
      v <- c_across(all_of(prioridad))
      idx <- which(!is.na(v) & v > 0)
      if (length(idx)) prioridad[min(idx)] else NA_character_
    }
  ) |>
  ungroup()

cats_rank <- prioridad
rank <- setNames(seq_along(cats_rank), cats_rank)

count_mat <- out |>
  select(all_of(cats_rank)) |>
  as.data.frame()

ord_list <- apply(count_mat, 1, function(v) {
  pres <- names(v)[!is.na(v) & v > 0]
  if (length(pres) == 0) return(character(0))
  v_pres <- as.numeric(v[pres])
  o <- order(-v_pres, rank[pres], pres)
  pres[o]
})

k <- 5

for (i in seq_len(k)) {
  out[[paste0("q7_", i)]] <- vapply(
    ord_list,
    function(x) if (length(x) >= i) x[[i]] else NA_character_,
    character(1)
  )
}

# Eliminar las columnas de quanteda y la columna normalizada
out <- out |>
  relocate(starts_with("q7_"), .after = q7) |> 
  select(-q7_norm, -all_of(cats), c(etiquetas, etiqueta_principal))

resumen <- out |>
  # Recalcular el resumen usando solo las columnas finales
  select(starts_with("q7_")) |>
  pivot_longer(everything(), names_to = "posicion", values_to = "categoria") |>
  filter(!is.na(categoria)) |>
  count(categoria, name = "n") |>
  arrange(desc(n))

print(resumen)

openxlsx::write.xlsx(out, paste0("data/processed/analysis/", year, "/", round_id, "/r11_q7.xlsx"))
