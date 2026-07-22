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

dict_q5 <- dictionary(list(
  presencia_policial = c(
    "polic*", "patrull*", "calle*", "barrio*", "presencia", "efectiv*", "salir",
    "sal*", "recorr*"
  ),
  oportunidades_laborales = c(
    "trabaj*", "lab*", "empleo*", "econom*", "esfuerz*", "merit*", "ganar*", "salario*"
  ),
  educacion_capacitacion = c(
    "educ*", "capacit*", "formac*", "form*", "curso*", "taller*", "prepar*", "cultura*",
    "deport*"
  ),
  vigilancia_tecnologia = c(
    "camara*", "vigil*", "tecnolog*", "monitor*", "sistema*"
  ),
  justicia_penas = c(
    "ley*", "pena*", "castig*", "delito*", "delincuen*", "cumpl*", "sever*", "dur*",
    "endur*", "prisi*", "conden*", "justici*"
  ),
  social_comunidad = c(
    "social*", "sociedad*", "comunidad*", "vecin*", "particip*", "colabor*", "apoyo*",
    "ayud*", "solidaridad*", "integracion*", "inclusion*", "dign*", "vulnerab*"
  ),
  narco_droga = c(
    "narco*", "trafic*", "drog*", "combat*", "lucha*", "narcomenud*", "narcotrafic*"
  ),
  reinsercion_carceles = c(
    "carcel*", "reinserc*", "reinsert*", "libertad*", "rehabilit*", "preso*",
    "recluso*", "dignidad*", "liber*"
  ),
  igualdad = c(
    "igualdad*", "equidad*", "condicion*", "favor*", "merit*", "derecho*", "equi*"
  ),
  prevencion_juventud = c(
    "joven*", "prevencion*", "niñ*", "adolescen*", "juventud*"
  )
))


encuesta <- readxl::read_excel(paste0("data/processed/analysis/", year, "/", round_id, "/r11_q4.xlsx")) |>
  mutate(q5_norm = norm_txt(q5))

toks <- tokens(encuesta$q5_norm, remove_punct = TRUE, remove_numbers = TRUE)

mat <- dfm(toks) |>
  dfm_lookup(dict_q5, valuetype = "glob", case_insensitive = TRUE)

cats <- names(dict_q5)

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

prioridad <- names(dict_q5)

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
  out[[paste0("q5_", i)]] <- vapply(
    ord_list,
    function(x) if (length(x) >= i) x[[i]] else NA_character_,
    character(1)
  )
}

# Eliminar las columnas de quanteda y la columna normalizada
out <- out |>
  relocate(starts_with("q5_"), .after = q5) |> 
  select(-q5_norm, -all_of(cats))

resumen <- out |>
  # Recalcular el resumen usando solo las columnas finales
  select(starts_with("q5_")) |>
  pivot_longer(everything(), names_to = "posicion", values_to = "categoria") |>
  filter(!is.na(categoria)) |>
  count(categoria, name = "n") |>
  arrange(desc(n))

print(resumen)

openxlsx::write.xlsx(out, paste0("data/processed/analysis/", year, "/", round_id, "/r11_q5.xlsx"))
