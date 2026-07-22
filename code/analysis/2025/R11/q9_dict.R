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

dict_q9 <- dictionary(list(
  "despliegue_patrullaje" = c(
    "policia*", "policial*", "presencia", "patrull*",
    "calle*", "barri*", "zona*", "interior", "permanente"
  ),
  "tecnologia_monitoreo" = c(
    "camara*", "vigilancia", "inteligenc*", "movil*",
    "monitoreo", "tecnolog*", "drone*"
  ),
  "recursos_humanos" = c(
    "efectiv*", "agente*", "funcionari*", "condicion*",
    "sueldo*", "salari*", "capacit*", "formacion",
    "formad*", "instruccion"
  ),
  "sistema_carcelario_rehabilitacion" = c(
    "carcel*", "penitenciari*", "carcelari*",
    "centro*", "rehabilit*", "reinserc*"
  ),
  "justicia_penas" = c(
    "pena*", "conden*", "justicia", "juec*"
  ),
  "prevencion_social_salud_mental" = c(
    "prevencion", "educacion", "joven*", "trabajo",
    "oportunidad*", "curso*", "oficio*", "programa*",
    "comunitar*", "familia*",
    "salud", "mental", phrase("salud mental"),
    "droga*", "consumo"
  ),
  "iluminacion_espacio_publico" = c(
    "iluminacion", "espacio*"
  ),
  "narcotrafico" = c(
    "narco*"
  )
))


encuesta <- readxl::read_excel(paste0("data/processed/analysis/", year, "/", round_id, "/r11_q8.xlsx")) |>
  mutate(q9_norm = norm_txt(q9))

toks <- tokens(encuesta$q9_norm, remove_punct = TRUE, remove_numbers = TRUE)

mat <- dfm(toks) |>
  dfm_lookup(dict_q9, valuetype = "glob", case_insensitive = TRUE)

cats <- names(dict_q9)

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

prioridad <- names(dict_q9)

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
  out[[paste0("q9_", i)]] <- vapply(
    ord_list,
    function(x) if (length(x) >= i) x[[i]] else NA_character_,
    character(1)
  )
}

# Eliminar las columnas de quanteda y la columna normalizada
out <- out |>
  relocate(starts_with("q9_"), .after = q9) |> 
  select(-q9_norm, -all_of(cats), c(etiquetas, etiqueta_principal))

resumen <- out |>
  # Recalcular el resumen usando solo las columnas finales
  select(starts_with("q9_")) |>
  pivot_longer(everything(), names_to = "posicion", values_to = "categoria") |>
  filter(!is.na(categoria)) |>
  count(categoria, name = "n") |>
  arrange(desc(n))

print(resumen)

openxlsx::write.xlsx(out, paste0("data/processed/analysis/", year, "/", round_id, "/R11_etiquetada.xlsx"))
