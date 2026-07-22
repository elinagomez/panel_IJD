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

dict_q8 <- dictionary(list(
  "capacitacion_formacion" = c(
    "capacit*", "cursos", "formación", "oficios", "programas", "pasantías"
  ),
  "generacion_de_empleo" = c(
    "empleo*", "trabajo", "trabajos", "puestos", "plazas", "fuente*", "contrata*"
  ),
  "pymes_emprendimientos" = c(
    "pymes", "pyme*", "emprended*", "emprendim*", "cooperativas"
  ),
  "impulso_productivo_industrial" = c(
    "fábricas", "fabricas", "industria", "productos", "producción", "nacional", "fomentar"
  ),
  "obra_publica_construccion" = c(
    "obras", "obra", "construcción", "construcc*", "centros"
  ),
  "incentivos_impuestos_costos" = c(
    "impuestos", "iva", "incentiv*", "crédit*", "credito*", "benefic*", "financiamiento",
    "reducci*", "reducir", "bajar"
  ),
  "condiciones_laborales" = c(
    "salario", "salarios", "sueldo*", "horas", "jornada"
  ),
  "jovenes_primer_empleo" = c(
    "jóven*", "joven*", "experiencia", "primer", "primero", "pasantías"
  ),
  "sector_publico_privado" = c(
    "estado", "públicas", "privadas"
  ),
  "vivienda_alquiler" = c(
    "vivienda", "viviendas", "alquiler", "alquileres"
  ),
  "territorio_desarrollo_local" = c(
    "interior", "ciudad", "local", "departamentos", "barrios"
  ),
  "tecnologia_innovacion" = c(
    "tecnolog*"
  )
))


encuesta <- readxl::read_excel(paste0("data/processed/analysis/", year, "/", round_id, "/r11_q7.xlsx")) |>
  mutate(q8_norm = norm_txt(q8))

toks <- tokens(encuesta$q8_norm, remove_punct = TRUE, remove_numbers = TRUE)

mat <- dfm(toks) |>
  dfm_lookup(dict_q8, valuetype = "glob", case_insensitive = TRUE)

cats <- names(dict_q8)

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

prioridad <- names(dict_q8)

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
  out[[paste0("q8_", i)]] <- vapply(
    ord_list,
    function(x) if (length(x) >= i) x[[i]] else NA_character_,
    character(1)
  )
}

# Eliminar las columnas de quanteda y la columna normalizada
out <- out |>
  relocate(starts_with("q8_"), .after = q8) |> 
  select(-q8_norm, -all_of(cats), c(etiquetas, etiqueta_principal))

resumen <- out |>
  # Recalcular el resumen usando solo las columnas finales
  select(starts_with("q8_")) |>
  pivot_longer(everything(), names_to = "posicion", values_to = "categoria") |>
  filter(!is.na(categoria)) |>
  count(categoria, name = "n") |>
  arrange(desc(n))

print(resumen)

openxlsx::write.xlsx(out, paste0("data/processed/analysis/", year, "/", round_id, "/r11_q8.xlsx"))
