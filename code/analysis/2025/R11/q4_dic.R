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

dict_q4 <- dictionary(list(
  generacion_empleo = c(
    "trabajo*", "empleo*", "puesto*", "fuente*", "trabajar*", "ocupacion*"
  ),

  atraccion_inversion_industria = c(
    "empresa*", "inversor*", "inversion*", "invertir*", "industria*",
    "fabrica*", "parque*", "planta*", "productiv*"
  ),

  incentivos_fiscales_subsidios = c(
    "impuesto*", "exoner*", "subsid*", "tribut*", "carga*", "iva", "bps",
    "aporte*", "alicuot*", "beneficio*", "incentiv*"
  ),

  capacitacion_formacion_oficios = c(
    "capacit*", "curso*", "formacion*", "oficio*", "taller*", "habilidad*",
    "reconvers*", "certific*", "entrenam*", "practic*", "pasant*", "estudi*"
  ),

  insercion_jovenes_mayores_experiencia = c(
    "joven*", "experienc*", "primer*", "egresad*", "mayor*", "50", "adulto*",
    "inclusion*", "discapacidad*", "vulnerabl*", "edad*", "jornales", "solidarios"
  ),

  salarios_condiciones_formalizacion = c(
    "salario*", "sueldo*", "digno*", "formal*", "precar*", "jornada*",
    "horario*", "estabilidad*", "contrat*", "seguro*", "remunera*",
    "renunerado"
  ),

  pymes_emprendimiento = c(
    "pyme*", "emprend*", "microempresa*", "cooperativ*", "autonom*",
    "credito*", "financiamien*", "unipersonal*"
  ),

  empleo_territorial = c(
    "interior*", "departament*", "barrio*", "local*", "territor*", "rural*"
  ),

  clientelismo_amiguismo = c(
    "amigu*", "clientel*", "compra*", "voto*", "favores", "favor*",
    "politic*", "corrupcion*", "corrupto*", "dedo*", "personales",
    "atornillad*", "relaciones", "sorteo*", "conocido*", "concurso*",
    "sortead*", "amigo*"
  )
))



encuesta <- readxl::read_excel(paste0("data/processed/analysis/", year, "/", round_id, "/r11_q3.xlsx")) |>
  mutate(q4_norm = norm_txt(q4))

toks <- tokens(encuesta$q4_norm, remove_punct = TRUE, remove_numbers = TRUE)

mat <- dfm(toks) |>
  dfm_lookup(dict_q4, valuetype = "glob", case_insensitive = TRUE)

cats <- names(dict_q4)

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

prioridad <- names(dict_q4)

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
  out[[paste0("q4_", i)]] <- vapply(
    ord_list,
    function(x) if (length(x) >= i) x[[i]] else NA_character_,
    character(1)
  )
}

# Eliminar las columnas de quanteda y la columna normalizada
out <- out |>
  relocate(starts_with("q4_"), .after = q4) |> 
  select(-q4_norm, -all_of(cats))

resumen <- out |>
  # Recalcular el resumen usando solo las columnas finales
  select(starts_with("q4_")) |>
  pivot_longer(everything(), names_to = "posicion", values_to = "categoria") |>
  filter(!is.na(categoria)) |>
  count(categoria, name = "n") |>
  arrange(desc(n))

print(resumen)

openxlsx::write.xlsx(out, paste0("data/processed/analysis/", year, "/", round_id, "/r11_q4.xlsx"))
