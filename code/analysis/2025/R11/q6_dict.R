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

dict_q6 <- dictionary(list(
  salud_mental_adicciones = c(
    "mental*", "depresion*", "depresión*", "suicidio*", "adiccion*", "adicciones",
    "psicolog*", "psiquiatr*", "siquiatr*"
  ),
  medicacion = c(
    "medicamentos*", "medicacion*", "medicación*", "remedio*", "ticket", "tickets",
    "farmacia", "farmacias"
  ),
  personal_condiciones = c(
    "personal", "médico", "medico", "médicos", "medicos",
    "enfermera*", "enfermero*", "especialista*", "profesionale*",
    "contratar*", "contratación", "contratacion", "formación", "formacion",
    "capacitación", "capacitacion", "sueldo*", "salario*"
  ),
  infraestructura_equipamiento = c(
    "instalacion*", "instalaciones", "infraestructura*", "edilicia",
    "equipamiento*", "equipo*", "aparato*", "maquina*", "tecnologia*", "tecnología*",
    "cama*", "diagnostic*", "imagen", "hospital*", "cti"
  ),
  atencion_primaria_prevencion = c(
    "policlinic*", "policlínic*", "comunitari*", "barrial*", "barriales",
    "primaria", "primera", "prevencion*", "prevención*", "promocion*", "promoción*",
    "vacunacion*", "vacunación*", "control*"
  ),
  emergencia_traslados = c(
    "ambulancia*", "emergencia*", "urgencia*", "traslado*"
  ),
  acceso_espera = c(
    "acceso*", "espera*", "lista*", "demor*", "turno*", "tiempo*", "año*", "mes*"
  ),
  descentralizacion = c(
    "interior", "ciudad*", "capital", "departamental", "departamento*"
  ),
  costos_financiamiento = c(
    "costo*", "reducir", "compra", "centralizado", "fondo*", "financiamiento*",
    "priori*"
  ),
  enfermedades_raras = c(
    "rara*", "terminal*", "oncológ*", "oncolog*", "diabetes", "hipertensi*",
    "cronica*"
  ),
  calidad_trato = c(
    "calidad", "humana", "digno*", "trato", "respeto", "empatia", "niñ*", "ancian*"
  )
))


encuesta <- readxl::read_excel(paste0("data/processed/analysis/", year, "/", round_id, "/r11_q5.xlsx")) |>
  mutate(q6_norm = norm_txt(q6))

toks <- tokens(encuesta$q6_norm, remove_punct = TRUE, remove_numbers = TRUE)

mat <- dfm(toks) |>
  dfm_lookup(dict_q6, valuetype = "glob", case_insensitive = TRUE)

cats <- names(dict_q6)

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

prioridad <- names(dict_q6)

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
  out[[paste0("q6_", i)]] <- vapply(
    ord_list,
    function(x) if (length(x) >= i) x[[i]] else NA_character_,
    character(1)
  )
}

# Eliminar las columnas de quanteda y la columna normalizada
out <- out |>
  relocate(starts_with("q6_"), .after = q6) |> 
  select(-q6_norm, -all_of(cats), c(etiquetas, etiqueta_principal))

resumen <- out |>
  # Recalcular el resumen usando solo las columnas finales
  select(starts_with("q6_")) |>
  pivot_longer(everything(), names_to = "posicion", values_to = "categoria") |>
  filter(!is.na(categoria)) |>
  count(categoria, name = "n") |>
  arrange(desc(n))

print(resumen)

openxlsx::write.xlsx(out, paste0("data/processed/analysis/", year, "/", round_id, "/r11_q6.xlsx"))
