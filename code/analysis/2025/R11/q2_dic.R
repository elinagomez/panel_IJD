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

dict_q2 <- dictionary(list(
  "Rapidez y Eficiencia" = c(
    "rapidez*", "rapido*", "agil*", "eficiencia*", "eficiente*",
    "tiempo* de espera*", "espera*", "demora*", "tiempo* de respuesta",
    "cola*", "fila*", "turno*", "agenda*", "agendar*", "sacar turno*",
    "numero* para agendar", "en tiempo y forma",
    "telemedicina", "teleconsulta*", "online", "app", "web", "turner*"
  ),

  "Acceso a Medicamentos y Especialistas" = c(
    "acceso* a medicament*", "medicament* disponible*", "falta* de medicament*",
    "farmacia*", "receta*", "medicaci*", "entrega de medicament*",
    "cobertura* medicament*", "disponibilidad* medicament*", "indicacion*",
    "acceso* a especialista*", "especialista*", "especialidad*", "interconsulta*",
    "derivacion*", "agenda* para especialista*", "espera para especialista*",
    "cardiolog*", "oftalmolog*", "otorrin*", "neurolog*", "traumat*", "ginecolog*",
    "pediatr*", "dermat*", "urolog*", "endocrin*", "odontolog*"
  ),

  "Mejor Calidad y Trato Humano" = c(
    "trato human*", "trato digno*", "buen trato", "calidez*", "empati*",
    "respeto*", "amabil*", "atencion human*", "atencion cercan*", "atencion digna",
    "calidad de atencion*", "calidad del servicio*", "mejorar atencion*",
    "mejor atencion*", "mejorar servicio*", "humaniza*"
  ),

  "Salud Mental y Adicciones" = c(
    "salud mental", "psicolog*", "psiquiatr*", "terapia*", "atencion psicolog*",
    "depresion*", "ansiedad*", "consumo problematic*", "adiccion*", "rehabilit*",
    "tratamiento* de adiccion*", "drog*", "alcohol*"
  ),

  "Infraestructura y Equidad Territorial" = c(
    "infraestructur*", "instalacion*", "instalac*", "obra* de salud",
    "centro* de salud", "policlinic*", "policlinica*", "hospital*", "sanatorio*",
    "equip* hospital*", "equipamient*", "aparato*", "maquinaria*",
    "tomograf*", "resonanc*", "rayos x", "ecograf*", "laboratorio*",
    "estudio* diagnost*", "diagnostic* por imagen*",
    "ambulancia*", "movil*",
    "descentraliz*", "equidad territorial", "cercan*", "proxim*", "cerca", "lejos",
    "interior", "departamento*", "barrio*", "rural", "viajar*", "traslado*", "distancia*",
    "servicio* cercano*", "atencion cercana"
  ),

  "Bajar costos" = c(
    "costo*", "coste*", "precio*", "ticket*", "orden*", "copago*", "co-pago*", "co pago*",
    "bono*", "barat*", "gratuit*", "gratis", "exoner*", "subsid*",
    "reintegro*", "reembolso*", "bajar* costo*", "menor* costo*"
  ),

  "Infancia y adolescencia" = c(
    "primera infancia", "ninez", "nino*", "menor*", "adolescen*", "juventud*",
    "matern*", "embaraz*", "parto*", "puerper*", "neonat*",
    "pediatri*", "caif"
  ),

  "Enfermedades raras" = c(
    "enfermedad* rara*", "enfermedad* poco frecuente*", "poco frecuente*",
    "enfermedad* huerfan*", "enfermedad* minoritari*",
    "medicament* de alto costo", "tratamiento* de alto costo", "alto costo* especifico*",
    "diagnostic* genetic*", "genetic*",
    "centro* de referencia", "tratamiento* especific*"
  )
))

encuesta <- readxl::read_excel(paste0("data/processed/analysis/", year, "/", round_id, "/r11_q1.xlsx")) |>
  mutate(q2_norm = norm_txt(q2))

toks <- tokens(encuesta$q2_norm, remove_punct = TRUE, remove_numbers = TRUE)

mat <- dfm(toks) |>
  dfm_lookup(dict_q2, valuetype = "glob", case_insensitive = TRUE)

cats <- names(dict_q2)

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

prioridad <- names(dict_q2)

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
  out[[paste0("q2_", i)]] <- vapply(
    ord_list,
    function(x) if (length(x) >= i) x[[i]] else NA_character_,
    character(1)
  )
}

# Eliminar las columnas de quanteda y la columna normalizada
out <- out |>
  relocate(starts_with("q2_"), .after = q2) |> 
  select(-q2_norm, -all_of(cats))

resumen <- out |>
  # Recalcular el resumen usando solo las columnas finales
  select(starts_with("q2_")) |>
  pivot_longer(everything(), names_to = "posicion", values_to = "categoria") |>
  filter(!is.na(categoria)) |>
  count(categoria, name = "n") |>
  arrange(desc(n))

print(resumen)

openxlsx::write.xlsx(out, paste0("data/processed/analysis/", year, "/", round_id, "/r11_q2.xlsx"))
