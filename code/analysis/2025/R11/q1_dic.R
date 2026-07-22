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

dict_q1 <- dictionary(list(
  trabajo_empleo = c(
    "trabajo*", "empleo*", "puesto* de trabajo*", "fuente* de trabajo*",
    "laburo*", "laboral*", "salario*", "sueldo*", "contratacion*", "desempleo*",
    "estabilidad laboral", "oportunidades laboral*"
  ),
  seguridad_delito = c(
    "seguridad*", "delincuenc*", "delincuent*", "robo*", "hurto*", "violenc*",
    "homicid*", "policia*", "patrull*", "carcel*", "reincidenc*", "boca*"
  ),
  salud = c(
    "salud*", "salud mental", "hospital*", "asse", "mutual*",
    "medic*ment*", "emergenc*", "ambulanc*", "especialista*", "psicolog*", "psiquiatr*"
  ),
  educacion = c(
    "educacion*", "escuela*", "liceo*", "utu", "utec", "docent*", "maestr*",
    "profesor*", "beca*", "formacion*"
  ),
  oportunidades_social = c(
    "oportunidad*", "pobreza*", "inclusion*", "ayuda* social*", "apoyo social*",
    "mides", "igualdad*", "equidad*", "infan*"
  ),
  economia_precios = c(
    "economi*", "inflacion*", "precio*", "costo de vida", "impuesto*", "iva",
    "tarifa*", "combustibl*", "invers*", "inviert*", "gasto*", "poder adquisitivo",
    "riqueza", "disrib*", "presupuesto"
  ),
  vivienda = c(
    "vivienda*", "alquiler*", "asentamiento*", "techo*", "saneamient*", "habitat*"
  ),
  gobierno_corrupcion = c(
    "corrupcion*", "transparenc*", "honestid*", "burocrac*", "gestion publica",
    "gasto del estado", "estado ineficiente", "ministerio*", "inef*"
  ),
  transporte = c(
    "transporte*", "omnibus*", "colectivo*", "boleto*", "ruta*", "camino*",
    "calle*", "movilidad*"
  ),
  medio_ambiente = c(
    "ambiente*", "medio ambient*", "agua*", "sequ*", "basura*", "limpieza*",
    "recicla*", "contamin*"
  ),
  justicia = c(
    "justicia*", "juez*", "fiscal*", "pena*", "sistema penal*"
  ),
  drogas_adicciones = c(
    "droga*", "adiccion*", "consumo problematic*", "narco*", "boca* de venta",
    "alcohol*", "pasta base", "marihuana*", "cocaina*"
  )
))

encuesta <- read_csv(paste0("data/processed/transcriptions/output/", year, "/transcripcion_", round_id, ".csv"), show_col_types = FALSE) |>
  mutate(q1_norm = norm_txt(q1))

toks <- tokens(encuesta$q1_norm, remove_punct = TRUE, remove_numbers = TRUE)

mat <- dfm(toks) |>
  dfm_lookup(dict_q1, valuetype = "glob", case_insensitive = TRUE)

cats <- names(dict_q1)

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

prioridad <- c(
  "trabajo_empleo","seguridad_delito","salud","oportunidades_social",
  "educacion","economia_precios","vivienda","gobierno_corrupcion",
  "transporte","medio_ambiente","justicia","drogas_adicciones"
)

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
  out[[paste0("q1_", i)]] <- vapply(
    ord_list,
    function(x) if (length(x) >= i) x[[i]] else NA_character_,
    character(1)
  )
}

# Eliminar las columnas de quanteda y la columna normalizada
out <- out |>
  relocate(starts_with("q1_"), .after = q1) |> 
  select(-q1_norm, -all_of(cats))

resumen <- out |>
  # Recalcular el resumen usando solo las columnas finales
  select(starts_with("q1_")) |>
  pivot_longer(everything(), names_to = "posicion", values_to = "categoria") |>
  filter(!is.na(categoria)) |>
  count(categoria, name = "n") |>
  arrange(desc(n))

print(resumen)

openxlsx::write.xlsx(out, paste0("data/processed/analysis/", year, "/", round_id, "/r11_q1.xlsx"))
