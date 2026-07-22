library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(quanteda)
library(stringi)
year <- 2025
round_id <- "R15"


norm_txt <- function(x) {
  x <- ifelse(is.na(x), "", x)
  x |>
    stringi::stri_trans_general("Latin-ASCII") |>
    tolower() |>
    stringr::str_squish()
}

dict_q3 <- dictionary(list(
  # 1) protección de hijas/os y adolescentes
  "proteccion_de_hijos" = c(
    "hij*", "nino*", "adolescen*", "custodia", "tenencia"
  ),

  # 2) denuncias y alertas previas (señales / audios / amenazas)
  "denuncias_y_alertas_previas" = c(
    "denuncia*", "senales", "alerta*", "audio", "audios", "amenaz*"
  ),

  # 3) acción estatal / justicia / policía
  "accion_estado_justicia_policia" = c(
    "policia", "justicia", "juez*", "jueza*", "fiscal*", "fiscalia",
    "ministerio", phrase("ministerio del interior")
  ),

  # 4) salud mental, tratamiento y seguimiento
  "salud_mental_y_tratamiento" = c(
    phrase("salud mental"), "psicolog*", "psiqui*", "tratamient*", "terapia", "seguim*"
  ),

  # 5) violencia de género / doméstica
  "violencia_de_genero_domestica" = c(
    phrase("violencia de genero"), phrase("violencia domestica"),
    "femicid*", "vbg", "mujer", "violencia"
  ),

  # 6) medidas cautelares y protección
  "medidas_cautelares_y_proteccion" = c(
    "tobillera", phrase("tobillera electronica"),
    "alejamiento", phrase("orden de alejamiento"),
    "restric*", phrase("custodia policial"), phrase("guardia policial")
  ),

  # 7) ineficacia y demoras (tiempos, fallas, impunidad)
  "ineficacia_y_demoras" = c(
    "tard*", "agil", "eficient*", "demor*", "fall*", "impunid*"
  ),

  # 8) responsabilidad del agresor / premeditación
  "responsabilidad_del_agresor_premeditacion" = c(
    "asesin*", "matar*", "premedit*", "venganza", "castig*"
  ),

  # 9) responsabilidad social/familiar (no estatal)
  "responsabilidad_social_familia" = c(
    "familia", "sociedad", "vecin*", "callaron"
  ),

  # 10) inevitabilidad / arrebato / impulso
  "inevitabilidad_arrebato" = c(
    "inevitab*", "impuls*", "arrebato", phrase("no hay forma")
  ),

  # 11) suicidio
  "suicidio" = c("suicid*")
))



encuesta <- readr::read_csv(paste0("data/processed/transcriptions/output/", year, "/transcripcion_", round_id, ".csv")) |>
  mutate(q3_norm = norm_txt(q3))

toks <- tokens(encuesta$q3_norm, remove_punct = TRUE, remove_numbers = TRUE)

mat <- dfm(toks) |>
  dfm_lookup(dict_q3, valuetype = "glob", case_insensitive = TRUE)

cats <- names(dict_q3)

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

prioridad <- names(dict_q3)

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
  out[[paste0("q3_", i)]] <- vapply(
    ord_list,
    function(x) if (length(x) >= i) x[[i]] else NA_character_,
    character(1)
  )
}

# Eliminar las columnas de quanteda y la columna normalizada
out <- out |>
  relocate(starts_with("q3_"), .after = q3) |> 
  select(-q3_norm, -all_of(cats))

resumen <- out |>
  # Recalcular el resumen usando solo las columnas finales
  select(starts_with("q3_")) |>
  pivot_longer(everything(), names_to = "posicion", values_to = "categoria") |>
  filter(!is.na(categoria)) |>
  count(categoria, name = "n") |>
  arrange(desc(n))

print(resumen)

dir.create(paste0("data/processed/analysis/", year, "/", round_id, ""))
openxlsx::write.xlsx(out, paste0("data/processed/analysis/", year, "/", round_id, "/r15_q3.xlsx"))
