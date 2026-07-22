library(readr)
library(dplyr)
library(purrr)
library(ellmer)
year <- 2025
round_id <- "R7"


# cargo respuestas abiertas
raw <- read_csv(paste0("data/processed/transcriptions/output/", year, "/transcripcion_", round_id, ".csv")) |>
  filter(!is.na(q2)) |>
  mutate(rowid = row_number())

# preparo el archivo incremental
out_path <- "data/analysis/q2_tagged_progress.csv"
if (file.exists(out_path)) {
  done <- read_csv(out_path, show_col_types = FALSE)
  raw  <- anti_join(raw, done, by = "rowid")
} else {
  done <- tibble()
}

# creo el chat con ollama
cat_motivos <- c(
  "deber_estado",
  "solidaridad",
  "situacion_economica",
  "ineficiencia",
  "responsabilidad_individual"
)

chat <- chat_ollama(
  model    = "qwen3",
  base_url = "http://localhost:11434",
  system_prompt =
    paste(
      "eres un codificador de motivos.",
      "elige exactamente uno de estos valores para 'motivo':",
      paste(cat_motivos, collapse = ", "),
      "devuelve solo un objeto json con ese único campo."
    ),
  api_args = list(max_tokens = 25)
)

# defino el esquema de salida
type_motivo <- type_object(
  motivo = type_enum(values = cat_motivos)
)

# función robusta para clasificar cada texto
clasificar_seguro <- purrr::possibly(
  function(txt) chat$chat_structured(txt, type = type_motivo),
  otherwise = list(motivo = NA_character_),
  quiet = TRUE
)

# función auxiliar para procesar un bloque y mostrar la barra de purrr
procesar_bloque <- function(df_bloque) {
  df_bloque |>
    mutate(
      res_llm = map(q2, clasificar_seguro, .progress = TRUE),
      motivo  = map_chr(res_llm, "motivo")
    ) |>
    select(rowid, motivo)
}

# divido en bloques de 20 filas y proceso
bloques <- split(raw, (seq_len(nrow(raw)) - 1) %/% 20)
for (bloque in bloques) {
  res_parcial <- procesar_bloque(bloque)
  write_csv(bind_rows(done, res_parcial), out_path)
  done <- bind_rows(done, res_parcial)
}

# exporto la versión final
write_csv(done, "data/analysis/q2_tagged_final.csv")

done |>
  count(motivo, sort = TRUE)

# hago join 
final <- raw |>
  left_join(done, by = "rowid")

final <- final |>
  relocate(motivo, .after = q2)

final <- final |> rename(motivo_q2 = motivo)
write_csv(final, paste0("data/processed/analysis/", year, "/", round_id, "/r7_motivo_q2.csv"))
