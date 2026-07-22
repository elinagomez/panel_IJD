library(readr)
library(dplyr)
library(purrr)
library(ellmer)
year <- 2025
round_id <- "R7"


# cargo respuestas abiertas
raw <- read_csv("data/analysis/r7_motivo_q2.csv") |>
  filter(!is.na(q6)) |>
  mutate(rowid = row_number())

# defino la tipología para q6
cat_motivos_q6 <- c(
  "seguridad_publica",
  "orden_disciplina",
  "ineficacia_justicia",
  "preocupacion_autoritaria",
  "impacto_contextual"
)

# archivo incremental
out_path <- "data/analysis/q6_tagged_progress.csv"
if (file.exists(out_path)) {
  done <- read_csv(out_path, show_col_types = FALSE)
  raw  <- anti_join(raw, done, by = "rowid")
} else {
  done <- tibble()
}

# creo el chat con ollama
chat <- chat_ollama(
  model    = "qwen3",
  base_url = "http://localhost:11434",
  system_prompt =
    paste(
      # contexto de la tarea
      "eres un codificador de motivos.",
      "lee la respuesta abierta de la pregunta q6:",
      "'un poco de mano dura del gobierno no viene mal al país'.",
      "",
      # instrucción de salida
      "elige exactamente uno de los siguientes valores para 'motivo':",
      paste(cat_motivos_q6, collapse = ", "),
      "",
      # formato de salida
      "devuelve solo un objeto json con ese único campo.",
      "",
      # ejemplos de referencia
      "ejemplos:",
      '{"motivo":"seguridad_publica"}        # texto: \"hay que meter presos a esos rateros; con más policía bajamos la delincuencia\"',
      '{"motivo":"orden_disciplina"}         # texto: \"hace falta imponer orden y respeto; la sociedad está descontrolada\"',
      '{"motivo":"ineficacia_justicia"}      # texto: \"los jueces garantistas sueltan a los delincuentes, por eso se necesita mano dura\"',
      '{"motivo":"preocupacion_autoritaria"} # texto: \"la mano dura termina en abusos de poder y violaciones de derechos humanos\"',
      '{"motivo":"impacto_contextual"}       # texto: \"después de la ola de asaltos que hubo este mes, es lógico endurecer las medidas\"',
      sep = "\n"
    ),
  api_args = list(max_tokens = 25)
)


# esquema de salida esperado
type_motivo_q6 <- type_object(
  motivo = type_enum(values = cat_motivos_q6)
)

# función robusta de clasificación (con reset) ------------------------------
clasificar_seguro <- purrr::possibly(
  function(txt) {
    res <- chat$chat_structured(txt, type = type_motivo_q6)
    chat$reset()              
    res
  },
  otherwise = list(motivo = NA_character_),
  quiet = TRUE
)

# función auxiliar para procesar en bloques 
procesar_bloque <- function(df_bloque) {
  df_bloque |>
    mutate(
      res_llm = map(q6, clasificar_seguro, .progress = TRUE),
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
write_csv(done, "data/analysis/q6_tagged_final.csv")

# resumen de frecuencias
done |>
  count(motivo, sort = TRUE)

# incorporo las etiquetas al dataset original y lo guardo
final <- read_csv("data/analysis/r7_motivo_q2.csv") |>
  mutate(rowid = row_number()) |>
  left_join(done, by = "rowid") |>
  relocate(motivo, .after = q6) |>
  rename(motivo_q6 = motivo)

write_csv(final, paste0("data/processed/analysis/", year, "/", round_id, "/r7_motivo_q6.csv"))