library(readr)
library(dplyr)
library(purrr)
library(ellmer)
year <- 2025
round_id <- "R8"


round <- "R8"
raw <- read_csv(paste0("data/processed/transcriptions/output/2025/transcripcion_", round, ".csv"))
q <- "q6"
out_path <- paste0("data/processed/analysis/", year, "/", round_id, "")

# funciones 
procesar_bloque <- function(df_bloque, chat, pregunta) {
  type_sentimiento <- type_object(
    sentimiento = type_enum(values = c("NEU", "NEG", "POS"))
  )

  clasificar_seguro <- purrr::possibly(
    function(txt) chat$chat_structured(txt, type = type_sentimiento),
    otherwise = list(sentimiento = NA_character_),
    quiet = TRUE
  )

  df_bloque |>
    mutate(
      res_llm    = map(.data[[pregunta]], clasificar_seguro, .progress = TRUE),
      sentimiento = map_chr(res_llm, "sentimiento")
    ) |>
    select(rowid, sentimiento)
}

# configurar chat
cat_sentimientos <- c("NEU", "NEG", "POS")
system_prompt <- paste(
  "/no_think",
  "eres un clasificador de sentimientos.",
  "analiza el texto según estos criterios específicos:",
  "NEU: enunciado descriptivo de algo que pasó/va a pasar, sin implicancias emocionales",
  "NEU: si una persona habla bien de sí misma",
  "NEG: adjetivación negativa de algo que hizo/dijo una persona",
  "NEG: análisis con adjetivos negativos sobre acciones/dichos de una persona",
  "NEG: comentarios irónicos sobre una persona",
  "POS: adjetivación positiva de algo que hizo/dijo una persona",
  "POS: análisis con adjetivos positivos sobre acciones/dichos de una persona",
  "POS: miembros de una agrupación hablan bien de otra persona de la misma agrupación",
  "elige exactamente uno de estos valores para 'sentimiento':",
  paste(cat_sentimientos, collapse = ", "),
  "devuelve solo un objeto json con ese único campo."
)

chat <- chat_ollama(
  model         = "qwen3",
  base_url      = "http://localhost:11434",
  system_prompt = system_prompt,
  api_args      = list(max_tokens = 25)
)

# clasificar sentimientos por bloques
chunk_size <- 20L
bloques <- split(raw, (seq_len(nrow(raw)) - 1L) %/% chunk_size)

sentiment_results <- map_dfr(
  bloques,
  ~procesar_bloque(.x, chat, q)
)

# generar dataset final
sentiment_col <- paste0("sentimiento_", q)

final_dataset <- raw |>
  left_join(sentiment_results, by = "rowid") |>
  relocate(sentimiento, .after = all_of(q)) |>
  rename(!!sentiment_col := sentimiento)

# guardar resultado
write_csv(final_dataset, paste0(out_path, "/", round, "_sentiment_", q, ".csv"))

# mostrar resumen
if (nrow(sentiment_results) > 0) {
  resumen <- sentiment_results |>
    count(sentimiento, sort = TRUE) |>
    mutate(porcentaje = round(n / sum(n) * 100, 1))

  cat("\nresumen de clasificación de sentimientos:\n")
  print(resumen)

  total     <- nrow(sentiment_results)
  positivos <- sum(sentiment_results$sentimiento == "POS", na.rm = TRUE)
  negativos <- sum(sentiment_results$sentimiento == "NEG", na.rm = TRUE)
  neutrales <- sum(sentiment_results$sentimiento == "NEU", na.rm = TRUE)

  cat("\n total textos:", total,
      "\n positivos  :", positivos, "(", round(positivos / total * 100, 1), "%)",
      "\n negativos  :", negativos, "(", round(negativos / total * 100, 1), "%)",
      "\n neutrales  :", neutrales, "(", round(neutrales / total * 100, 1), "%)", "\n")
}