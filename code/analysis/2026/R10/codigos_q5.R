source("code/analysis/2026/R10/codigos_utils.R")

config <- list(
  year = 2026,
  round = "R10",
  pregunta = "q5",
  base_path = "data/processed",
  transcription_file = "data/processed/transcriptions/output/2026/transcripcion_R10_2.xlsx",
  tema = "acciones_minterior_mas_barrio",
  campo_salida = "codigos_q5",
  chunk_size = 20L,
  model = "gpt-5.4-mini",
  contexto_abierto = paste0(
    'respuestas a la pregunta abierta: "Teniendo en cuenta esto, ¿En qué tipo ',
    'de acciones cree que el Ministerio del Interior debería involucrarse?" ',
    'El contexto es el Programa Más Barrio.'
  ),
  instrucciones_extra = c(
    "Clasifica las acciones esperadas para el Ministerio del Interior dentro del Programa Más Barrio.",
    "Si la respuesta menciona patrullaje, cercanía, narcotráfico, prevención o intervención integral a la vez, devuelve todos los códigos pertinentes."
  )
)

run_multicode_coding(config)
