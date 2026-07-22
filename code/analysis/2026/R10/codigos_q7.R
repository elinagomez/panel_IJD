source("code/analysis/2026/R10/codigos_utils.R")

config <- list(
  year = 2026,
  round = "R10",
  pregunta = "q7",
  base_path = "data/processed",
  transcription_file = "data/processed/transcriptions/output/2026/transcripcion_R10_2.xlsx",
  tema = "opinion_mas_barrio",
  campo_salida = "codigos_q7",
  chunk_size = 20L,
  model = "gpt-5.4-mini",
  contexto_abierto = 'respuestas a la pregunta abierta: "En términos generales ¿Qué opinión te merece el Programa Más barrio?"',
  instrucciones_extra = c(
    "Clasifica la evaluación general del Programa Más Barrio.",
    "Si la respuesta aprueba la idea pero duda de la ejecución, devuelve ambos códigos pertinentes si corresponde."
  )
)

run_multicode_coding(config)
