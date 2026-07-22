source("code/analysis/2026/R10/codigos_utils.R")

config <- list(
  year = 2026,
  round = "R10",
  pregunta = "q6",
  base_path = "data/processed",
  transcription_file = "data/processed/transcriptions/output/2026/transcripcion_R10_2.xlsx",
  tema = "riesgos_minterior_mas_barrio",
  campo_salida = "codigos_q6",
  chunk_size = 20L,
  model = "gpt-5.4-mini",
  contexto_abierto = 'respuestas a la pregunta abierta: "¿Identificas algún riesgo de la participación del Ministerio del Interior en este Programa?"',
  instrucciones_extra = c(
    "Clasifica riesgos percibidos de la participación del Ministerio del Interior en Más Barrio.",
    "Si no identifica riesgos, usa el código de ausencia de riesgos del codebook."
  )
)

run_multicode_coding(config)
