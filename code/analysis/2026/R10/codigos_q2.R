source("code/analysis/2026/R10/codigos_utils.R")

config <- list(
  year = 2026,
  round = "R10",
  pregunta = "q2",
  base_path = "data/processed",
  transcription_file = "data/processed/transcriptions/output/2026/transcripcion_R10_2.xlsx",
  tema = "papel_educacion",
  campo_salida = "codigos_q2",
  chunk_size = 20L,
  model = "gpt-5.4-mini",
  contexto_abierto = 'respuestas a la pregunta abierta: "¿Cuál cree usted que debería ser el papel de la educación en estos tiempos?"',
  instrucciones_extra = c(
    "Clasifica las funciones o roles esperados para la educación.",
    "Si se mencionan varios roles, devuelve todos los códigos aplicables."
  )
)

run_multicode_coding(config)
