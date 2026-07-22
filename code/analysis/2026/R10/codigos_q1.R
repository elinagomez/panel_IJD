source("code/analysis/2026/R10/codigos_utils.R")

config <- list(
  year = 2026,
  round = "R10",
  pregunta = "q1",
  base_path = "data/processed",
  transcription_file = "data/processed/transcriptions/output/2026/transcripcion_R10_2.xlsx",
  tema = "educacion_publica_valoraciones",
  campo_salida = "codigos_q1",
  chunk_size = 20L,
  model = "gpt-5.4-mini",
  contexto_abierto = paste0(
    'respuestas a la pregunta abierta: "En términos generales ¿Qué opinión, ',
    'sentimientos o valoraciones le despierta la educación pública en nuestro país?"'
  ),
  instrucciones_extra = c(
    "Clasifica valoraciones, sentimientos y preocupaciones sobre la educación pública.",
    "Si una respuesta combina una valoración positiva con críticas específicas, devuelve todos los códigos pertinentes."
  )
)

run_multicode_coding(config)
