source("code/analysis/2026/R10/codigos_utils.R")

config <- list(
  year = 2026,
  round = "R10",
  pregunta = "q12",
  base_path = "data/processed",
  transcription_file = "data/processed/transcriptions/output/2026/transcripcion_R10_2.xlsx",
  tema = "preocupaciones_tecnologias_seguridad",
  campo_salida = "codigos_q12",
  chunk_size = 20L,
  model = "gpt-5.4-mini",
  contexto_abierto = 'respuestas a la pregunta abierta: "¿Te genera alguna preocupación el uso de estas tecnologías? ¿cuál?"',
  instrucciones_extra = c(
    "Las tecnologías mencionadas son cámaras de videovigilancia, drones e inteligencia artificial en tareas de seguridad.",
    "Si la respuesta no expresa preocupación y apoya el uso de tecnologías, usa el código de sin preocupación/apoyo total."
  )
)

run_multicode_coding(config)
