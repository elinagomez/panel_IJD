source("code/analysis/2026/R9/codigos_utils.R")

config <- list(
  year = 2026,
  round = "R9",
  pregunta = "q4",
  base_path = "data/processed",
  tema = "preocupacion_economica",
  campo_salida = "codigos_q4",
  chunk_size = 20L,
  model = "gpt-5.4-mini",
  contexto_abierto = paste0(
    'respuestas a la pregunta abierta: "¿Cuándo pensás en la situación económica ',
    'del país, que es lo que MÁS te preocupa?"'
  ),
  instrucciones_extra = c(
    "Clasifica las preocupaciones economicas mencionadas por la persona.",
    "Si aparecen varias preocupaciones concretas, devuelve todos los codigos pertinentes."
  )
)

run_multicode_coding(config)
