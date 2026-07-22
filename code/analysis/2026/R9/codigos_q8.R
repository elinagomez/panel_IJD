source("code/analysis/2026/R9/codigos_utils.R")

config <- list(
  year = 2026,
  round = "R9",
  pregunta = "q8",
  base_path = "data/processed",
  tema = "estrategia_situacion_calle",
  campo_salida = "codigos_q8",
  chunk_size = 20L,
  model = "gpt-5.4-mini",
  contexto_abierto = paste0(
    'respuestas a la pregunta abierta: "¿Qué opinas de la Estrategia?" ',
    'La estrategia refiere al abordaje de las personas en situación de calle.'
  ),
  instrucciones_extra = c(
    "Clasifica la evaluacion de la estrategia para personas en situacion de calle.",
    "Si la respuesta menciona simultaneamente diseño, implementacion, vivienda, salud mental u otros ejes, devuelve todos los codigos pertinentes."
  )
)

run_multicode_coding(config)
