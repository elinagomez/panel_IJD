source("code/analysis/2026/R9/codigos_utils.R")

config <- list(
  year = 2026,
  round = "R9",
  pregunta = "q6",
  base_path = "data/processed",
  tema = "empleo_priorizado",
  campo_salida = "codigos_q6",
  chunk_size = 20L,
  model = "gpt-5.4-mini",
  contexto_abierto = paste0(
    'respuestas a la pregunta abierta: "¿Qué opinión tienes sobre esta propuesta ',
    'del gobierno que prioriza el empleo en jóvenes, mujeres que crían solas y ',
    'mayores de 50 años?"'
  ),
  instrucciones_extra = c(
    "Clasifica la opinion sobre la propuesta de priorizacion de empleo.",
    "Si la respuesta combina apoyo, reparos o grupos especificos, devuelve todos los codigos aplicables."
  )
)

run_multicode_coding(config)
