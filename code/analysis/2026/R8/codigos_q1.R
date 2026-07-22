source("code/analysis/2026/R8/codigos_utils.R")

config <- list(
  year = 2026,
  round = "R8",
  pregunta = "q1",
  base_path = "data/processed",
  tema = "interpelacion_recuerdos",
  chunk_size = 20L,
  model = "gpt-5.4-mini",
  contexto_abierto = paste0(
    'respuestas a la pregunta: "Hace unos días el senador colorado Pedro Bordaberry ',
    'condujo una interpelación al Ministro del Interior Carlos Negro. ',
    '¿Viste o escuchaste algo? ¿qué recuerdas haber visto o escuchado?"'
  ),
  category_to_field = c(
    "Nivel de exposición" = "nivel_exposicion_q1",
    "Dimensiones Temáticas (¿Qué recuerdan?)" = "dimension_tematica_q1",
    "Tono" = "tono_q1"
  ),
  instrucciones_generales = c(
    "Clasifica cada dimensión de forma independiente.",
    "Una misma respuesta puede indicar nivel de exposición alto y, al mismo tiempo, recordar un tema concreto y expresar un tono."
  )
)

run_multicategory_coding(config)
