source("code/analysis/2026/R9/codigos_utils.R")

config <- list(
  year = 2026,
  round = "R9",
  pregunta = "q2",
  pregunta_cerrada = "q1",
  base_path = "data/processed",
  tema = "politica_desinteres",
  campo_salida = "codigos_q2",
  chunk_size = 20L,
  model = "gpt-5.4-mini",
  contexto_abierto = 'respuestas a la pregunta abierta: "¿Por qué?"',
  contexto_cerrado = paste0(
    'respuestas a la pregunta cerrada previa: "Según información proveniente de ',
    'empresas que estudian opinión pública se señala que cada vez hay un mayor ',
    'número de personas que expresan desinterés por la política. ¿Cuán de acuerdo ',
    'está con esta afirmación? Opciones: Muy en desacuerdo; En desacuerdo; ',
    'Ni de acuerdo ni en desacuerdo; De acuerdo; Muy de acuerdo."'
  ),
  instrucciones_extra = c(
    "Clasifica las razones que explican acuerdo o desacuerdo con la idea de desinteres por la politica.",
    "La escala de q1 ayuda a interpretar si la persona esta justificando acuerdo, desacuerdo o una posicion matizada."
  )
)

run_multicode_coding(config)
