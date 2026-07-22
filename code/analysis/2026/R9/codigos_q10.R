source("code/analysis/2026/R9/codigos_utils.R")

config <- list(
  year = 2026,
  round = "R9",
  pregunta = "q10",
  pregunta_cerrada = "q9",
  base_path = "data/processed",
  tema = "cooperacion_interinstitucional",
  campo_salida = "codigos_q10",
  chunk_size = 20L,
  model = "gpt-5.4-mini",
  contexto_abierto = 'respuestas a la pregunta abierta: "¿Por qué?"',
  contexto_cerrado = paste0(
    'respuestas a la pregunta cerrada previa: "En las propuestas que presentó el ',
    'gobierno se habla de la necesidad de acciones que comprometan a diferentes ',
    'Ministerios y Organismos promoviendo planes, proyectos y acciones ',
    'inter-institucionales en las que ministerios y organismos públicos coordinen ',
    'y cooperen para el logro de los fines propuestos. ¿Cuánto confías en que se ',
    'pueda lograr esta cooperación? Opciones: MUCHO; POCO; NADA."'
  ),
  instrucciones_extra = c(
    "Clasifica las razones de confianza o desconfianza sobre la cooperacion interinstitucional.",
    "La respuesta cerrada de q9 ayuda a interpretar si la razon expresa confianza alta, baja o nula."
  )
)

run_multicode_coding(config)
