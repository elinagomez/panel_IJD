source("code/analysis/2026/R10/codigos_utils.R")

config <- list(
  year = 2026,
  round = "R10",
  pregunta = "q4",
  pregunta_cerrada = "q3",
  base_path = "data/processed",
  transcription_file = "data/processed/transcriptions/output/2026/transcripcion_R10_2.xlsx",
  tema = "conocimiento_mas_barrio",
  campo_salida = "codigos_q4",
  chunk_size = 20L,
  model = "gpt-5.4-mini",
  contexto_abierto = 'respuestas a la pregunta abierta: "(para los que están muy o algo informados) ¿Qué conoce o ha oído hablar del Programa Más barrio?"',
  contexto_cerrado = paste0(
    'respuestas a la pregunta cerrada previa: "Cambiando de tema, ¿conoce o ha ',
    'oído hablar algo del Programa Más barrio? Opciones: Si, estoy muy informado; ',
    'Si, estoy algo informado; Escuché del programa pero no sé de que se trata; ',
    'No he escuchado nada al respecto; No contesta."'
  ),
  closed_value_map = c(
    `1` = "Si, estoy muy informado",
    `2` = "Si, estoy algo informado",
    `3` = "Escuché del programa pero no sé de que se trata",
    `4` = "No he escuchado nada al respecto",
    `5` = "No contesta"
  ),
  skip_values = "99",
  instrucciones_extra = c(
    "Usa la respuesta cerrada previa para interpretar el nivel declarado de conocimiento.",
    "Clasifica solo respuestas abiertas con contenido sustantivo sobre lo que conoce u oyó del programa."
  )
)

run_multicode_coding(config)
