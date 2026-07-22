source("code/analysis/2026/R10/codigos_utils.R")

config <- list(
  year = 2026,
  round = "R10",
  pregunta = "q10",
  pregunta_cerrada = "q9",
  base_path = "data/processed",
  transcription_file = "data/processed/transcriptions/output/2026/transcripcion_R10_2.xlsx",
  tema = "corrupcion_policial_seguridad",
  campo_salida = "codigos_q10",
  chunk_size = 20L,
  model = "gpt-5.4-mini",
  contexto_abierto = 'respuestas a la pregunta abierta: "Si responde que sí o Más o menos, ¿De qué manera?"',
  contexto_cerrado = paste0(
    'respuestas a la pregunta cerrada previa: "Y específicamente con respecto ',
    'a la corrupción en la policía en Uruguay ¿crees que es un factor que afecta ',
    'a la seguridad pública? Opciones: Si; Más o menos; No; No lo sé."'
  ),
  closed_value_map = c(
    "Si" = "Sí",
    "Sí" = "Sí",
    "Más o menos" = "Más o menos",
    "No" = "No",
    "No lo sé" = "No lo sé"
  ),
  skip_values = "99",
  instrucciones_extra = c(
    "Usa la respuesta cerrada previa para interpretar si la persona está describiendo una afectación concreta de la corrupción policial sobre la seguridad.",
    "Clasifica solo respuestas abiertas con contenido sustantivo sobre la manera en que afecta."
  )
)

run_multicode_coding(config)
