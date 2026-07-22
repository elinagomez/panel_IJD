source("code/analysis/2026/R8/codigos_utils.R")

config <- list(
  year = 2026,
  round = "R8",
  pregunta = "q10",
  pregunta_cerrada = "q9",
  base_path = "data/processed",
  tema = "importancia_rehabilitacion",
  chunk_size = 20L,
  model = "gpt-5.4-mini",
  contexto_abierto = 'respuestas a la pregunta: "¿Qué te hace pensar así?"',
  contexto_cerrado = paste0(
    'respuestas a la pregunta cerrada previa: "¿Cuán importante te parece que ',
    'son las políticas de rehabilitación para las personas que están en la ',
    'cárcel? Opciones: Muy importante; Algo importante; Ni una cosa ni la otra; ',
    'Poco importante; Nada importante; No tengo opinión."'
  ),
  closed_value_map = c(
    A = "Muy importante",
    B = "Algo importante",
    C = "Ni una cosa ni la otra",
    D = "Poco importante",
    E = "Nada importante",
    F = "No tengo opinión"
  ),
  category_to_field = c(
    "Única" = "codigo_q10"
  ),
  instrucciones_generales = c(
    "Clasifica solo el fundamento dominante de la respuesta.",
    "La respuesta cerrada previa sirve como contexto, pero no debes inventar una razon que no aparezca en la respuesta abierta.",
    "Fuerza la codificación usando el fundamento sustantivo más cercano cuando la respuesta tenga contenido utilizable."
  )
)

run_multicategory_coding(config)
