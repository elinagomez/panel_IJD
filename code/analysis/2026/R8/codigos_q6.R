source("code/analysis/2026/R8/codigos_utils.R")

config <- list(
  year = 2026,
  round = "R8",
  pregunta = "q6",
  pregunta_cerrada = "q5",
  base_path = "data/processed",
  tema = "credibilidad_cifras",
  chunk_size = 20L,
  model = "gpt-5.4-mini",
  contexto_abierto = 'respuestas a la pregunta: "¿Por qué?"',
  contexto_cerrado = paste0(
    'respuestas a la pregunta cerrada previa: "Durante la interpelación, el ',
    'Ministro del Interior comunicó cifras que muestran descensos en los ',
    'homicidios, hurtos, rapiñas, estafas informáticas y abigeato. ¿Cuán creíble ',
    'te resultan las cifras comunicadas por el Ministerio del Interior? Opciones: ',
    'Me resultaron creíbles; No me resultaron creíbles; No sabría decir."'
  ),
  closed_value_map = c(
    A = "Me resultaron creíbles",
    B = "No me resultaron creíbles",
    C = "No sabría decir"
  ),
  category_to_field = c(
    "1. Evaluación" = "evaluacion_q6",
    "2. Argumentos de Desconfianza" = "argumentos_desconfianza_q6",
    "3. Argumentos de Confianza" = "argumentos_confianza_q6",
    "4. Influencia Externa" = "influencia_externa_q6",
    "5. Contexto / Otros" = "contexto_otros_q6"
  ),
  instrucciones_generales = c(
    "Clasifica cada dimensión de forma independiente.",
    "La respuesta cerrada previa es especialmente importante para resolver la evaluación cuando la explicación abierta es breve.",
    "Fuerza la codificación en cada dimensión usando el código sustantivo más cercano."
  )
)

run_multicategory_coding(config)
