source("code/analysis/2026/R8/codigos_utils.R")

config <- list(
  year = 2026,
  round = "R8",
  pregunta = "q4",
  pregunta_cerrada = "q3",
  base_path = "data/processed",
  tema = "accionar_oposicion",
  chunk_size = 20L,
  model = "gpt-5.4-mini",
  contexto_abierto = 'respuestas a la pregunta: "¿Por qué?"',
  contexto_cerrado = paste0(
    'respuestas a la pregunta cerrada previa: "¿Cómo evalúas el accionar de la ',
    'oposición en este caso? Opciones: Está actuando de manera correcta; No está ',
    'actuando de manera correcta; No tengo una opinión formada."'
  ),
  closed_value_map = c(
    A = "Está actuando de manera correcta",
    B = "No está actuando de manera correcta",
    C = "No tengo una opinión formada"
  ),
  category_to_field = c(
    "1. Postura Debate (Negro vs Bordaberry)" = "postura_debate_q4",
    "2. Evaluación de la Oposición" = "evaluacion_oposicion_q4",
    "3. Argumentos (Justificación)" = "argumentos_justificacion_q4",
    "4. Percepción del Sistema" = "percepcion_sistema_q4"
  ),
  instrucciones_generales = c(
    "Clasifica cada dimensión de forma independiente.",
    "La respuesta cerrada previa debe servir para resolver formulaciones breves o elipticas sobre la evaluacion de la oposicion.",
    "Fuerza la codificación en cada dimensión usando el código sustantivo más cercano."
  )
)

run_multicategory_coding(config)
