source("code/analysis/2026/R8/codigos_utils.R")

config <- list(
  year = 2026,
  round = "R8",
  pregunta = "q8",
  pregunta_cerrada = "q7",
  base_path = "data/processed",
  tema = "inversion_carceles",
  chunk_size = 20L,
  model = "gpt-5.4-mini",
  contexto_abierto = 'respuestas a la pregunta: "¿Por qué?"',
  contexto_cerrado = paste0(
    'respuestas a la pregunta cerrada previa: "Actualmente en Uruguay hay más de ',
    '16.000 personas privadas de libertad en cárceles. Como informan diversos ',
    'medios hay varias cárceles con sobrepoblación y otras con problemas para ',
    'atender a los internos. Cuál de las siguientes frases representa mejor tu ',
    'opinión sobre las responsabilidades del Estado uruguayo. Crees que es ',
    'prioritario invertir: Opciones: en la construcción de nuevas cárceles; en la ',
    'mejora de las cárceles ya existentes; tanto en la construcción de cárceles ',
    'como en la mejora de las ya existentes; NO es prioritario que el Estado ',
    'uruguayo invierta en cárceles."'
  ),
  closed_value_map = c(
    A = "en la construcción de nuevas cárceles",
    B = "en la mejora de las cárceles ya existentes",
    C = "tanto en la construcción de cárceles como en la mejora de las ya existentes",
    D = "NO es prioritario que el Estado uruguayo invierta en cárceles"
  ),
  category_to_field = c(
    "1. Prioridad de Inversión" = "prioridad_inversion_q8",
    "2. Justificación: Rehabilitación" = "justificacion_rehabilitacion_q8",
    "3. Justificación: Punitivismo" = "justificacion_punitivismo_q8",
    "4. Gestión y Recursos" = "gestion_recursos_q8"
  ),
  instrucciones_generales = c(
    "Clasifica cada dimensión de forma independiente.",
    "La respuesta cerrada previa debe ayudar a resolver respuestas abiertas muy breves sobre la prioridad de inversión.",
    "Fuerza la codificación en cada dimensión usando el código sustantivo más cercano."
  )
)

run_multicategory_coding(config)
