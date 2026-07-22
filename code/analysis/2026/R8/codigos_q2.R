source("code/analysis/2026/R8/codigos_utils.R")

config <- list(
  year = 2026,
  round = "R8",
  pregunta = "q2",
  base_path = "data/processed",
  tema = "debate_liberar_presos",
  chunk_size = 20L,
  model = "gpt-5.4-mini",
  contexto_abierto = paste0(
    'respuestas a la pregunta: "Uno de los puntos álgidos del debate fue cuando ',
    'Bordaberry acusó al gobierno de pretender liberar presos con el nuevo Plan ',
    'Nacional de Seguridad Pública. El Ministro Negro defendió la propuesta ',
    'aclarando que lo que se busca es que NO ingresen a la cárcel personas con ',
    'delitos de muy leve entidad para evitar que sean reclutados para formar parte ',
    'de una banda criminal. ¿cuál es tu opinión sobre este debate?"'
  ),
  category_to_field = c(
    "1. Postura General" = "postura_general_q2",
    "2. Percepción del Sistema" = "percepcion_sistema_q2",
    "3. Argumentos y Temores" = "argumentos_temores_q2",
    "4. Propuestas de Solución" = "propuestas_solucion_q2"
  ),
  instrucciones_generales = c(
    "Clasifica cada dimensión de forma independiente.",
    "Fuerza la codificación en cada dimensión usando el código sustantivo más cercano al sentido predominante de la respuesta."
  )
)

run_multicategory_coding(config)
