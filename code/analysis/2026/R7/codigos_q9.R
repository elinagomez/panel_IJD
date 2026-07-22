library(readr)
library(dplyr)
library(purrr)
library(ellmer)

# configuracion
config <- list(
  year = 2026,
  round = "R7",
  pregunta = "q9",
  base_path = "data/processed",
  tema = "educacion_publica",
  contexto = "respuestas a la pregunta: \"Que opinion tiene sobre la educacion publica en nuestro pais?, que sentimientos o valoraciones le despierta?\"",
  ejemplo_postura = "defensa_entusiasta",
  ejemplo_fundamento = "valor_publico_igualdad",
  chunk_size = 20L
)

dir.create(
  paste0(config$base_path, "/analysis/", config$year, "/", config$round),
  recursive = TRUE, showWarnings = FALSE
)

# datos
raw <- read_csv(paste0(
  config$base_path, "/transcriptions/output/", config$year,
  "/transcripcion_", config$round, ".csv"
))

# codebooks
codigos_postura_df <- read_csv(paste0(
  config$base_path, "/analysis/", config$year, "/", config$round,
  "/codigos_", config$pregunta, "_postura.csv"
))
codigos_fundamento_df <- read_csv(paste0(
  config$base_path, "/analysis/", config$year, "/", config$round,
  "/codigos_", config$pregunta, "_fundamento.csv"
))

categorias_postura <- codigos_postura_df$codigo
categorias_fundamento <- codigos_fundamento_df$codigo

build_system_prompt <- function(contexto, foco, codigos_df, campo_salida, ejemplo_salida, instrucciones_extra = NULL) {
  definiciones <- paste0("- ", codigos_df$codigo, ": ", codigos_df$descripcion, collapse = "\n")

  paste(
    paste0(
      "Developer: Eres un clasificador de ", contexto, ". ",
      foco, " Comienza con una breve lista de verificacion conceptual para asegurarte de que sigues ",
      "todos los pasos necesarios para la clasificacion. Analiza cada comentario recibido y ",
      "asignale exactamente una de las siguientes categorias, utilizando ese valor como el unico ",
      "posible para el campo obligatorio '", campo_salida, "':"
    ),
    definiciones,
    instrucciones_extra,
    paste0(
      "Selecciona solo uno de estos valores y responde unicamente con un objeto JSON. ",
      "Asegurate de seguir el formato exacto de salida especificado a continuacion y revisa que ",
      "solo incluyes la clave '", campo_salida, "' con uno de los valores listados. ",
      "No agregues explicaciones adicionales."
    ),
    "## Formato de salida",
    "Responde unicamente con un objeto JSON siguiendo este ejemplo:",
    paste0('{"', campo_salida, '": "', ejemplo_salida, '"}'),
    sep = "\n\n"
  )
}

system_prompt_postura <- build_system_prompt(
  contexto = config$contexto,
  foco = "Tu tarea es clasificar la postura general y el tono valorativo de la respuesta frente a la educacion publica. Debes evaluar si la respuesta cierra en defensa entusiasta, en apoyo con matices, en orgullo historico con preocupacion actual, en critica moderada, en critica severa o en ausencia de postura util.",
  codigos_df = codigos_postura_df,
  campo_salida = "postura",
  ejemplo_salida = config$ejemplo_postura,
  instrucciones_extra = paste(
    "Respuestas breves pero claramente positivas como 'bien', 'es buena', 'me gusta', 'me parece bien' o equivalentes van en 'defensa_entusiasta' si no agregan un problema o caveat explicito.",
    "Respuestas breves pero claramente negativas como 'negativa', 'mala', 'mediocre', 'esta peor' o equivalentes no van en 'ns_nr_sin_contacto'; deben clasificarse como critica moderada o severa segun la intensidad.",
    "Si la respuesta combina aspectos positivos y negativos pero no remite explicitamente al pasado, a la tradicion o al ideal historico de la educacion publica, usa 'apoyo_con_matices'.",
    "Solo uses 'orgullo_historico_y_preocupacion' cuando haya un contraste explicito entre el valor historico, la experiencia biografica o el ideal de la educacion publica y una preocupacion por el presente. Tambien incluye nostalgias claras del tipo 'volver a la educacion de antes' o recuperar un modelo perdido.",
    "No uses 'orgullo_historico_y_preocupacion' si la referencia al pasado o a la experiencia personal sirve para elogiar el presente o registrar avance; en esos casos usa 'defensa_entusiasta' o 'apoyo_con_matices'.",
    "Distingue entre 'critica_moderada' y 'critica_severa' por la intensidad del lenguaje. Reformas, carencias o deterioro sin dramatizacion corresponden a 'critica_moderada'. Crisis, desastre, decadencia extrema, indignacion o frases como 'no aprenden nada' corresponden a 'critica_severa'.",
    "Usa 'ns_nr_sin_contacto' solo si la persona no sabe, no opina, no tiene contacto o no hay informacion suficiente para inferir una postura. Respuestas breves pero inteligibles como 'muy positiva' o 'muy mala' no van en esa categoria.",
    sep = "\n"
  )
)

system_prompt_fundamento <- build_system_prompt(
  contexto = config$contexto,
  foco = "Tu tarea es clasificar el fundamento dominante que sostiene la valoracion de la persona. No clasifiques la postura general sino el argumento principal que explica esa postura.",
  codigos_df = codigos_fundamento_df,
  campo_salida = "fundamento",
  ejemplo_salida = config$ejemplo_fundamento,
  instrucciones_extra = paste(
    "Debes asignar un solo fundamento dominante. Si aparecen dos, elige el que mejor explique el cierre valorativo de la respuesta.",
    "Usa 'sin_fundamento_especifico' cuando la respuesta solo ofrezca una valoracion breve o vaga sin razon desarrollada.",
    "Si la respuesta menciona presupuesto, recortes, 6%+1, falta de plata, falta de infraestructura, centros a reparar o carencias materiales, prioriza 'recursos_infraestructura_presupuesto'.",
    "Distingue entre 'calidad_exigencia_aprendizajes' y 'valores_disciplina_y_familias': el primero se centra en nivel educativo, contenidos, exigencia y resultados; el segundo en respeto, disciplina, violencia o responsabilidad del hogar y de las familias.",
    "Si el texto atribuye el buen o mal nivel al compromiso, preparacion o desempeno de maestros, profesores, directores o al modo de gestion, prioriza 'docentes_y_gestion' sobre 'calidad_exigencia_aprendizajes'.",
    "Distingue entre 'recursos_infraestructura_presupuesto' y 'docentes_y_gestion': el primero se centra en plata, edificios, insumos, cupos, recortes o carencias materiales; el segundo en docentes, formacion, vocacion, direccion, burocracia, organizacion o sindicatos.",
    "Usa 'inclusion_y_apoyos' solo cuando el texto describa necesidades de apoyo especificas, adaptaciones o barreras para inclusion educativa.",
    "Si la respuesta culpa genericamente a un gobierno o a actores politicos sin desarrollar un fundamento del codebook, usa 'sin_fundamento_especifico'.",
    sep = "\n"
  )
)

cat("=== PROMPT POSTURA ===\n")
cat(system_prompt_postura)
cat("\n\n=== PROMPT FUNDAMENTO ===\n")
cat(system_prompt_fundamento)
cat("\n")

chat_postura <- chat_openai(
  model = "gpt-5.4-mini",
  base_url = "https://api.openai.com/v1",
  api_key = Sys.getenv("OPENAI_API_KEY"),
  system_prompt = system_prompt_postura
)

chat_fundamento <- chat_openai(
  model = "gpt-5.4-mini",
  base_url = "https://api.openai.com/v1",
  api_key = Sys.getenv("OPENAI_API_KEY"),
  system_prompt = system_prompt_fundamento
)

texto_vacio <- function(texto) {
  is.na(texto) || !nzchar(trimws(texto))
}

clasificar_postura <- function(texto) {
  if (texto_vacio(texto)) {
    return(NA_character_)
  }

  tryCatch({
    resultado <- chat_postura$chat_structured(
      texto,
      type = type_object(postura = type_enum(values = categorias_postura))
    )
    return(resultado$postura)
  }, error = function(e) {
    return(NA_character_)
  })
}

clasificar_fundamento <- function(texto) {
  if (texto_vacio(texto)) {
    return(NA_character_)
  }

  tryCatch({
    resultado <- chat_fundamento$chat_structured(
      texto,
      type = type_object(fundamento = type_enum(values = categorias_fundamento))
    )
    return(resultado$fundamento)
  }, error = function(e) {
    return(NA_character_)
  })
}

# procesamiento en bloques
bloques <- split(raw, (seq_len(nrow(raw)) - 1L) %/% config$chunk_size)

clasificacion_results <- map_dfr(bloques, function(bloque) {
  bloque |>
    mutate(
      postura_temp = map_chr(.data[[config$pregunta]], clasificar_postura, .progress = TRUE),
      fundamento_temp = map_chr(.data[[config$pregunta]], clasificar_fundamento, .progress = TRUE)
    ) |>
    select(all_of(names(raw)), postura_temp, fundamento_temp)
}) |>
  rename(
    !!paste0("postura_", config$pregunta) := postura_temp,
    !!paste0("fundamento_", config$pregunta) := fundamento_temp
  )

final_col_postura <- paste0("postura_", config$pregunta)
final_col_fundamento <- paste0("fundamento_", config$pregunta)

posturas_invalidas <- setdiff(
  unique(stats::na.omit(clasificacion_results[[final_col_postura]])),
  categorias_postura
)

if (length(posturas_invalidas) > 0) {
  stop("Se generaron posturas fuera del codebook: ", paste(posturas_invalidas, collapse = ", "))
}

fundamentos_invalidos <- setdiff(
  unique(stats::na.omit(clasificacion_results[[final_col_fundamento]])),
  categorias_fundamento
)

if (length(fundamentos_invalidos) > 0) {
  stop("Se generaron fundamentos fuera del codebook: ", paste(fundamentos_invalidos, collapse = ", "))
}

# guardar y resumen
outfile <- paste0(
  config$base_path, "/analysis/", config$year, "/", config$round, "/",
  config$tema, "_", config$pregunta, ".csv"
)
write_csv(clasificacion_results, outfile)

cat("\n=== RESUMEN FINAL ===\n")
cat("distribucion de posturas (", final_col_postura, "):\n", sep = "")
print(table(clasificacion_results[[final_col_postura]], useNA = "ifany"))
cat("\ndistribucion de fundamentos (", final_col_fundamento, "):\n", sep = "")
print(table(clasificacion_results[[final_col_fundamento]], useNA = "ifany"))
cat("archivo guardado en: ", outfile, "\n", sep = "")
