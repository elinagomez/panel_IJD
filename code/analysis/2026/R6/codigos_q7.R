library(readr)
library(dplyr)
library(purrr)
library(ellmer)

# configuracion
config <- list(
  year = 2026,
  round = "R6",
  pregunta = "q7",
  base_path = "data/processed",
  tema = "tecnologia_seguridad",
  contexto = "respuestas a la pregunta: \"El plan se propone un uso intensivo de la tecnologia: videovigilancia masiva, cruce de datos interinstitucionales, historiales de salud y trazabilidad de los ciudadanos. Que posibles beneficios y riesgos percibes en esta medida?\"",
  ejemplo_postura = "apoyo_neto",
  ejemplo_beneficio = "eficacia_investigacion_prevencion",
  ejemplo_riesgo = "privacidad_libertades_vigilancia",
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
codigos_beneficio_df <- read_csv(paste0(
  config$base_path, "/analysis/", config$year, "/", config$round,
  "/codigos_", config$pregunta, "_beneficio.csv"
))
codigos_riesgo_df <- read_csv(paste0(
  config$base_path, "/analysis/", config$year, "/", config$round,
  "/codigos_", config$pregunta, "_riesgo.csv"
))
codigos_planos_df <- read_csv(paste0(
  config$base_path, "/analysis/", config$year, "/", config$round,
  "/codigos_", config$pregunta, ".csv"
))

categorias_postura <- codigos_postura_df$codigo
categorias_beneficio <- codigos_beneficio_df$codigo
categorias_riesgo <- codigos_riesgo_df$codigo
categorias_planas <- codigos_planos_df$codigo

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
  foco = "Tu tarea es clasificar la postura general de la respuesta frente a la medida. Debes evaluar si la respuesta cierra a favor, a favor con resguardos, en equilibrio entre beneficios y riesgos, en critica dominada por los riesgos, en escepticismo sobre la viabilidad o en desinformacion.",
  codigos_df = codigos_postura_df,
  campo_salida = "postura",
  ejemplo_salida = config$ejemplo_postura,
  instrucciones_extra = paste(
    "Si la respuesta expresa desconocimiento, falta de opinion o no tiene suficiente contenido, usa 'ns_nr_desinformado'.",
    "No uses 'ns_nr_desinformado' si la respuesta, aunque breve, expresa una posicion comprensible a favor o en contra.",
    "Si el eje es humo, inviabilidad o incapacidad estatal para implementarlo, usa 'escepticismo_inviabilidad'.",
    "Si el texto enumera beneficios y riesgos sin cierre claro, usa 'ambivalente_tradeoff'.",
    sep = "\n"
  )
)

system_prompt_beneficio <- build_system_prompt(
  contexto = config$contexto,
  foco = "Tu tarea es clasificar el beneficio principal que la persona percibe. No clasifiques la postura general ni el riesgo, solo el beneficio dominante o su ausencia.",
  codigos_df = codigos_beneficio_df,
  campo_salida = "beneficio",
  ejemplo_salida = config$ejemplo_beneficio,
  instrucciones_extra = paste(
    "Si la respuesta no formula un beneficio reconocible o lo niega, usa 'sin_beneficio_explicito'.",
    "No infieras un beneficio solo porque la pregunta menciona posibles ventajas; el beneficio debe estar formulado en el texto.",
    "Distingue entre eficacia general para investigar o prevenir, rapidez operativa, integracion de datos para diagnostico o seguimiento, y control/trazabilidad con efecto disuasivo.",
    sep = "\n"
  )
)

system_prompt_riesgo <- build_system_prompt(
  contexto = config$contexto,
  foco = "Tu tarea es clasificar el riesgo principal que la persona percibe. No clasifiques la postura general ni el beneficio, solo el riesgo dominante o su ausencia.",
  codigos_df = codigos_riesgo_df,
  campo_salida = "riesgo",
  ejemplo_salida = config$ejemplo_riesgo,
  instrucciones_extra = paste(
    "Si la respuesta no formula un riesgo reconocible o dice que no hay riesgos, usa 'sin_riesgo_explicito'.",
    "No infieras un riesgo solo porque la pregunta menciona posibles peligros; el riesgo debe estar formulado en el texto.",
    "Distingue entre privacidad/libertades, seguridad de datos y hackeo, abuso estatal o espionaje, e inoperancia o ineficacia de implementacion.",
    sep = "\n"
  )
)

cat("=== PROMPT POSTURA ===\n")
cat(system_prompt_postura)
cat("\n\n=== PROMPT BENEFICIO ===\n")
cat(system_prompt_beneficio)
cat("\n\n=== PROMPT RIESGO ===\n")
cat(system_prompt_riesgo)
cat("\n")

chat_postura <- chat_openai(
  model = "gpt-5.4-mini",
  base_url = "https://api.openai.com/v1",
  api_key = Sys.getenv("OPENAI_API_KEY"),
  system_prompt = system_prompt_postura
)

chat_beneficio <- chat_openai(
  model = "gpt-5.4-mini",
  base_url = "https://api.openai.com/v1",
  api_key = Sys.getenv("OPENAI_API_KEY"),
  system_prompt = system_prompt_beneficio
)

chat_riesgo <- chat_openai(
  model = "gpt-5.4-mini",
  base_url = "https://api.openai.com/v1",
  api_key = Sys.getenv("OPENAI_API_KEY"),
  system_prompt = system_prompt_riesgo
)

clasificar_postura <- function(texto) {
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

clasificar_beneficio <- function(texto) {
  tryCatch({
    resultado <- chat_beneficio$chat_structured(
      texto,
      type = type_object(beneficio = type_enum(values = categorias_beneficio))
    )
    return(resultado$beneficio)
  }, error = function(e) {
    return(NA_character_)
  })
}

clasificar_riesgo <- function(texto) {
  tryCatch({
    resultado <- chat_riesgo$chat_structured(
      texto,
      type = type_object(riesgo = type_enum(values = categorias_riesgo))
    )
    return(resultado$riesgo)
  }, error = function(e) {
    return(NA_character_)
  })
}

derivar_codigo_q7 <- function(postura, beneficio, riesgo) {
  postura <- if (is.na(postura)) "" else postura
  beneficio <- if (is.na(beneficio)) "" else beneficio
  riesgo <- if (is.na(riesgo)) "" else riesgo

  if (!nzchar(postura)) {
    return(NA_character_)
  }

  if (postura == "ns_nr_desinformado") {
    return("ns_nr_desinformado")
  }

  if (postura == "escepticismo_inviabilidad") {
    return("escepticismo_inviabilidad")
  }

  if (postura == "apoyo_condicionado_resguardos") {
    return("apoyo_condicionado_resguardos")
  }

  if (postura == "apoyo_neto" && beneficio == "integracion_datos_diagnostico") {
    return("apoyo_neto_integracion_datos")
  }

  if (postura == "apoyo_neto") {
    return("apoyo_neto_seguridad_control")
  }

  if (postura == "ambivalente_tradeoff" && riesgo == "privacidad_libertades_vigilancia") {
    return("tradeoff_privacidad_libertades")
  }

  if (
    postura == "ambivalente_tradeoff" &&
      riesgo %in% c(
        "seguridad_datos_filtracion_hackeo",
        "abuso_estatal_espionaje_desvio",
        "inoperancia_ineficacia"
      )
  ) {
    return("tradeoff_gobernanza_datos")
  }

  if (postura == "critica_riesgo_dominante" && riesgo == "privacidad_libertades_vigilancia") {
    return("critica_privacidad_libertades")
  }

  if (
    postura == "critica_riesgo_dominante" &&
      riesgo %in% c(
        "seguridad_datos_filtracion_hackeo",
        "abuso_estatal_espionaje_desvio",
        "inoperancia_ineficacia"
      )
  ) {
    return("critica_datos_abuso")
  }

  NA_character_
}

# procesamiento en bloques
bloques <- split(raw, (seq_len(nrow(raw)) - 1L) %/% config$chunk_size)

clasificacion_results <- map_dfr(bloques, function(bloque) {
  bloque |>
    mutate(
      postura_temp = map_chr(.data[[config$pregunta]], clasificar_postura, .progress = TRUE),
      beneficio_temp = map_chr(.data[[config$pregunta]], clasificar_beneficio, .progress = TRUE),
      riesgo_temp = map_chr(.data[[config$pregunta]], clasificar_riesgo, .progress = TRUE),
      codigo_temp = pmap_chr(
        list(postura_temp, beneficio_temp, riesgo_temp),
        derivar_codigo_q7
      )
    ) |>
    select(all_of(names(raw)), postura_temp, beneficio_temp, riesgo_temp, codigo_temp)
}) |>
  rename(
    !!paste0("postura_", config$pregunta) := postura_temp,
    !!paste0("beneficio_", config$pregunta) := beneficio_temp,
    !!paste0("riesgo_", config$pregunta) := riesgo_temp,
    !!paste0("codigo_", config$pregunta) := codigo_temp
  )

final_col_postura <- paste0("postura_", config$pregunta)
final_col_beneficio <- paste0("beneficio_", config$pregunta)
final_col_riesgo <- paste0("riesgo_", config$pregunta)
final_col_codigo <- paste0("codigo_", config$pregunta)

codigos_invalidos <- setdiff(
  unique(stats::na.omit(clasificacion_results[[final_col_codigo]])),
  categorias_planas
)

if (length(codigos_invalidos) > 0) {
  stop("Se generaron codigos planos fuera del codebook: ", paste(codigos_invalidos, collapse = ", "))
}

# guardar y resumen
outfile <- paste0(
  config$base_path, "/analysis/", config$year, "/", config$round, "/",
  config$tema, "_", config$pregunta, ".csv"
)
write_csv(clasificacion_results, outfile)

cat("\n=== RESUMEN FINAL ===\n")
cat("distribucion de codigos de postura (", final_col_postura, "):\n", sep = "")
print(table(clasificacion_results[[final_col_postura]], useNA = "ifany"))
cat("\ndistribucion de codigos de beneficio (", final_col_beneficio, "):\n", sep = "")
print(table(clasificacion_results[[final_col_beneficio]], useNA = "ifany"))
cat("\ndistribucion de codigos de riesgo (", final_col_riesgo, "):\n", sep = "")
print(table(clasificacion_results[[final_col_riesgo]], useNA = "ifany"))
cat("\ndistribucion de codigos planos (", final_col_codigo, "):\n", sep = "")
print(table(clasificacion_results[[final_col_codigo]], useNA = "ifany"))
cat("archivo guardado en: ", outfile, "\n", sep = "")
