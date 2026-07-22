library(readr)
library(dplyr)
library(purrr)
library(ellmer)

# configuracion
config <- list(
  year = 2026,
  round = "R6",
  pregunta = "q3",
  base_path = "data/processed",
  tema = "seguridad_interinstitucional",
  contexto = "respuestas a la pregunta: \"opinion sobre que el Plan Nacional de Seguridad Publica sea coordinado entre Ministerio del Interior, Fiscalia, Poder Judicial, Mides, INAU, ANEP, gobiernos departamentales y municipios\"",
  ejemplo_postura = "favorable_claro",
  ejemplo_fundamento = "coordinacion_responsabilidad_compartida",
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
codigos_planos_df <- read_csv(paste0(
  config$base_path, "/analysis/", config$year, "/", config$round,
  "/codigos_", config$pregunta, ".csv"
))

categorias_postura <- codigos_postura_df$codigo
categorias_fundamento <- codigos_fundamento_df$codigo
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
  foco = "Tu tarea es clasificar la postura general de la respuesta frente a esa caracteristica del plan.",
  codigos_df = codigos_postura_df,
  campo_salida = "postura",
  ejemplo_salida = config$ejemplo_postura,
  instrucciones_extra = "Si la respuesta no toma posicion clara porque expresa desconocimiento, desinformacion o ausencia de opinion, usa 'ns_nr_desinformado'."
)

system_prompt_fundamento <- build_system_prompt(
  contexto = config$contexto,
  foco = "Tu tarea es clasificar el fundamento principal que organiza la respuesta. No clasifiques la postura general, sino la razon dominante que la persona da para apoyar, condicionar, criticar o rechazar esa caracteristica del plan.",
  codigos_df = codigos_fundamento_df,
  campo_salida = "fundamento",
  ejemplo_salida = config$ejemplo_fundamento,
  instrucciones_extra = paste(
    "Si la respuesta no desarrolla una razon identificable, o solo expresa formulas breves de aprobacion, rechazo o desconocimiento sin explicacion, usa 'sin_fundamento_explicito'.",
    "Si la razon principal es que la seguridad deberia quedar centralmente en el Ministerio del Interior o la policia, usa 'interior_central'.",
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

clasificar_fundamento <- function(texto) {
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

derivar_codigo_q3 <- function(postura, fundamento) {
  if (is.na(postura) || !nzchar(postura)) {
    return(NA_character_)
  }

  if (postura == "ns_nr_desinformado") {
    return("ns_nr_desinformado")
  }

  if (postura == "rechazo_enfoque") {
    return("rechazo_solo_interior")
  }

  if (postura == "critico_esceptico") {
    return("critica_burocracia_no_novedad")
  }

  if (postura == "favorable_condicionado") {
    return("favor_condicionado_implementacion")
  }

  if (postura == "favorable_claro" && fundamento == "integral_multicausal") {
    return("favor_integral_multicausal")
  }

  if (postura == "favorable_claro" && fundamento == "coordinacion_responsabilidad_compartida") {
    return("favor_coordinacion_compartida")
  }

  if (postura == "favorable_claro") {
    return("favor_generico")
  }

  NA_character_
}

# procesamiento en bloques
bloques <- split(raw, (seq_len(nrow(raw)) - 1L) %/% config$chunk_size)

clasificacion_results <- map_dfr(bloques, function(bloque) {
  bloque |>
    mutate(
      postura_temp = map_chr(.data[[config$pregunta]], clasificar_postura, .progress = TRUE),
      fundamento_temp = map_chr(.data[[config$pregunta]], clasificar_fundamento, .progress = TRUE),
      codigo_temp = map2_chr(postura_temp, fundamento_temp, derivar_codigo_q3)
    ) |>
    select(all_of(names(raw)), postura_temp, fundamento_temp, codigo_temp)
}) |>
  rename(
    !!paste0("postura_", config$pregunta) := postura_temp,
    !!paste0("fundamento_", config$pregunta) := fundamento_temp,
    !!paste0("codigo_", config$pregunta) := codigo_temp
  )

final_col_postura <- paste0("postura_", config$pregunta)
final_col_fundamento <- paste0("fundamento_", config$pregunta)
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
cat("\ndistribucion de codigos de fundamento (", final_col_fundamento, "):\n", sep = "")
print(table(clasificacion_results[[final_col_fundamento]], useNA = "ifany"))
cat("\ndistribucion de codigos planos (", final_col_codigo, "):\n", sep = "")
print(table(clasificacion_results[[final_col_codigo]], useNA = "ifany"))
cat("archivo guardado en: ", outfile, "\n", sep = "")
