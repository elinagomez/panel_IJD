library(readr)
library(dplyr)
library(purrr)
library(ellmer)

# configuracion
config <- list(
  year = 2026,
  round = "R7",
  pregunta = "q11",
  base_path = "data/processed",
  campo_salida = "codigo",
  tema = "mejoras_educacion_publica",
  contexto = "respuestas a la pregunta: \"Que aspectos cree que deberian cambiarse o mejorarse en la educacion publica?\"",
  ejemplo_salida = "calidad_exigencia_aprendizajes",
  chunk_size = 20L
)

dir.create(
  paste0(config$base_path, "/analysis/", config$year, "/", config$round),
  recursive = TRUE, showWarnings = FALSE
)

raw <- read_csv(paste0(
  config$base_path, "/transcriptions/output/", config$year,
  "/transcripcion_", config$round, ".csv"
))

codigos_df <- read_csv(paste0(
  config$base_path, "/analysis/", config$year, "/", config$round,
  "/codigos_", config$pregunta, ".csv"
))
categorias <- codigos_df$codigo

definiciones <- paste0("- ", codigos_df$codigo, ": ", codigos_df$descripcion, collapse = "\n")

system_prompt <- paste(
  paste0(
    "Developer: Eres un clasificador de ", config$contexto,
    ". Comienza con una breve lista de verificacion conceptual para asegurarte de que sigues ",
    "todos los pasos necesarios para la clasificacion. Analiza cada comentario recibido y ",
    "asignale exactamente una de las siguientes categorias, utilizando ese valor como el unico ",
    "posible para el campo obligatorio '", config$campo_salida, "':"
  ),
  definiciones,
  paste(
    "Debes elegir el aspecto principal a mejorar. Si una respuesta enumera varios cambios, prioriza el primero o el que organiza mejor el sentido general del comentario.",
    "Usa 'cambio_integral_del_sistema' solo cuando la persona pida una reforma global sin un aspecto dominante claro.",
    "Usa 'sin_cambios_o_ns_nr' si la persona no sabe, no opina o dice que no cambiaria nada importante.",
    "Distingue entre 'contenidos_curricula_metodologias' y 'calidad_exigencia_aprendizajes': el primero refiere a programas, contenidos o enfoque pedagogico; el segundo a nivel de aprendizaje, evaluacion y rigor academico.",
    "Distingue entre 'docentes_gestion_gobernanza' y 'cobertura_tiempo_plantel': el primero refiere a docentes, su formacion o la gestion; el segundo a cantidad de personal, horas o tamano de grupos.",
    sep = "\n"
  ),
  paste0(
    "Selecciona solo uno de estos valores y responde unicamente con un objeto JSON. ",
    "Asegurate de seguir el formato exacto de salida especificado a continuacion y revisa que ",
    "solo incluyes la clave '", config$campo_salida, "' con uno de los valores listados. ",
    "No agregues explicaciones adicionales."
  ),
  "## Formato de salida",
  "Responde unicamente con un objeto JSON siguiendo este ejemplo:",
  paste0('{"', config$campo_salida, '": "', config$ejemplo_salida, '"}'),
  sep = "\n\n"
)

cat(system_prompt)

chat <- chat_openai(
  model = "gpt-5.4-mini",
  base_url = "https://api.openai.com/v1",
  api_key = Sys.getenv("OPENAI_API_KEY"),
  system_prompt = system_prompt
)

clasificar_texto <- function(texto) {
  if (is.na(texto) || !nzchar(trimws(texto))) {
    return(NA_character_)
  }

  tryCatch({
    resultado <- chat$chat_structured(
      texto,
      type = type_object(codigo = type_enum(values = categorias))
    )
    return(resultado$codigo)
  }, error = function(e) {
    return(NA_character_)
  })
}

bloques <- split(raw, (seq_len(nrow(raw)) - 1L) %/% config$chunk_size)

clasificacion_results <- map_dfr(bloques, function(bloque) {
  bloque |>
    mutate(
      codigo_temp = map_chr(.data[[config$pregunta]], clasificar_texto, .progress = TRUE)
    ) |>
    select(all_of(names(raw)), codigo_temp)
}) |>
  rename(!!paste0(config$campo_salida, "_", config$pregunta) := codigo_temp)

final_col <- paste0(config$campo_salida, "_", config$pregunta)

codigos_invalidos <- setdiff(
  unique(stats::na.omit(clasificacion_results[[final_col]])),
  categorias
)

if (length(codigos_invalidos) > 0) {
  stop("Se generaron codigos fuera del codebook: ", paste(codigos_invalidos, collapse = ", "))
}

outfile <- paste0(
  config$base_path, "/analysis/", config$year, "/", config$round, "/",
  config$tema, "_", config$pregunta, ".csv"
)
write_csv(clasificacion_results, outfile)

cat("distribucion de codigos (", final_col, "):\n", sep = "")
print(table(clasificacion_results[[final_col]], useNA = "ifany"))
cat("archivo guardado en: ", outfile, "\n", sep = "")
