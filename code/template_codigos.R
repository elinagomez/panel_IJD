library(readr)
library(dplyr)
library(purrr)
library(ellmer)

# plantilla base: reemplaza solo los valores de config
config <- list(
  year = 2025,
  round = "RXX",
  pregunta = "qX",
  base_path = "data/processed",
  campo_salida = "codigo",
  tema = "tema_slug",
  contexto = "respuestas a la pregunta: \"texto de la pregunta\"",
  ejemplo_salida = "PRIMER_CODIGO_DEL_CSV",
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

# codigos (el csv debe tener columnas: codigo, descripcion)
codigos_df <- read_csv(paste0(
  config$base_path, "/analysis/", config$year, "/", config$round,
  "/codigos_", config$pregunta, ".csv"
))
categorias <- codigos_df$codigo

# construccion del prompt
definiciones <- paste0("- ", codigos_df$codigo, ": ", codigos_df$descripcion, collapse = "\n")

system_prompt <- paste(
  paste0("Developer: Eres un clasificador de ", config$contexto,
         ". Comienza con una breve lista de verificacion conceptual para asegurarte de que sigues ",
         "todos los pasos necesarios para la clasificacion. Analiza cada comentario recibido y ",
         "asignale exactamente una de las siguientes categorias, utilizando ese valor como el unico ",
         "posible para el campo obligatorio '", config$campo_salida, "':"),
  definiciones,
  paste0("Selecciona solo uno de estos valores y responde unicamente con un objeto JSON. ",
         "Asegurate de seguir el formato exacto de salida especificado a continuacion y revisa que ",
         "solo incluyes la clave '", config$campo_salida, "' con uno de los valores listados. ",
         "No agregues explicaciones adicionales."),
  "## Formato de salida",
  "Responde unicamente con un objeto JSON siguiendo este ejemplo:",
  paste0('{"', config$campo_salida, '": "', config$ejemplo_salida, '"}'),
  sep = "\n\n"
)

cat(system_prompt)

# chat
chat <- chat_openai(
  model = "gpt-5-nano",
  base_url = "https://api.openai.com/v1",
  api_key = Sys.getenv("OPENAI_API_KEY"),
  system_prompt = system_prompt
)

# funcion clasificadora con manejo de errores
clasificar_texto <- function(texto) {
  tryCatch({
    resultado <- chat$chat_structured(texto, type = type_object(codigo = type_enum(values = categorias)))
    return(resultado$codigo)
  }, error = function(e) {
    return(NA_character_)
  })
}

# procesamiento en bloques
bloques <- split(raw, (seq_len(nrow(raw)) - 1L) %/% config$chunk_size)

clasificacion_results <- map_dfr(bloques, function(bloque) {
  bloque |>
    mutate(
      codigo_temp = map_chr(.data[[config$pregunta]], clasificar_texto, .progress = TRUE)
    ) |>
    select(all_of(names(raw)), codigo_temp)
}) |>
  rename(!!paste0(config$campo_salida, "_", config$pregunta) := codigo_temp)

# guardar y resumen
outfile <- paste0(
  config$base_path, "/analysis/", config$year, "/", config$round, "/",
  config$tema, "_", config$pregunta, ".csv"
)
write_csv(clasificacion_results, outfile)

final_col <- paste0(config$campo_salida, "_", config$pregunta)
cat("distribucion de codigos (", final_col, "):\n", sep = "")
print(table(clasificacion_results[[final_col]], useNA = "ifany"))
cat("archivo guardado en: ", outfile, "\n", sep = "")
