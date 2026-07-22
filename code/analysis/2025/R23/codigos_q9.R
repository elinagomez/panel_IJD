library(readr)
library(dplyr)
library(purrr)
library(ellmer)

# configuración
config <- list(
  year = 2025,
  round = "R23",
  pregunta = "q9",
  base_path = "data/processed",
  campo_salida = "codigo",
  tema = "resolusion_situacion",
  contexto = "respuestas a la pregunta: \"¿Cómo creés que debería resolverse definitivamente esta situación?
a.	Álvaro Danza debería dejar el cargo de presidente de ASSE
b. Puede continuar como presidente de ASSE aun cuando tenga otros empleos privados en salud.
b.	Puede continuar como presidente de ASSE solo si renuncia a sus empleos privados en salud.
c.	No tengo una posición tomada 
¿Por qué? 
 
\"",  # contexto actualizado
  ejemplo_salida = "Incompatibilidad legal/constitucional",
  chunk_size = 20L
)

# datos
raw <- read_csv(paste0(config$base_path, "/transcriptions/output/", config$year, "/transcripcion_", config$round, ".csv"))

# códigos (el csv debe tener columnas: codigo, descripcion)
codigos_df <- read_csv(paste0(config$base_path, "/analysis/", config$year, "/", config$round, "/codigos_", config$pregunta, ".csv"))
categorias <- codigos_df$codigo

# construcción del prompt
definiciones <- paste0("- ", codigos_df$codigo, ": ", codigos_df$descripcion, collapse = "\n")

system_prompt <- paste(
  paste0("Developer: Eres un clasificador de ", config$contexto, 
         ". Comienza con una breve lista de verificación conceptual para asegurarte de que sigues ",
         "todos los pasos necesarios para la clasificación. Analiza cada comentario recibido y ",
         "asígnale exactamente una de las siguientes categorías, utilizando ese valor como el único ",
         "posible para el campo obligatorio '", config$campo_salida, "':"),
  definiciones,
  paste0("Selecciona sólo uno de estos valores y responde únicamente con un objeto JSON. ",
         "Asegúrate de seguir el formato exacto de salida especificado a continuación y revisa que ",
         "solo incluyes la clave '", config$campo_salida, "' con uno de los valores listados. ",
         "No agregues explicaciones adicionales."),
  "## Formato de salida",
  "Responde únicamente con un objeto JSON siguiendo este ejemplo:",
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

# función clasificadora con manejo de errores
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
outfile <- paste0(config$base_path, "/analysis/", config$year, "/", config$round, "/", config$tema, "_", config$pregunta, ".csv")
write_csv(clasificacion_results, outfile)

final_col <- paste0(config$campo_salida, "_", config$pregunta)
cat("distribución de códigos (", final_col, "):\n", sep = "")
print(table(clasificacion_results[[final_col]], useNA = "ifany"))
cat("archivo guardado en: ", outfile, "\n", sep = "")

