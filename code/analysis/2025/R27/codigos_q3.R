library(readr)
library(dplyr)
library(purrr)
library(ellmer)

# configuración
config <- list(
  year = 2025,
  round = "R27",
  pregunta_cerrada = "q1",
  pregunta_abierta = "q3",
  base_path = "data/processed",
  campo_salida = "codigo",
  tema = "solucion_problemas",
  contexto_cerrada = "respuestas a la pregunta: \"Nos interesa saber sobre esta clase de problemas en los cuales el ciudadano siente que el Estado o el gobierno podría hacer un poco más. ¿en cuáles de las siguientes áreas has experimentado algún problema en esta semana: educación, empleo, salud, transporte / tránsito, cuidados, seguridad, problemas vecinales, infraestructura urbana, otros (especificar)?\"",
  contexto_abierta = "respuestas a la pregunta: \"¿Cómo piensas que las políticas públicas del gobierno podrían ayudarte en este problema?\"",
  chunk_size = 20L
)


# datos
raw <- read_csv(paste0(config$base_path, "/transcriptions/output/", config$year, "/transcripcion_", config$round, ".csv"))

# códigos (el csv debe tener tres columnas: cat_cerrada, codigo, descripcion)
codigos_df <- read_csv(paste0(config$base_path, "/analysis/", config$year, "/", config$round, "/codigos_", config$pregunta_abierta, ".csv"))
# categorias <- codigos_df$codigo


# agrupar códigos por tema
codigos_tema <- codigos_df %>%
  group_by(cat_cerrada) %>%
  group_split() %>%
  setNames(map_chr(., ~unique(.x$cat_cerrada)))


# función para crear prompt específico por tema
prompt_maker <- function(categoria_cerrada, codigos_tema) {
  definiciones <- paste0("- ", codigos_tema$codigo, ": ", codigos_tema$descripcion, collapse = "\n")
  
  ejemplo_salida <- codigos_tema$codigo[1] # usar el primer código como ejemplo
  
  system_prompt <- paste(
    paste0("Developer: Eres un clasificador de respuestas de encuestas. ",
           "El contexto de análisis es el siguiente:",
           "PREGUNTA CERRADA: ", config$contexto_cerrada, "\n\n",
            "Respuesta seleccionada: ", categoria_cerrada, "\n\n",
           "PREGUNTA ABIERTA: ", config$contexto_abierta, "\n\n",
           "Tu tarea es clasificar las respuestas a la pregunta abierta, sabiendo que la persona ",
           "eligió '", categoria_cerrada, "' en la pregunta cerrada.", "\n\n",
           "Comienza con una breve lista de verificación conceptual para asegurarte de que sigues ",
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
    paste0('{"', config$campo_salida, '": "', ejemplo_salida, '"}', "\n\n"),
    sep = "\n\n"
  )
  
  return(system_prompt)
}

# Construcción del prompt específico para cada respuesta de la pregunta cerrada
temas <- names(codigos_tema)
cat("Temas encontrados en", config$pregunta_cerrada, ":", paste(temas, collapse = ", "), "\n\n")

system_prompt <- map(temas, ~prompt_maker(.x, codigos_tema[[.x]])) |> 
  setNames(temas)

walk(system_prompt, cat)

# ----
# hacer lista de chats. 
## Ejemplo de uso manual: chats[["Empleo"]]$chat("ayer me quedé sin empleo")
chats <- purrr::imap(system_prompt, function(prompt, tema) {
  chat_openai(
    model = "gpt-5-nano",
    base_url = "https://api.openai.com/v1",
    api_key = Sys.getenv("OPENAI_API_KEY"),
    system_prompt = prompt
  )
})

#chequear que los nombres se hayan asignado correctamente
cat("Se crearon los chats con los siguientes nombres:", paste(names(chats), collapse  = ", "), "\n\n")


# Funcion para clasificar Texto
clasificar_texto <- possibly(
  function(texto, cerrada){

    chat <- chats[[cerrada]]
    categorias <- codigos_tema[[cerrada]]$codigo
    
    resultado <- chat$chat_structured(
      texto,
      type = type_object(codigo = type_enum(values = categorias))
    )
    return(resultado$codigo)
  },
  otherwise = NA_character_
)

# Procesamiento por bloques
bloques <- split(raw, (seq_len(nrow(raw)) - 1L) %/% config$chunk_size)

clasificacion_results <- map_dfr(bloques, function(bloque) {
  bloque |> 
    mutate(
      codigo_temp = map2_chr(
        .data[[config$pregunta_abierta]],
        .data[[config$pregunta_cerrada]],
        clasificar_texto, 
        .progress = TRUE
      )) |> 
    select(all_of(names(raw)), codigo_temp)
}) |> 
  rename(!!paste0(config$campo_salida, "_", config$pregunta_abierta) := codigo_temp)



# guardar y resumen
outfile <- paste0(config$base_path, "/analysis/", config$year, "/", config$round, "/", config$tema, "_", config$pregunta_abierta, ".csv")
write_csv(clasificacion_results, outfile)

final_col <- paste0(config$campo_salida, "_", config$pregunta_abierta)
cat("\n=== RESUMEN FINAL ===\n","distribución de códigos (", final_col, "):\n", sep = "")
print(table(clasificacion_results[[final_col]], useNA = "ifany"))
cat("archivo guardado en: ", outfile, "\n", sep = "")








