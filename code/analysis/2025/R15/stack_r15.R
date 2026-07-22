# Paquetes
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(ggplot2)
library(sysfonts)
library(showtext)
library(purrr)
year <- 2025
round_id <- "R15"


pal_base <- c(
"#D62728",
"#EDC948",
"#59A14F"
)

# Tipografía Montserrat
font_add_google("Montserrat", "montserrat")
showtext_auto()

theme_set(
  theme_minimal(base_family = "montserrat") +
    theme(
      plot.title = element_text(face = "bold", size = 40),
      axis.text.x = element_text(angle = 20, hjust = 1, size = 30),
      axis.text.y = element_text(size = 30),
      axis.title = element_text(size = 35),
      legend.title = element_text(size = 35),
      legend.text = element_text(size = 30)
    )
)

# Títulos de preguntas (usados como título del gráfico)
q_titles <- c(
  q1 = "La semana pasada un padre de 28 años, Andrés Morosini, asesinó a sus 2 hijos y se suicidó\nhundiendo el auto donde viajaban en el arroyo Don Esteban, cerca de la ciudad de Young.\n¿Te has informado sobre el caso?",
  q2 = "Hay quienes dicen que los casos como el de Morosini son tragedias inevitables,\nmientras que otros afirman que hay responsables ¿Cuál es tu opinión? Se trata de…",
  q6 = "¿Dirías que hay beneficiados por la ley de presupuesto?",
  q8 = "¿Dirías que hay perjudicados por la ley de presupuesto?",
  q10 = "Si tienes en cuenta las acciones del gobierno nacional que tu conoces ¿Consideras que la frase\n\"Uruguay, un país para crecer\" refleja la orientación que tienen las acciones del gobierno?"
)

# Definir el orden correcto de las categorías para cada pregunta
categoria_orden <- list(
  # Para pregunta q1 (escala de información sobre el caso)
  informacion_caso = c("Nada", "Algo", "Mucho"),
  
  # Para pregunta q2 (opinión sobre responsabilidad)
  responsabilidad = c("Tragedia inevitable", "Hay responsables"),
  
  # Para preguntas q6 y q8 (beneficiados/perjudicados)
  si_no_nosabe = c("No", "No sé", "Sí"),
  
  # Para pregunta q10 (reflejo de orientación gobierno)
  reflejo_gobierno = c("Para nada", "Algo", "Refleja totalmente")
)

# Mapeo de preguntas a tipo de escala
pregunta_escala <- list(
  q1 = "informacion_caso",
  q2 = "responsabilidad", 
  q6 = "si_no_nosabe",
  q8 = "si_no_nosabe",
  q10 = "reflejo_gobierno"
)

# Datos
df <- read_csv(paste0("data/processed/transcriptions/output/", year, "/transcripcion_", round_id, ".csv")) |> 
  mutate(q7 = replace_na(q7, "Ni uno, ni otro")) |> 
  mutate(q7 = if_else(q7 == "Ni uno, ni otro.", "Ni uno, ni otro", q7))

# ORDENAR CATEGORÍAS DESPUÉS DE LEER LOS DATOS
df <- df %>%
  mutate(
    # Ordenar q1 (información sobre el caso)
    q1 = factor(q1, levels = categoria_orden$informacion_caso),
    
    # Ordenar q2 (responsabilidad)  
    q2 = factor(q2, levels = categoria_orden$responsabilidad),
    
    # Ordenar q6 y q8 (sí/no/no sé)
    q6 = factor(q6, levels = categoria_orden$si_no_nosabe),
    q8 = factor(q8, levels = categoria_orden$si_no_nosabe),
    
    # Ordenar q10 (reflejo gobierno)
    q10 = factor(q10, levels = categoria_orden$reflejo_gobierno)
  )

# Carpeta de salida
# dir.create(paste0("plots/", year, "/", round_id, ""), recursive = TRUE, showWarnings = FALSE)

# Helper para apiladas por variable de agrupación (segmento o etiqueta)
stacked_plot <- function(data, q, group_var, file_stub) {
  # Ahora trabajamos con una sola columna por pregunta (q1, q3, q5, ...)
  if (!q %in% names(data)) return(invisible(NULL))
  if (!group_var %in% names(data)) return(invisible(NULL))

  long <- data |>
    select(all_of(c(group_var, q))) |>
    rename(categoria = all_of(q)) |>
    filter(!is.na(categoria), !is.na(.data[[group_var]]))

  if (nrow(long) == 0) return(invisible(NULL))

  # Mantener el orden de factores ya definido para las categorías
  # Si la variable ya es factor, mantener sus niveles
  if (is.factor(long$categoria)) {
    cat_levels <- levels(long$categoria)
  } else {
    # Si no es factor, usar el orden definido para esta pregunta
    escala_tipo <- pregunta_escala[[q]]
    if (!is.null(escala_tipo)) {
      cat_levels <- categoria_orden[[escala_tipo]]
    } else {
      # Fallback: orden por frecuencia
      cat_levels <- long |>
        count(categoria, name = "tot") |>
        arrange(desc(tot)) |>
        pull(categoria)
    }
  }

  # Orden de grupos por total
  grp_order <- long |>
    count(.data[[group_var]], name = "tot") |>
    arrange(desc(tot)) |>
    pull(1)

  # Conteos absolutos y factores ordenados
  counts <- long |>
    count(.data[[group_var]], categoria, name = "n") |>
    mutate(
      categoria = factor(categoria, levels = cat_levels)
    )
  
  # Factorizar el grupo
  counts[[group_var]] <- factor(counts[[group_var]], levels = grp_order)

  # Paleta mapeada a niveles presentes
  levs <- levels(counts$categoria)
  pal_vals <- pal_base[seq_len(min(length(levs), length(pal_base)))]
  names(pal_vals) <- levs[seq_along(pal_vals)]

  # Título desde el objeto de preguntas; fallback al nombre de la columna
  titulo <- if (!is.null(q_titles[[q]])) q_titles[[q]] else q

  p <- ggplot(counts, aes(x = .data[[group_var]], y = n, fill = categoria)) +
    geom_col() +
    scale_fill_manual(values = pal_vals, drop = FALSE) +
    labs(x = str_to_title(group_var), y = "Frecuencia", title = titulo, fill = "Categoría") +
    guides(fill = guide_legend(reverse = FALSE)) +
    theme(plot.title = element_text(face = "bold", lineheight = 0.25),
          legend.position = "right")

  ggsave(
    filename = paste0("plots/", year, "/", round_id, "/", file_stub, "_", q, ".png"),
    plot = p, width = 12, height = 7, dpi = 300
  )
}

# Loop de preguntas y salida
preguntas <- c("q1", "q2", "q6", "q8", "q10")

walk(preguntas, ~ stacked_plot(df, .x, "segmento", "R15_stack_segmento"))
walk(preguntas, ~ stacked_plot(df, .x, "etiqueta",  "R15_stack_etiqueta"))
