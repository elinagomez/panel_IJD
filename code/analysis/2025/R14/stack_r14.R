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
round_id <- "R14"


pal_base <- c(
"#D62728",
"#E15759",
"#EDC948",
"#98DF8A",
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
  q1 = "Según información proveniente de empresas que estudian opinión publica se señala\nque cada vez hay un mayor número de personas que expresan desinterés por la política.\n¿Cuán de acuerdo está con esta afirmación?",
  q3 = "El pasado domingo se presentó la ley de presupuesto nacional 2025-2029 que organiza\nla inversión y los gastos del estado para los próximos 5 años.\n¿Cuán informado sentís que estás sobre el contenido de la ley de presupuesto?",
  q5 = "Sobre el impuesto dirigido a gravar a las empresas multinacionales instaladas en el país. Ud. está...",
  q7 = "También se incluye una propuesta de gravar uruguayos con depósitos en el exterior. Ud. está...",
  q9 = "Otra propuesta da cuenta de gravar a las compras realizadas fuera del país de manera virtual\nal que se ha denominado \"impuesto Temu\". Ud. está..."
)

# Definir el orden correcto de las categorías para cada pregunta
categoria_orden <- list(
  # Para preguntas q1, q5, q7, q9 (escala de acuerdo)
  acuerdo = c("Muy en desacuerdo", "En desacuerdo", "Ni uno, ni otro", 
              "De acuerdo", "Muy de acuerdo"),
  
  # Para pregunta q3 (escala de información)
  informacion = c("Nada informado", "Poco informado", "Ni uno, ni otro", 
                  "Informado", "Muy informado")
)
categoria_orden <- rev(categoria_orden)
# Mapeo de preguntas a tipo de escala
pregunta_escala <- list(
  q1 = "acuerdo",
  q3 = "informacion", 
  q5 = "acuerdo",
  q7 = "acuerdo",
  q9 = "acuerdo"
)

# Datos
df <- read_csv(paste0("data/processed/transcriptions/output/", year, "/transcripcion_", round_id, ".csv")) |> 
  mutate(q7 = replace_na(q7, "Ni uno, ni otro")) |> 
  mutate(q7 = if_else(q7 == "Ni uno, ni otro.", "Ni uno, ni otro", q7))

# ORDENAR CATEGORÍAS DESPUÉS DE LEER LOS DATOS
df <- df %>%
  mutate(
    # Ordenar q1 (escala de acuerdo)
    q1 = factor(q1, levels = categoria_orden$acuerdo),
    
    # Ordenar q3 (escala de información)  
    q3 = factor(q3, levels = categoria_orden$informacion),
    
    # Ordenar q5, q7, q9 (escala de acuerdo)
    q5 = factor(q5, levels = categoria_orden$acuerdo),
    q7 = factor(q7, levels = categoria_orden$acuerdo),
    q9 = factor(q9, levels = categoria_orden$acuerdo)
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
preguntas <- c("q1", "q3", "q5", "q7", "q9")

walk(preguntas, ~ stacked_plot(df, .x, "segmento", "R14_stack_segmento"))
walk(preguntas, ~ stacked_plot(df, .x, "etiqueta",  "R14_stack_etiqueta"))
