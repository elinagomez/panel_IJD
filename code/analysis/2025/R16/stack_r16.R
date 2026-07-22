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
round_id <- "R16"


pal_base <- c(
  "#4E79A7", "#59A14F", "#E15759", "#76B7B2", "#9C755F",
  "#B07AA1", "#F28E2B", "#EDC948", "#FF9DA7", "#BAB0AC",
  "#1F77B4", "#2CA02C", "#D62728", "#17BECF", "#8C564B",
  "#9467BD", "#BCBD22", "#7F7F7F", "#AEC7E8", "#98DF8A"
)

# Tipografía Montserrat con fallback sin red
base_font <- "montserrat"
try({
  font_add_google("Montserrat", "montserrat")
  showtext_auto()
}, silent = TRUE)
if (!("montserrat" %in% sysfonts::font_families())) {
  base_font <- "sans"
}

theme_set(
  theme_minimal(base_family = base_font) +
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
  q1 = "¿Cómo calificaría en general la situación económica actual del país? Diría Ud. que es…",
  q4 = "Si comparas el actual gobierno (Yamandú Orsi) con el gobierno pasado (Lacalle Pou)\n¿Percibes alguna diferencia entre los gobiernos?",
  q6 = "En lo que va del año, te parece que la inseguridad en general ha…"
)

# Definir el orden correcto de las categorías para cada pregunta cerrada
categoria_orden <- list(
  q1 = c("Muy Buena", "Buena", "Regular", "Mala", "Muy Mala"),
  q4 = c("Sí", "No"),
  q6 = c("Aumentado mucho", "Aumentado algo", "Permanecido igual", "Disminuido algo")
)

# Paletas específicas por pregunta para asegurar correspondencia color-opción
paletas_pregunta <- list(
  q1 = c(
    "Muy Buena" = "#59A14F",
    "Buena" = "#98DF8A",
    "Regular" = "#EDC948",
    "Mala" = "#E15759",
    "Muy Mala" = "#D62728"
  ),
  q4 = c(
    "Sí" = "#59A14F",
    "No" = "#E15759",
    "No sé" = "#EDC948"
  ),
  q6 = c(
    "Aumentado mucho" = "#59A14F",
    "Aumentado algo" = "#98DF8A",
    "Permanecido igual" = "#EDC948",
    "Disminuido algo" = "#F28E2B",
    "Disminuido mucho" = "#D62728"
  )
)

# Helper para limpiar respuestas cerradas
normalize_closed <- function(x) {
  str_trim(str_to_lower(x))
}

# Datos
df <- read_csv(paste0("data/processed/transcriptions/output/", year, "/transcripcion_", round_id, ".csv")) |> 
  mutate(
    q1 = case_when(
      is.na(q1) ~ NA_character_,
      str_detect(normalize_closed(q1), "muy buena") ~ "Muy Buena",
      str_detect(normalize_closed(q1), "^buena$") ~ "Buena",
      str_detect(normalize_closed(q1), "^regular$") ~ "Regular",
      str_detect(normalize_closed(q1), "^mala$") ~ "Mala",
      str_detect(normalize_closed(q1), "muy mala") ~ "Muy Mala",
      TRUE ~ q1
    ),
    q4 = case_when(
      is.na(q4) ~ NA_character_,
      str_detect(normalize_closed(q4), "^si$|^sí$") ~ "Sí",
      str_detect(normalize_closed(q4), "^no$") ~ "No",
      str_detect(normalize_closed(q4), "^no$") ~ "No sé",
      TRUE ~ q4
    ),
    q6 = case_when(
      is.na(q6) ~ NA_character_,
      str_detect(normalize_closed(q6), "aumentado mucho") ~ "Aumentado mucho",
      str_detect(normalize_closed(q6), "aumentado") ~ "Aumentado algo",
      str_detect(normalize_closed(q6), "disminuido mucho") ~ "Disminuido mucho",
      str_detect(normalize_closed(q6), "disminuido") ~ "Disminuido algo",
      str_detect(normalize_closed(q6), "permanecido igual") ~ "Permanecido igual",
      TRUE ~ q6
    )
  )

# ORDENAR CATEGORÍAS DESPUÉS DE LEER LOS DATOS
df <- df %>%
  mutate(
    q1 = factor(q1, levels = categoria_orden$q1),
    q4 = factor(q4, levels = categoria_orden$q4),
    q6 = factor(q6, levels = categoria_orden$q6)
  )

# Carpeta de salida
dir.create(paste0("plots/", year, "/", round_id, ""), recursive = TRUE, showWarnings = FALSE)

# Helper para apiladas por variable de agrupación (segmento o etiqueta)
stacked_plot <- function(data, q, group_var, file_stub) {
  # Trabajamos con la columna de la pregunta cerrada (q1, q4 o q6)
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
    escala_tipo <- categoria_orden[[q]]
    if (!is.null(escala_tipo)) {
      cat_levels <- escala_tipo
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
  if (!is.null(paletas_pregunta[[q]])) {
    pal_vals <- paletas_pregunta[[q]][levs]
    if (any(is.na(pal_vals))) {
      faltantes <- which(is.na(pal_vals))
      pal_vals[faltantes] <- pal_base[seq_len(length(faltantes))]
    }
    pal_vals <- setNames(pal_vals, levs)
  } else {
    n_cols <- min(length(levs), length(pal_base))
    pal_vals <- setNames(pal_base[seq_len(n_cols)], levs[seq_len(n_cols)])
  }

  # Título desde el objeto de preguntas; fallback al nombre de la columna
  titulo <- if (!is.null(q_titles[[q]])) q_titles[[q]] else q

  p <- ggplot(counts, aes(x = .data[[group_var]], y = n, fill = categoria)) +
    geom_col() +
    scale_fill_manual(values = pal_vals, limits = levs, drop = FALSE) +
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
preguntas <- c("q1", "q4", "q6")

walk(preguntas, ~ stacked_plot(df, .x, "segmento", "R16_stack_segmento"))
walk(preguntas, ~ stacked_plot(df, .x, "etiqueta", "R16_stack_etiqueta"))
