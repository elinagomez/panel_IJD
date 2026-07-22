# Paquetes
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(ggplot2)
library(sysfonts)
library(showtext)
library(purrr)
year <- 2025
round_id <- "R11"


# Paleta requerida
pal_base <- c(
  "#4E79A7", "#59A14F", "#E15759", "#76B7B2", "#9C755F",
  "#B07AA1", "#F28E2B", "#EDC948", "#FF9DA7", "#BAB0AC",
  "#1F77B4", "#2CA02C", "#D62728", "#17BECF", "#8C564B",
  "#9467BD", "#BCBD22", "#7F7F7F", "#AEC7E8", "#98DF8A"
)

# Tipografía Montserrat
font_add_google("Montserrat", "montserrat")
showtext_auto()

theme_set(
  theme_minimal(base_family = "montserrat") +
    theme(
      plot.title = element_text(face = "bold", size = 60),
      axis.text.x = element_text(angle = 20, hjust = 1, size = 30),
      axis.text.y = element_text(size = 30),
      axis.title = element_text(size = 35),
      legend.title = element_text(size = 35),
      legend.text = element_text(size = 30)
    )
)

# Datos
df <- readxl::read_excel(paste0("data/processed/analysis/", year, "/", round_id, "/R11_etiquetada.xlsx"))

# Carpeta de salida
# dir.create(paste0("plots/", year, "/", round_id, ""), recursive = TRUE, showWarnings = FALSE)

# Helper para apiladas por variable de agrupación (segmento o etiqueta)
stacked_plot <- function(data, q, group_var, file_stub) {
  # Selección de columnas qX_1..qX_5 (flexible si vienen más: toma primeras 5)
  q_cols <- names(data)[str_detect(names(data), paste0("^", q, "_\\d+$"))]
  q_cols <- q_cols[order(readr::parse_number(q_cols))]
  if (length(q_cols) == 0) return(invisible(NULL))
  q_cols <- head(q_cols, 5)

  if (!all(c(group_var) %in% names(data))) return(invisible(NULL))

  long <- data |>
    select(all_of(c(group_var, q_cols))) |>
    pivot_longer(cols = all_of(q_cols), names_to = "pos", values_to = "categoria") |>
    mutate(
      categoria = as.character(categoria),
      categoria = str_squish(categoria)
    ) |>
    filter(!is.na(categoria), categoria != "", !is.na(.data[[group_var]]))

  if (nrow(long) == 0) return(invisible(NULL))

  # Orden de categorías por frecuencia total en la pregunta
  cat_order <- long |>
    count(categoria, name = "tot") |>
    arrange(desc(tot)) |>
    pull(categoria)

  # Orden de grupos por total
  grp_order <- long |>
    count(.data[[group_var]], name = "tot") |>
    arrange(desc(tot)) |>
    pull(1)

  # Conteos absolutos y factores ordenados
  counts <- long |>
    count(.data[[group_var]], categoria, name = "n") |>
    mutate(
      categoria = factor(categoria, levels = cat_order),
      {{ group_var }} := factor(.data[[group_var]], levels = grp_order)
    )

  # Paleta mapeada a niveles presentes
  levs <- levels(counts$categoria)
  pal_vals <- pal_base[seq_len(min(length(levs), length(pal_base)))]
  names(pal_vals) <- levs[seq_along(pal_vals)]

  titulo <- paste0(q, ", categorías por ", group_var)

  p <- ggplot(counts, aes(x = .data[[group_var]], y = n, fill = categoria)) +
    geom_col() +
    scale_fill_manual(values = pal_vals, drop = FALSE) +
    labs(x = str_to_title(group_var), y = "Frecuencia", title = titulo, fill = "Categoría") +
    guides(fill = guide_legend(reverse = FALSE)) +
    theme(legend.position = "right")

  ggsave(
    filename = paste0("plots/", year, "/", round_id, "/", file_stub, "_", q, ".png"),
    plot = p, width = 12, height = 7, dpi = 300
  )
}

# Loop de preguntas y salida
preguntas <- paste0("q", 1:9)

walk(preguntas, ~ stacked_plot(df, .x, "segmento", "R11_stack_segmento"))
walk(preguntas, ~ stacked_plot(df, .x, "etiqueta",  "R11_stack_etiqueta"))
