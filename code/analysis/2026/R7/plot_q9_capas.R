library(readr)
library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(sysfonts)
library(showtext)

year <- 2026
round_id <- "R7"
input_file <- paste0("data/processed/analysis/", year, "/", round_id, "/educacion_publica_q9.csv")
output_dir <- paste0("plots/", year, "/", round_id)
question_title <- "¿Qué opinión tiene sobre la educación pública\nen nuestro país?, ¿qué sentimientos o valoraciones le despierta?"

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

postura_palette <- c(
  "defensa_entusiasta" = "#4C7A2E",
  "apoyo_con_matices" = "#A7BF67",
  "orgullo_historico_y_preocupacion" = "#F2DEC1",
  "critica_moderada" = "#E7B8AF",
  "critica_severa" = "#C47C7A",
  "ns_nr_sin_contacto" = "#D9D9D9"
)

fundamento_palette <- c(
  "valor_publico_igualdad" = "#4C7A2E",
  "calidad_exigencia_aprendizajes" = "#668E38",
  "docentes_y_gestion" = "#7EA347",
  "recursos_infraestructura_presupuesto" = "#A7BF67",
  "valores_disciplina_y_familias" = "#F2DEC1",
  "inclusion_y_apoyos" = "#D99694",
  "sin_fundamento_especifico" = "#D9D9D9"
)

registrar_montserrat <- function() {
  try({
    sysfonts::font_add_google(
      name = "Montserrat",
      family = "montserrat_google_bold",
      regular.wt = 500,
      bold.wt = 700
    )
    return("montserrat_google_bold")
  }, silent = TRUE)

  ff <- sysfonts::font_files()
  regular_hit <- ff[
    grepl("^Montserrat\\.ttf$", ff$file, ignore.case = TRUE),
  ]
  bold_hit <- ff[
    grepl("Montserrat-VariableFont_wght\\.ttf$", ff$file, ignore.case = TRUE),
  ]

  if (nrow(bold_hit) > 0) {
    bold_path <- file.path(bold_hit$path[1], bold_hit$file[1])
    sysfonts::font_add("montserrat_google_bold", regular = bold_path)
    return("montserrat_google_bold")
  }

  if (nrow(regular_hit) > 0) {
    regular_path <- file.path(regular_hit$path[1], regular_hit$file[1])
    sysfonts::font_add("montserrat_google_bold", regular = regular_path)
    return("montserrat_google_bold")
  }

  "sans"
}

base_font <- registrar_montserrat()
showtext_auto()
showtext_opts(dpi = 300)

theme_set(
  theme_minimal(base_family = base_font) +
    theme(
      plot.title = element_text(face = "bold", size = 24, hjust = 0),
      plot.subtitle = element_text(size = 14, face = "bold", hjust = 0),
      axis.text.x = element_text(size = 12, face = "bold", angle = 18, hjust = 1),
      axis.text.y = element_blank(),
      axis.title = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.title = element_blank(),
      legend.text = element_text(size = 11),
      legend.position = "right"
    )
)

group_order <- c(
  "Canelones",
  "Interior Coalición",
  "Interior Frente Amplio",
  "Montevideo",
  "CM",
  "FA"
)

build_grouped_data <- function(df, value_col) {
  segmentos <- df |>
    filter(segmento %in% group_order[1:4]) |>
    transmute(grupo = segmento, categoria = .data[[value_col]])

  bloques_voto <- df |>
    filter(voto2 %in% group_order[5:6]) |>
    transmute(grupo = voto2, categoria = .data[[value_col]])

  bind_rows(segmentos, bloques_voto) |>
    filter(!is.na(categoria), categoria != "") |>
    mutate(grupo = factor(grupo, levels = group_order))
}

plot_layer <- function(df, value_col, subtitle, output_name) {
  plot_df <- build_grouped_data(df, value_col)

  palette_map <- if (value_col == "postura_q9") postura_palette else fundamento_palette
  cat_order <- names(palette_map)

  counts <- plot_df |>
    count(grupo, categoria, name = "n") |>
    group_by(grupo) |>
    mutate(prop = n / sum(n)) |>
    ungroup() |>
    mutate(categoria = factor(categoria, levels = cat_order))

  p <- ggplot(counts, aes(x = grupo, y = prop, fill = categoria)) +
    geom_col(width = 0.82, color = "white", linewidth = 0.3) +
    geom_text(
      aes(label = n),
      position = position_stack(vjust = 0.5),
      family = base_font,
      size = 3.5,
      color = "white",
      fontface = "bold",
      check_overlap = TRUE
    ) +
    scale_fill_manual(values = palette_map, drop = FALSE) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
    labs(title = question_title, subtitle = subtitle) +
    coord_cartesian(clip = "off")

  ggsave(
    filename = file.path(output_dir, output_name),
    plot = p,
    width = 13.5,
    height = 7.5,
    dpi = 320
  )
}

df <- read_csv(input_file, show_col_types = FALSE)

plot_layer(
  df = df,
  value_col = "postura_q9",
  subtitle = "Posturas por grupo",
  output_name = "R7_postura_q9_stack_abs.png"
)

plot_layer(
  df = df,
  value_col = "fundamento_q9",
  subtitle = "Fundamentos por grupo",
  output_name = "R7_fundamento_q9_stack_abs.png"
)
