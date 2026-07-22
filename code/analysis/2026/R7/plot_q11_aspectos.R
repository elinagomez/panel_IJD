library(readr)
library(dplyr)
library(forcats)
library(ggplot2)
library(sysfonts)
library(showtext)

year <- 2026
round_id <- "R7"
input_file <- paste0(
  "data/processed/analysis/", year, "/", round_id,
  "/mejoras_educacion_publica_q11.csv"
)
output_dir <- paste0("plots/", year, "/", round_id)
question_title <- "¿Qué aspectos cree que deberían cambiarse o mejorarse en la educación\npública?"

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

q11_palette <- c(
  "docentes_gestion_gobernanza" = "#4C7A2E",
  "presupuesto_infraestructura_recursos" = "#668E38",
  "cobertura_tiempo_plantel" = "#7EA347",
  "contenidos_curricula_metodologias" = "#A7BF67",
  "calidad_exigencia_aprendizajes" = "#C7D990",
  "inclusion_apoyos_bienestar" = "#F2DEC1",
  "disciplina_convivencia_familias" = "#E7B8AF",
  "cambio_integral_del_sistema" = "#D99694",
  "sin_cambios_o_ns_nr" = "#D9D9D9"
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
    filter(
      !is.na(categoria),
      categoria != "",
      categoria != "NA"
    ) |>
    mutate(grupo = factor(grupo, levels = group_order))
}

df <- read_csv(input_file, show_col_types = FALSE)

plot_df <- build_grouped_data(df, "codigo_q11")
cat_order <- names(q11_palette)

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
  scale_fill_manual(values = q11_palette, drop = FALSE) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
  labs(
    title = question_title,
    subtitle = "Aspectos de mejora por grupo"
  ) +
  coord_cartesian(clip = "off")

ggsave(
  filename = file.path(output_dir, "R7_aspectos_q11_stack_abs.png"),
  plot = p,
  width = 13.5,
  height = 7.5,
  dpi = 320
)
