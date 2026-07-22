library(readr)
library(dplyr)
library(ggplot2)
library(sysfonts)
library(showtext)

year <- 2026
round_id <- "R7"
input_file <- paste0(
  "data/processed/transcriptions/output/", year,
  "/transcripcion_", round_id, ".csv"
)
output_dir <- paste0("plots/", year, "/", round_id)
question_title <- "¿Está usted o algún familiar directo vinculado a la educación pública?\n(Por ejemplo: hijo/a, nieto/a, sobrino/a, padre, madre, Hermano/a, etc.)"

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

q10_labels <- c(
  "A" = "Sí, asistiendo",
  "B" = "Sí, como docente o funcionario",
  "C" = "Sí, ambos",
  "D" = "No"
)

q10_palette <- c(
  "Sí, asistiendo" = "#A7BF67",
  "Sí, como docente o funcionario" = "#7EA347",
  "Sí, ambos" = "#4C7A2E",
  "No" = "#E7B8AF"
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

df <- read_csv(input_file, show_col_types = FALSE) |>
  mutate(
    q10_rec = recode(q10, !!!q10_labels)
  )

plot_df <- build_grouped_data(df, "q10_rec")
cat_order <- names(q10_palette)

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
  scale_fill_manual(values = q10_palette, drop = FALSE) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
  labs(
    title = question_title,
    subtitle = "Vinculación con la educación pública por grupo"
  ) +
  coord_cartesian(clip = "off")

ggsave(
  filename = file.path(output_dir, "R7_q10_vinculo_stack_abs.png"),
  plot = p,
  width = 13.5,
  height = 7.5,
  dpi = 320
)
