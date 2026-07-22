library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(showtext)
library(sysfonts)

year <- 2026
round_id <- "R6"
pregunta <- "q8"

input_file <- file.path(
  "data", "processed", "transcriptions", "output", year,
  paste0("transcripcion_", round_id, ".csv")
)

output_dir <- file.path("plots", year, round_id)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

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

q8_levels <- c(
  "Muy en desacuerdo",
  "En desacuerdo",
  "Ni uno, ni otro",
  "De acuerdo",
  "Muy de acuerdo"
)

q8_labels <- c(
  "Muy en desacuerdo" = "Muy en desacuerdo",
  "En desacuerdo" = "En desacuerdo",
  "Ni uno, ni otro" = "Ni uno, ni otro",
  "De acuerdo" = "De acuerdo",
  "Muy de acuerdo" = "Muy de acuerdo"
)

q8_palette <- c(
  "Muy en desacuerdo" = "#C47C7A",
  "En desacuerdo" = "#E7B8AF",
  "Ni uno, ni otro" = "#F2DEC1",
  "De acuerdo" = "#A7BF67",
  "Muy de acuerdo" = "#4C7A2E"
)

q8_title <- "Plan de seguridad a diez años, trascendiendo este gobierno"

theme_set(
  theme_minimal(base_family = base_font) +
    theme(
      text = element_text(family = base_font, face = "bold", colour = "black"),
      plot.title = element_text(family = base_font, face = "bold", size = 22, colour = "black"),
      axis.title = element_text(family = base_font, face = "bold", size = 18, colour = "black"),
      axis.text.x = element_text(family = base_font, face = "bold", size = 15, angle = 0, hjust = 0.5, colour = "black"),
      axis.text.y = element_text(family = base_font, face = "bold", size = 15, colour = "black"),
      legend.title = element_text(family = base_font, face = "bold", size = 15, colour = "black"),
      legend.text = element_text(family = base_font, face = "bold", size = 13, colour = "black"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = margin(12, 12, 12, 12)
    )
)

df <- read_csv(input_file, show_col_types = FALSE) |>
  mutate(
    q8 = if_else(q8 == "Ni uno, ni otro.", "Ni uno, ni otro", q8)
  )

plot_q8 <- function(data) {
  grupo_levels <- c(
    "Canelones",
    "Interior Coalición",
    "Interior Frente Amplio",
    "Montevideo",
    "CM",
    "FA"
  )

  counts_segmento <- data |>
    filter(!is.na(segmento), !is.na(q8)) |>
    count(segmento, q8, name = "n") |>
    transmute(grupo = segmento, respuesta = q8, n = n)

  counts_voto <- data |>
    filter(!is.na(voto2), !is.na(q8)) |>
    count(voto2, q8, name = "n") |>
    transmute(grupo = voto2, respuesta = q8, n = n)

  counts <- bind_rows(counts_segmento, counts_voto) |>
    filter(respuesta %in% q8_levels) |>
    mutate(
      respuesta = factor(respuesta, levels = q8_levels, labels = unname(q8_labels)),
      grupo = factor(grupo, levels = grupo_levels)
    ) |>
    complete(grupo, respuesta, fill = list(n = 0)) |>
    group_by(grupo) |>
    mutate(p = n / sum(n)) |>
    ungroup()

  p <- ggplot(counts, aes(x = grupo, y = p, fill = respuesta)) +
    geom_col(width = 0.62, color = "white", linewidth = 0.3) +
    geom_text(
      data = counts |> filter(n > 0),
      aes(label = n),
      position = position_stack(vjust = 0.5),
      size = 4.8,
      family = base_font,
      fontface = "bold",
      color = "black"
    ) +
    scale_fill_manual(
      values = q8_palette,
      guide = guide_legend(reverse = TRUE)
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.02)),
      breaks = seq(0, 1, by = 0.2)
    ) +
    labs(
      title = q8_title,
      x = NULL,
      y = NULL,
      fill = NULL
    ) +
    theme(
      legend.position = "right",
      plot.title.position = "plot",
      axis.text.x = element_text(
        family = base_font,
        face = "bold",
        size = 14,
        angle = 18,
        hjust = 1,
        colour = "black"
      ),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "#D0D0D0", linewidth = 0.7),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    )

  ggsave(
    filename = file.path(output_dir, paste0(round_id, "_", pregunta, "_stack_abs.png")),
    plot = p,
    width = 14,
    height = 8.5,
    dpi = 300,
    bg = "white"
  )
}

plot_q8(df)

cat("Gráficos guardados en:", output_dir, "\n")
