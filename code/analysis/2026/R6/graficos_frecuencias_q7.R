library(readr)
library(dplyr)
library(tidyr)
library(forcats)
library(stringr)
library(ggplot2)
library(showtext)
library(sysfonts)

year <- 2026
round_id <- "R6"
pregunta <- "q7"

input_file <- file.path(
  "data", "processed", "analysis", year, round_id,
  "tecnologia_seguridad_q7.csv"
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

label_maps <- list(
  postura_q7 = c(
    apoyo_neto = "Apoyo neto",
    apoyo_condicionado_resguardos = "Apoyo condicionado",
    ambivalente_tradeoff = "Ambivalente",
    critica_riesgo_dominante = "Crítica por riesgo",
    escepticismo_inviabilidad = "Escepticismo",
    ns_nr_desinformado = "NS/NR"
  ),
  beneficio_q7 = c(
    eficacia_investigacion_prevencion = "Eficacia / prevención",
    celeridad_respuesta = "Celeridad",
    integracion_datos_diagnostico = "Integración de datos",
    control_trazabilidad_disuasiva = "Control / trazabilidad",
    sin_beneficio_explicito = "Sin beneficio"
  ),
  riesgo_q7 = c(
    privacidad_libertades_vigilancia = "Privacidad / libertades",
    seguridad_datos_filtracion_hackeo = "Filtración / hackeo",
    abuso_estatal_espionaje_desvio = "Abuso / espionaje",
    inoperancia_ineficacia = "Inoperancia",
    sin_riesgo_explicito = "Sin riesgo"
  ),
  codigo_q7 = c(
    apoyo_neto_seguridad_control = "Apoyo neto / control",
    apoyo_neto_integracion_datos = "Apoyo neto / datos",
    apoyo_condicionado_resguardos = "Apoyo condicionado",
    tradeoff_privacidad_libertades = "Trade-off / privacidad",
    tradeoff_gobernanza_datos = "Trade-off / gobernanza",
    critica_privacidad_libertades = "Crítica / privacidad",
    critica_datos_abuso = "Crítica / datos-abuso",
    escepticismo_inviabilidad = "Escepticismo",
    ns_nr_desinformado = "NS/NR",
    `__NA__` = "Sin derivación"
  )
)

palettes <- list(
  postura_q7 = c(
    "Apoyo neto" = "#4C7A2E",
    "Apoyo condicionado" = "#A7BF67",
    "Ambivalente" = "#F2DEC1",
    "Crítica por riesgo" = "#D99694",
    "Escepticismo" = "#B7726C",
    "NS/NR" = "#D9D9D9"
  ),
  beneficio_q7 = c(
    "Eficacia / prevención" = "#4C7A2E",
    "Celeridad" = "#7EA347",
    "Integración de datos" = "#A7BF67",
    "Control / trazabilidad" = "#F2DEC1",
    "Sin beneficio" = "#D99694"
  ),
  riesgo_q7 = c(
    "Privacidad / libertades" = "#4C7A2E",
    "Filtración / hackeo" = "#7EA347",
    "Abuso / espionaje" = "#A7BF67",
    "Inoperancia" = "#F2DEC1",
    "Sin riesgo" = "#D99694"
  ),
  codigo_q7 = c(
    "Apoyo neto / control" = "#4C7A2E",
    "Apoyo neto / datos" = "#668E38",
    "Apoyo condicionado" = "#A7BF67",
    "Trade-off / privacidad" = "#C7D990",
    "Trade-off / gobernanza" = "#F2DEC1",
    "Crítica / privacidad" = "#E7B8AF",
    "Crítica / datos-abuso" = "#D99694",
    "Escepticismo" = "#B7726C",
    "NS/NR" = "#D9D9D9",
    "Sin derivación" = "#BEBEBE"
  )
)

titles <- c(
  postura_q7 = "Balance general frente al uso intensivo de tecnología",
  beneficio_q7 = "¿Qué beneficios percibes en esta medida?",
  riesgo_q7 = "¿Qué riesgos percibes en esta medida?",
  codigo_q7 = "Síntesis de posiciones sobre beneficios y riesgos"
)

theme_set(
  theme_minimal(base_family = base_font) +
    theme(
      text = element_text(family = base_font, face = "bold", colour = "black"),
      plot.title = element_text(family = base_font, face = "bold", size = 30, colour = "black"),
      plot.subtitle = element_text(family = base_font, face = "bold", size = 16, colour = "black"),
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
    codigo_q7 = if_else(is.na(codigo_q7) | codigo_q7 == "NA", "__NA__", codigo_q7)
  )

plot_abs <- function(data, code_var) {
  labels <- label_maps[[code_var]]
  code_levels <- names(labels)
  if (code_var == "codigo_q7") {
    code_levels <- setdiff(code_levels, "__NA__")
    labels <- labels[code_levels]
  }
  grupo_levels <- c(
    "Canelones",
    "Interior Coalición",
    "Interior Frente Amplio",
    "Montevideo",
    "CM",
    "FA"
  )

  counts_segmento <- data |>
    filter(!is.na(segmento), !is.na(.data[[code_var]])) |>
    count(segmento, .data[[code_var]], name = "n") |>
    transmute(grupo = segmento, codigo = .data[[code_var]], n = n)

  counts_voto <- data |>
    filter(!is.na(voto2), !is.na(.data[[code_var]])) |>
    count(voto2, .data[[code_var]], name = "n") |>
    transmute(grupo = voto2, codigo = .data[[code_var]], n = n)

  counts <- bind_rows(counts_segmento, counts_voto) |>
    filter(codigo %in% code_levels) |>
    mutate(
      codigo = factor(codigo, levels = code_levels, labels = unname(labels)),
      grupo = factor(grupo, levels = grupo_levels)
    ) |>
    complete(grupo, codigo, fill = list(n = 0)) |>
    group_by(grupo) |>
    mutate(p = n / sum(n)) |>
    ungroup()

  p <- ggplot(counts, aes(x = grupo, y = p, fill = codigo)) +
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
      values = palettes[[code_var]],
      guide = guide_legend(reverse = TRUE)
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.02)),
      breaks = seq(0, 1, by = 0.2)
    ) +
    labs(
      title = titles[[code_var]],
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

  file_stub <- paste0(round_id, "_", pregunta, "_", code_var, "_stack_abs.png")
  ggsave(
    filename = file.path(output_dir, file_stub),
    plot = p,
    width = 14,
    height = 8.5,
    dpi = 300,
    bg = "white"
  )
}

for (code_var in c("postura_q7", "beneficio_q7", "riesgo_q7", "codigo_q7")) {
  plot_abs(df, code_var)
}

cat("Gráficos guardados en:", output_dir, "\n")
