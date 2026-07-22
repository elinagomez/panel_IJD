#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(forcats)
  library(ggplot2)
  library(patchwork)
  library(scales)
})

args <- commandArgs(trailingOnly = TRUE)

infile <- if (length(args) >= 1) {
  args[[1]]
} else {
  file.path("data", "processed", "matched", "r3_20260309.csv")
}

data_outdir <- if (length(args) >= 2) {
  args[[2]]
} else {
  file.path("data", "processed", "analysis", "2026", "R3")
}

plots_dir <- if (length(args) >= 3) {
  args[[3]]
} else {
  file.path("plots", "2026", "R3")
}

dir.create(data_outdir, recursive = TRUE, showWarnings = FALSE)
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)

df <- read.csv(infile, check.names = FALSE, stringsAsFactors = FALSE, na.strings = c("", "NA"))

df <- df |>
  mutate(across(where(is.character), trimws))

clean_cat <- function(x, var) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_

  if (identical(var, "n_educativo")) {
    x <- recode(
      x,
      "Eduación Media Básica" = "Educacion Media Basica",
      "Educación Media Básica" = "Educacion Media Basica",
      "Educación Media Superior" = "Educacion Media Superior",
      "Educación Media Basica" = "Educacion Media Basica"
    )
    x <- ifelse(is.na(x), "Sin dato", x)
    lvl <- c(
      "Primaria incompleta",
      "Primaria completa",
      "Educacion Media Basica",
      "Educacion Media Superior",
      "Terciario no universitario",
      "Magisterio o profesorado",
      "Universidad o posgrado",
      "Sin dato"
    )
    return(factor(x, levels = lvl))
  }

  if (identical(var, "edad")) {
    x <- ifelse(is.na(x), "Sin dato", x)
    lvl <- c("30 y menos", "31 a 59", "60 y mas", "60 y más", "Sin dato")
    # Normalizar a ASCII para etiquetas consistentes
    x <- recode(x, "60 y más" = "60 y mas")
    lvl <- c("30 y menos", "31 a 59", "60 y mas", "Sin dato")
    return(factor(x, levels = lvl))
  }

  if (identical(var, "genero")) {
    x <- recode(x, "Masculino" = "Hombre", "Femenino" = "Mujer")
    x <- ifelse(is.na(x), "Sin dato", x)
    lvl <- c("Mujer", "Hombre", "Otro", "Sin dato")
    return(factor(x, levels = lvl))
  }

  if (identical(var, "etiqueta")) {
    x <- ifelse(is.na(x), "Sin dato", x)
    return(factor(x))
  }

  if (identical(var, "departamento")) {
    x <- recode(x, "Tacurembó" = "Tacuarembo", "Tacuarembó" = "Tacuarembo")
    x <- ifelse(is.na(x), "Sin dato", x)
    return(factor(x))
  }

  x <- ifelse(is.na(x), "Sin dato", x)
  factor(x)
}

segmento_levels <- df |>
  mutate(segmento = clean_cat(segmento, "segmento")) |>
  count(segmento, sort = TRUE) |>
  pull(segmento) |>
  as.character()

df <- df |>
  mutate(segmento = factor(as.character(clean_cat(segmento, "segmento")), levels = segmento_levels))

theme_paneles <- function(base_size = 16) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = rel(1.15)),
      plot.subtitle = element_text(color = "#4D4D4D"),
      axis.title = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.title = element_blank(),
      legend.position = "bottom",
      plot.margin = margin(10, 12, 10, 12)
    )
}

palette_levels <- function(levels_vec, palette_name = "Dark 3") {
  lv <- levels_vec[!is.na(levels_vec)]
  setNames(grDevices::hcl.colors(length(lv), palette_name), lv)
}

count_overall <- function(data, var) {
  data |>
    transmute(cat = clean_cat(.data[[var]], var)) |>
    count(cat, sort = TRUE) |>
    filter(!is.na(cat)) |>
    mutate(
      pct = n / sum(n),
      label = paste0(n, " (", percent(pct, accuracy = 0.1), ")"),
      cat = forcats::fct_reorder(cat, n)
    )
}

plot_overall_bar <- function(data, var, title, fill_palette = "Dark 3") {
  tbl <- count_overall(data, var)
  cols <- palette_levels(levels(tbl$cat), fill_palette)

  ggplot(tbl, aes(x = cat, y = n, fill = cat)) +
    geom_col(width = 0.75, show.legend = FALSE) +
    geom_text(aes(label = label), hjust = -0.05, size = 4.2) +
    coord_flip(clip = "off") +
    scale_fill_manual(values = cols, drop = FALSE) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.20))) +
    labs(title = title, subtitle = "Distribucion total (n y %)") +
    theme_paneles()
}

plot_segment_fill <- function(data, var, title, fill_palette = "Dark 3") {
  tbl <- data |>
    transmute(
      segmento = factor(segmento, levels = segmento_levels),
      cat = clean_cat(.data[[var]], var)
    ) |>
    filter(!is.na(segmento), !is.na(cat)) |>
    count(segmento, cat) |>
    group_by(segmento) |>
    mutate(p = n / sum(n)) |>
    ungroup()

  cat_levels <- tbl |>
    group_by(cat) |>
    summarise(n = sum(n), .groups = "drop") |>
    arrange(desc(n)) |>
    pull(cat) |>
    as.character()
  tbl <- tbl |>
    mutate(cat = factor(as.character(cat), levels = cat_levels))

  cols <- palette_levels(levels(tbl$cat), fill_palette)

  ggplot(tbl, aes(x = segmento, y = p, fill = cat)) +
    geom_col(position = "fill", width = 0.8) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    scale_fill_manual(values = cols, drop = FALSE) +
    labs(title = title, subtitle = "Composicion porcentual dentro de cada segmento") +
    theme_paneles() +
    theme(
      axis.text.x = element_text(angle = 18, hjust = 1),
      panel.grid.major.y = element_line(color = "#E5E5E5"),
      panel.grid.major.x = element_blank()
    )
}

save_combo_plot <- function(data, var, title_left, title_right, outfile, palette_name = "Dark 3") {
  p1 <- plot_overall_bar(data, var, title_left, fill_palette = palette_name)
  p2 <- plot_segment_fill(data, var, title_right, fill_palette = palette_name)

  combo <- p1 | p2
  ggsave(
    filename = outfile,
    plot = combo,
    width = 16,
    height = 7.5,
    dpi = 220,
    bg = "white"
  )
}

plot_segmento <- function(data, outfile) {
  tbl <- data |>
    transmute(segmento = factor(segmento, levels = segmento_levels)) |>
    count(segmento, sort = TRUE) |>
    mutate(
      pct = n / sum(n),
      label = paste0(n, " (", percent(pct, accuracy = 0.1), ")"),
      segmento = forcats::fct_reorder(segmento, n)
    )

  cols <- palette_levels(levels(tbl$segmento), "Dark 3")

  p <- ggplot(tbl, aes(x = segmento, y = n, fill = segmento)) +
    geom_col(width = 0.75, show.legend = FALSE) +
    geom_text(aes(label = label), hjust = -0.05, size = 4.8) +
    coord_flip(clip = "off") +
    scale_fill_manual(values = cols, drop = FALSE) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
    labs(
      title = "Composicion del panel por segmento",
      subtitle = paste0("R3 2026 | n = ", nrow(data))
    ) +
    theme_paneles(base_size = 18)

  ggsave(outfile, p, width = 13.5, height = 7.2, dpi = 220, bg = "white")
}

plot_complementarias <- function(data, outfile) {
  dep <- data |>
    transmute(cat = as.character(clean_cat(departamento, "departamento"))) |>
    count(cat, sort = TRUE)

  if (nrow(dep) > 10) {
    dep <- dep |>
      mutate(cat = ifelse(row_number() <= 10, cat, "Otros")) |>
      group_by(cat) |>
      summarise(n = sum(n), .groups = "drop") |>
      arrange(n)
  } else {
    dep <- dep |>
      arrange(n)
  }
  dep <- dep |>
    mutate(
      pct = n / sum(n),
      label = paste0(n, " (", percent(pct, accuracy = 0.1), ")"),
      cat = factor(cat, levels = cat)
    )

  p_dep <- ggplot(dep, aes(x = cat, y = n, fill = cat)) +
    geom_col(show.legend = FALSE) +
    geom_text(aes(label = label), hjust = -0.05, size = 3.6) +
    coord_flip(clip = "off") +
    scale_fill_manual(values = palette_levels(levels(dep$cat), "Set 3"), drop = FALSE) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
    labs(title = "Departamento (top 10 + Otros)", subtitle = "Distribucion total") +
    theme_paneles(base_size = 14)

  voto <- count_overall(data, "voto")
  p_voto <- ggplot(voto, aes(x = cat, y = n, fill = cat)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    scale_fill_manual(values = palette_levels(levels(voto$cat), "Dark 2"), drop = FALSE) +
    labs(title = "Voto", subtitle = "Distribucion total") +
    theme_paneles(base_size = 13) +
    theme(plot.margin = margin(10, 6, 10, 12))

  etiqueta <- count_overall(data, "etiqueta")
  p_etiqueta <- ggplot(etiqueta, aes(x = cat, y = n, fill = cat)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    scale_fill_manual(values = palette_levels(levels(etiqueta$cat), "Temps"), drop = FALSE) +
    labs(title = "Etiqueta", subtitle = "Incluye Sin dato") +
    theme_paneles(base_size = 13) +
    theme(plot.margin = margin(10, 12, 10, 6))

  combo <- p_dep / (p_voto | p_etiqueta) +
    plot_layout(heights = c(1.15, 1))

  ggsave(outfile, combo, width = 13.5, height = 7.2, dpi = 220, bg = "white")
}

# Exportar graficos
plot_segmento(df, file.path(plots_dir, "01_segmento.png"))
save_combo_plot(df, "genero", "Genero del panel", "Genero por segmento", file.path(plots_dir, "02_genero.png"), "Dark 3")
save_combo_plot(df, "edad", "Grupos etarios", "Edad por segmento", file.path(plots_dir, "03_edad.png"), "Set 2")
save_combo_plot(df, "n_educativo", "Nivel educativo", "Educacion por segmento", file.path(plots_dir, "04_educacion.png"), "Set 3")
plot_complementarias(df, file.path(plots_dir, "05_complementarias.png"))

# Resumen minimo para la portada del PPT
summary_tbl <- tibble::tibble(
  metrica = c("n_total", "n_segmentos", "fuente_csv"),
  valor = c(as.character(nrow(df)), as.character(n_distinct(df$segmento)), normalizePath(infile))
)
write.csv(summary_tbl, file.path(data_outdir, "r3_composicion_summary.csv"), row.names = FALSE)

segmentos_tbl <- df |>
  count(segmento, sort = TRUE) |>
  mutate(pct = n / sum(n))
write.csv(segmentos_tbl, file.path(data_outdir, "r3_segmentos_composicion.csv"), row.names = FALSE)

message("Graficos guardados en: ", normalizePath(plots_dir))
