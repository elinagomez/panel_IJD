library(tidyverse)
library(scales)
library(showtext)

font_add_google("Montserrat", "Montserrat")
showtext_auto(TRUE)
showtext_opts(dpi = 300)

cambio_lvls <- c("Empeoró mucho","Empeoró","Se mantuvo","Mejoró","Mejoró mucho")
eval_lvls   <- c("MB–muy responsables","Bien–responsables","Ni bien ni mal",
                 "Mal–irresponsables","MM–muy irresp.")

tab_opo <- tribble(
  ~evaluacion,              ~cambio,           ~n,
  "MB–muy responsables",    "Se mantuvo",       4,
  "MB–muy responsables",    "Mejoró",           1,
  "MB–muy responsables",    "Mejoró mucho",     2,

  "Bien–responsables",      "Empeoró",          5,
  "Bien–responsables",      "Se mantuvo",      11,
  "Bien–responsables",      "Mejoró",           4,

  "Ni bien ni mal",         "Empeoró mucho",    2,
  "Ni bien ni mal",         "Empeoró",          7,
  "Ni bien ni mal",         "Se mantuvo",      21,
  "Ni bien ni mal",         "Mejoró",           5,
  "Ni bien ni mal",         "Mejoró mucho",     2,

  "Mal–irresponsables",     "Empeoró mucho",    2,
  "Mal–irresponsables",     "Empeoró",         11,
  "Mal–irresponsables",     "Se mantuvo",       6,
  "Mal–irresponsables",     "Mejoró",           3,

  "MM–muy irresp.",         "Empeoró mucho",    2,
  "MM–muy irresp.",         "Se mantuvo",       4
) |>
  mutate(
    cambio     = factor(cambio, levels = cambio_lvls),
    evaluacion = factor(evaluacion, levels = eval_lvls)
  )

base_size  <- 18
label_size <- base_size / 4

theme_base <- theme_minimal(base_size = base_size, base_family = "Montserrat") +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    plot.title   = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  )

p1 <- ggplot(tab_opo, aes(x = cambio, y = n, fill = evaluacion)) +
  geom_col(position = "fill", width = .8, color = "white") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Evaluación de la oposición dentro de cada percepción de cambio",
    x = NULL, y = "% dentro de cada categoría de cambio", fill = "Evaluación"
  ) + theme_base

p2 <- ggplot(tab_opo, aes(x = evaluacion, y = n, fill = cambio)) +
  geom_col(position = "fill", width = .8, color = "white") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Percepción de cambio dentro de cada evaluación de la oposición",
    x = NULL, y = "% dentro de cada evaluación", fill = "Cambio percibido"
  ) + theme_base

p3 <- ggplot(tab_opo, aes(x = cambio, y = evaluacion, fill = n)) +
  geom_tile(color = "white") +
  geom_text(aes(label = n), family = "Montserrat", size = label_size) +
  scale_fill_gradient(low = "#F2F0F7", high = "#54278F") +
  labs(
    title = "Evaluación vs. cambio percibido (conteos)",
    x = NULL, y = NULL, fill = "Casos"
  ) + theme_base

out_dir <- "plots/acumulada/acumulada_agosto"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

ggsave(file.path(out_dir, "oposicion_eval_dentro_cambio_pct.png"), p1,
       width = 14, height = 9, dpi = 300, bg = "white")
ggsave(file.path(out_dir, "oposicion_cambio_dentro_eval_pct.png"), p2,
       width = 14, height = 9, dpi = 300, bg = "white")
ggsave(file.path(out_dir, "oposicion_heatmap_conteos.png"), p3,
       width = 14, height = 9, dpi = 300, bg = "white")
