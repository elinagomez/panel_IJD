library(tidyverse)
library(scales)
library(showtext)

# fuente y render con showtext
font_add_google("Montserrat", "Montserrat")
showtext_auto(TRUE)
showtext_opts(dpi = 300)

# ---------- datos ----------
cambio_lvls <- c("Empeoró mucho","Empeoró","Se mantuvo","Mejoró","Mejoró mucho","No respondió")
aprob_lvls  <- c("Aprueba su desempeño","Ni apr. ni desapr.","Desapr. su desempeño","Pref. no contestar")

tab <- tribble(
  ~aprobacion,               ~cambio,          ~n,
  "Aprueba su desempeño",    "Empeoró mucho",   0,
  "Aprueba su desempeño",    "Empeoró",         0,
  "Aprueba su desempeño",    "Se mantuvo",     13,
  "Aprueba su desempeño",    "Mejoró",          5,
  "Aprueba su desempeño",    "Mejoró mucho",    3,
  "Aprueba su desempeño",    "No respondió",    0,

  "Ni apr. ni desapr.",      "Empeoró mucho",   0,
  "Ni apr. ni desapr.",      "Empeoró",        13,
  "Ni apr. ni desapr.",      "Se mantuvo",     11,
  "Ni apr. ni desapr.",      "Mejoró",          3,
  "Ni apr. ni desapr.",      "Mejoró mucho",    0,
  "Ni apr. ni desapr.",      "No respondió",    0,

  "Desapr. su desempeño",    "Empeoró mucho",   9,
  "Desapr. su desempeño",    "Empeoró",         4,
  "Desapr. su desempeño",    "Se mantuvo",      3,
  "Desapr. su desempeño",    "Mejoró",          0,
  "Desapr. su desempeño",    "Mejoró mucho",    0,
  "Desapr. su desempeño",    "No respondió",    0,

  "Pref. no contestar",      "Empeoró mucho",   0,
  "Pref. no contestar",      "Empeoró",         0,
  "Pref. no contestar",      "Se mantuvo",      0,
  "Pref. no contestar",      "Mejoró",          0,
  "Pref. no contestar",      "Mejoró mucho",    0,
  "Pref. no contestar",      "No respondió",    1
) |>
  mutate(
    cambio = factor(cambio, levels = cambio_lvls),
    aprobacion = factor(aprobacion, levels = aprob_lvls)
  )

# ---------- estilos (tamaño x3) ----------
base_size <- 18
label_size <- base_size / 4  # para números del heatmap

theme_base <- theme_minimal(base_size = base_size, base_family = "Montserrat") +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    plot.title   = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  )

# ---------- gráficos ----------
p1 <- ggplot(tab, aes(x = cambio, y = n, fill = aprobacion)) +
  geom_col(position = "fill", width = .8, color = "white") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Distribución de aprobación dentro de cada percepción de cambio",
    x = NULL, y = "% dentro de cada categoría de cambio", fill = "Aprobación"
  ) +
  theme_base

p2 <- ggplot(tab, aes(x = aprobacion, y = n, fill = cambio)) +
  geom_col(position = "fill", width = .8, color = "white") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Distribución de percepción de cambio dentro de cada nivel de aprobación",
    x = NULL, y = "% dentro de cada nivel de aprobación", fill = "Cambio percibido"
  ) +
  theme_base

p3 <- ggplot(tab, aes(x = cambio, y = aprobacion, fill = n)) +
  geom_tile(color = "white") +
  geom_text(aes(label = n), family = "Montserrat", size = label_size) +
  scale_fill_gradient(low = "#F2F0F7", high = "#54278F") +
  labs(
    title = "Aprobación vs. percepción del cambio (conteos)",
    x = NULL, y = NULL, fill = "Casos"
  ) +
  theme_base

# ---------- guardar ----------
out_dir <- "plots/acumulada/acumulada_agosto"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

ggsave(file.path(out_dir, "aprob_dentro_de_cambio_pct.png"), p1,
       width = 14, height = 9, dpi = 300, bg = "white")

ggsave(file.path(out_dir, "cambio_dentro_de_aprob_pct.png"), p2,
       width = 14, height = 9, dpi = 300, bg = "white")

ggsave(file.path(out_dir, "heatmap_conteos.png"), p3,
       width = 14, height = 9, dpi = 300, bg = "white")
