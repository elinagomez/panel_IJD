# Temáticas emergentes R24 vs R27

library(dplyr)
library(readxl)
library(ggplot2)
library(ggsankey)
library(tidyr)
library(forcats)
library(stringr)
library(scales)
library(ggbump)
library(sysfonts)
library(showtext)
year <- 2025
round_id <- "R27"


font_add_google("Montserrat", "montserrat")
showtext_auto()

# Stub retained for clarity; dates are hardcoded below
get_round_date <- function(round_num) NA_character_

# Harmonize labels across rounds
recode_tema <- function(x) {
  x <- str_trim(x)
  x <- dplyr::recode(
    x,
    `Transporte/tránsito` = "Transporte",
    `Transporte / Tránsito` = "Transporte",
    `No` = "Otro",
    `Otro` = "Otro",
    `Otros (Especificar)` = "Otro",
    `Infrestructura Urbana` = "Otro",
    `Económico` = "Empleo",
    `Recursos Económicos` = "Empleo",
    `Problemas Vecinales` = "Seguridad",
    .default = x
  )

  # Optional catch-alls for typos/variants
  x <- ifelse(str_detect(x, regex("^econ", ignore_case = TRUE)), "Empleo", x)
  x
}

# Read joined dataset with q1 for R24 and R27
s <- read_excel(paste0("data/processed/analysis/", year, "/", round_id, "/tematicas_emergentes.xlsx"), guess_max = 5000) |>
  mutate(
    r24_q1 = recode_tema(r24_q1),
    r27_q1 = recode_tema(r27_q1)
  )

# Flow counts between rounds
s_area <- s |>
  filter(!is.na(r24_q1) & !is.na(r27_q1)) |>
  group_by(r24_q1, r27_q1) |>
  summarise(n = n(), .groups = "drop")

# Helper to build per-round label vectors
count_labels <- function(vec) {
  tab <- sort(table(vec), decreasing = TRUE)
  setNames(sprintf("%s (%s)", names(tab), comma(as.integer(tab))), names(tab))
}

# Per-round labels (for axis-specific counts)
labels_r24 <- count_labels(s$r24_q1)
labels_r27 <- count_labels(s$r27_q1)

# Axis labels with hardcoded campaign dates
label_two_axes <- c(
  r24_q1 = "Ronda 24\n15/11",
  r27_q1 = "Ronda 27\n06/12"
)

# Order categories by combined mentions across both rounds (descending)
cat_totals_two <- bind_rows(
  s_area |>
    group_by(cat = r24_q1) |>
    summarise(total = sum(n), .groups = "drop"),
  s_area |>
    group_by(cat = r27_q1) |>
    summarise(total = sum(n), .groups = "drop")
) |>
  group_by(cat) |>
  summarise(total = sum(total), .groups = "drop") |>
  arrange(desc(total))

levels_order <- cat_totals_two$cat

# Distinct palette with enough colors for all categories
palette_cols <- setNames(hue_pal()(length(levels_order)), levels_order)

s_area_plot <- s_area |>
  mutate(
    r24_q1 = fct_relevel(r24_q1, levels_order),
    r27_q1 = fct_relevel(r27_q1, levels_order)
  )

# Highlight top categories to make main flows easier to follow (como en R21)
top_n <- 3
top_areas <- head(levels_order, n = min(top_n, length(levels_order)))

s_area_plot <- s_area_plot |>
  mutate(highlight = if_else(r24_q1 %in% top_areas | r27_q1 %in% top_areas,
                             "highlight", "other"))

p <- ggplot(
  s_area_plot,
  aes(
    axis1 = r24_q1,
    axis2 = r27_q1,
    y = n
  )
) +
  ggalluvial::geom_alluvium(aes(fill = r24_q1, alpha = highlight), width = 1/16, color = NA) +
  ggalluvial::geom_stratum(width = 1/16, fill = "grey95", color = "grey75") +
  geom_text(
    stat = ggalluvial::StatStratum,
    aes(
      x = after_stat(ifelse(x < 1.5, x - 0.05, x + 0.05)),
      hjust = after_stat(ifelse(x < 1.5, 1, 0)),
      label = str_wrap(after_stat(paste0(stratum, " (", comma(round(ymax - ymin)), ")")), width = 22)
    ),
    size = 4.5 * 3,
    color = "black",
    family = "montserrat",
    check_overlap = TRUE
  ) +
  coord_cartesian(clip = "off") +
  scale_x_discrete(limits = c("Ronda 24\n15/11", "Ronda 27\n06/12"), expand = c(.05, .05)) +
  scale_alpha_manual(values = c(highlight = 0.75, other = 0.5), guide = "none") +
  scale_fill_manual(values = palette_cols) +
  scale_y_continuous(expand = expansion(mult = c(0.03, 0.03))) +
  labs(
    title = "Variación de temáticas entre Ronda 24 y Ronda 27",
    y = "",
    x = ""
  ) +
  theme_void(base_family = "montserrat") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 18 * 3, face = "bold", family = "montserrat"),
    axis.text.x = element_text(size = 12 * 3, face = "bold", family = "montserrat", lineheight = 0.3),
    axis.text.y = element_blank(),
    plot.margin = margin(20, 120, 20, 120)
  )

if (!dir.exists(paste0("plots/", year, "/", round_id, ""))) dir.create(paste0("plots/", year, "/", round_id, ""), recursive = TRUE)
ggsave(paste0("plots/", year, "/", round_id, "/variacion_areas_r24_r27.png"), p, width = 12, height = 8, dpi = 300, bg = "white")

# --- Versión con las cuatro rondas (R19, R21, R24, R27) ---
r21 <- read.csv(paste0("data/processed/analysis/", year, "/R21/R21_emergentes.csv")) |>
  transmute(
    numero = as.character(numero),
    r19_q1 = recode_tema(Area_R19),
    r21_q1 = recode_tema(Area_R21)
  )

r27 <- s |>
  transmute(
    numero = as.character(numero),
    r24_q1,
    r27_q1
  )

panel <- inner_join(r21, r27, by = "numero") |>
  filter(!is.na(r19_q1), !is.na(r21_q1), !is.na(r24_q1), !is.na(r27_q1))

panel_counts <- panel |>
  count(r19_q1, r21_q1, r24_q1, r27_q1, name = "n")

cat_totals_4 <- bind_rows(
  panel_counts |>
    group_by(cat = r19_q1) |>
    summarise(total = sum(n), .groups = "drop"),
  panel_counts |>
    group_by(cat = r21_q1) |>
    summarise(total = sum(n), .groups = "drop"),
  panel_counts |>
    group_by(cat = r24_q1) |>
    summarise(total = sum(n), .groups = "drop"),
  panel_counts |>
    group_by(cat = r27_q1) |>
    summarise(total = sum(n), .groups = "drop")
) |>
  group_by(cat) |>
  summarise(total = sum(total), .groups = "drop") |>
  arrange(desc(total))

levels_order_4 <- cat_totals_4$cat

palette_cols_4 <- setNames(hue_pal()(length(levels_order_4)), levels_order_4)
labels4_r19 <- count_labels(panel$r19_q1)
labels4_r21 <- count_labels(panel$r21_q1)
labels4_r24 <- count_labels(panel$r24_q1)
labels4_r27 <- count_labels(panel$r27_q1)

# Axis labels with campaign date per ronda (earliest date per campaign)
label_four_axes <- c(
  r19_q1 = "Ronda 19\n11/10",
  r21_q1 = "Ronda 21\n25/10",
  r24_q1 = "Ronda 24\n15/11",
  r27_q1 = "Ronda 27\n06/12"
)

panel_plot <- panel_counts |>
  mutate(
    r19_q1 = factor(r19_q1, levels = levels_order_4),
    r21_q1 = factor(r21_q1, levels = levels_order_4),
    r24_q1 = factor(r24_q1, levels = levels_order_4),
    r27_q1 = factor(r27_q1, levels = levels_order_4)
  ) |>
  tidyr::uncount(n)

panel_long <- ggsankey::make_long(panel_plot, r19_q1, r21_q1, r24_q1, r27_q1) |>
  mutate(
    node_label = case_when(
      x == "r19_q1" ~ labels4_r19[as.character(node)],
      x == "r21_q1" ~ labels4_r21[as.character(node)],
      x == "r24_q1" ~ labels4_r24[as.character(node)],
      x == "r27_q1" ~ labels4_r27[as.character(node)],
      TRUE ~ as.character(node)
    )
  )

p4 <- ggplot(
  panel_long,
  aes(
    x = x,
    next_x = next_x,
    node = node,
    next_node = next_node,
    fill = node
  )
) +
  geom_sankey(flow.alpha = 0.7, color = NA, width = 0.35) +
  geom_sankey_text(
    aes(label = node_label),
    size = 4.5 * 3,
    color = "black",
    family = "montserrat"
  ) +
  scale_x_discrete(labels = label_four_axes, expand = c(.05, .05)) +
  scale_fill_manual(values = palette_cols_4) +
  labs(
    title = "¿En cuál de estas áreas has experimentado problemas durante esta semana?",
    y = "",
    x = ""
  ) +
  theme_void(base_family = "montserrat") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 18 * 3, face = "bold", family = "montserrat"),
    axis.text.x = element_text(size = 12 * 3, face = "bold", family = "montserrat", lineheight = 0.3),
    axis.text.y = element_blank(),
    plot.margin = margin(20, 120, 20, 120)
  )

ggsave(paste0("plots/", year, "/", round_id, "/variacion_areas_r19_r21_r24_r27.png"), p4, width = 14, height = 8, dpi = 300, bg = "white")

# --- Ranking de temáticas por ronda (similar a prensa) ---
counts_rounds <- bind_rows(
  tibble(ronda = "R19", tema = r21$r19_q1),
  tibble(ronda = "R21", tema = r21$r21_q1),
  tibble(ronda = "R24", tema = s$r24_q1),
  tibble(ronda = "R27", tema = s$r27_q1)
) |>
  filter(!is.na(tema), tema != "") |>
  count(ronda, tema, name = "n") |>
  mutate(
    ronda = factor(ronda, levels = c("R19", "R21", "R24", "R27")),
    tema = factor(tema, levels = levels_order_4)
  )

ranking_rounds <- counts_rounds |>
  group_by(ronda) |>
  mutate(rank = dense_rank(desc(n))) |>
  ungroup()

max_rank_rounds <- max(ranking_rounds$rank, na.rm = TRUE)

p_rank <- ggplot(
  ranking_rounds,
  aes(x = ronda, y = rank, color = tema, group = tema)
) +
  geom_bump(linewidth = 1.6, smooth = 10) +
  geom_point(aes(fill = tema), size = 4.5, shape = 21, stroke = 1.2, color = "white") +
  scale_color_manual(
    values = palette_cols_4,
    drop = FALSE,
    guide = guide_legend(override.aes = list(size = 4, fill = NA))
  ) +
  scale_fill_manual(values = palette_cols_4, guide = "none") +
  scale_y_reverse(breaks = seq_len(max_rank_rounds)) +
  scale_x_discrete(labels = label_four_axes, expand = c(.05, .05)) +
  labs(
    title = "Ranking de temáticas por ronda",
    x = "Ronda",
    y = "",
    color = "Temática"
  ) +
  theme_minimal(base_family = "montserrat") +
  theme(
    plot.title = element_text(face = "bold", size = 14 * 3),
    axis.title = element_text(size = 11 * 3, color = "#333333"),
    axis.text.x = element_text(size = 11 * 3, margin = margin(t = 5), color = "#444444", lineheight = 0.3),
    axis.text.y = element_text(size = 11 * 3, color = "#444444"),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "right",
    legend.title = element_text(size = 10 * 3, color = "#333333", face = "bold"),
    legend.text = element_text(size = 9 * 3, color = "#333333"),
    legend.key = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(20, 40, 20, 40)
  )

ggsave(paste0("plots/", year, "/", round_id, "/ranking_areas_r19_r21_r24_r27.png"), p_rank, width = 12, height = 8, dpi = 300, bg = "white")
