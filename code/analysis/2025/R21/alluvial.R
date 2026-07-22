# temáticas emergentes

library(dplyr)
library(readxl)
library(ggplot2)
library(ggalluvial)
library(forcats)
library(stringr)
library(scales)
library(sysfonts)
library(showtext)
year <- 2025
round_id <- "R21"


font_add_google("Montserrat", "montserrat")
showtext_auto()

s <- read_xlsx(paste0("data/processed/analysis/", year, "/", round_id, "/emergentes_join.xlsx"))

# Recode missing category label
s <- s %>%
  mutate(Area_R21 = if_else(Area_R21 == "No", "Otro", Area_R21)) |> 
  mutate(Area_R21 = if_else(Area_R21 == "Transporte/tránsito", "Transporte", Area_R21)) |> 
  mutate(Area_R19 = if_else(Area_R19 == "Transporte/tránsito", "Transporte", Area_R19))

head(s)

# # A tibble: 6 × 18
#   nombre edadnum edad  genero numero departamento n_educativo oficio voto  segmento voto2 etiqueta Area_R19 Problema_R19
#   <chr>  <chr>   <chr> <chr>  <chr>  <chr>        <chr>       <chr>  <chr> <chr>    <chr> <chr>    <chr>    <chr>       
# 1 Carol… 34      31 a… Mujer  59895… Canelones    Universida… Audit… Fren… Canelon… FA    NA       Transpo… El principa…
# 2 Crist… NA      60 y… Mujer  59891… Canelones    Eduación M… Emple… Fren… Canelon… FA    NA       Segurid… Yo personal…
# 3 Debor… 37      31 a… Mujer  59891… Canelones    Educación … Emple… Fren… Canelon… FA    NA       Otro     Económicos  
# 4 Aleja… 41      31 a… Mujer  59894… Canelones    Educación … Ama d… Otro… Canelon… CM    NA       Empleo   Mando y man…
# 5 Alina  24      30 y… Mujer  59898… Canelones    Universida… Ama d… Fren… Canelon… FA    NA       Empleo   Estoy busca…
# 6 Carol… 34      31 a… Mujer  59891… Colonia      Eduación M… Emple… Fren… Interio… FA    NA       Transpo… Demoras en …
# # ℹ 4 more variables: Solucion_R19 <chr>, Area_R21 <chr>, Problema_R21 <chr>, Solucion_R21 <chr>
# >

# grafico variación de categorías de área entre R19 y R21

s_area <- s %>%
  filter(!is.na(Area_R19) & !is.na(Area_R21)) %>%
  group_by(Area_R19, Area_R21) %>%
  summarise(n = n()) %>%
  ungroup()

# Order categories by total mentions in R19 and apply same order to R21
levels_order <- s_area %>%
  group_by(Area_R19) %>%
  summarise(total = sum(n), .groups = "drop") %>%
  arrange(desc(total)) %>%
  pull(Area_R19)

s_area_plot <- s_area %>%
  mutate(
    Area_R19 = fct_relevel(Area_R19, levels_order),
    Area_R21 = fct_relevel(Area_R21, levels_order)
  )

# Choose top N categories to highlight
top_n <- 3
top_areas <- head(levels_order, n = min(top_n, length(levels_order)))

s_area_plot <- s_area_plot %>%
  mutate(highlight = if_else(Area_R19 %in% top_areas | Area_R21 %in% top_areas,
                             "highlight", "other"))
ggplot(s_area_plot,
       aes(axis1 = Area_R19,
           axis2 = Area_R21,
           y = n)) +
  geom_alluvium(aes(fill = Area_R19, alpha = highlight), width = 1/16, color = NA) +
  # Light strata so they frame flows without overpowering
  geom_stratum(width = 1/16, fill = "grey95", color = "grey75") +
  # Move labels just outside the strata and append absolute counts
  geom_text(
    stat = "stratum",
    aes(
      # push left side outward and right side outward
      x = after_stat(ifelse(x < 1.5, x - 0.05, x + 0.05)),
      hjust = after_stat(ifelse(x < 1.5, 1, 0)),
      label = str_wrap(after_stat(paste0(stratum, " (", comma(round(ymax - ymin)), ")")), width = 22)
    ),
    size = 4.5*3,
    color = "black",
    family = "montserrat",
    check_overlap = TRUE
  ) +
  coord_cartesian(clip = "off") +
  scale_x_discrete(limits = c("Ronda 19", "Ronda 21"), expand = c(.05, .05)) +
  scale_alpha_manual(values = c(highlight = 0.75, other = 0.5), guide = "none") +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  scale_y_continuous(expand = expansion(mult = c(0.03, 0.03))) +
  labs(title = "Variación de temáticas entre Ronda 19 y Ronda 21",
       y = "",
       x = "") +
  theme_void(base_family = "montserrat") +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(size = 18*3, face = "bold", family = "montserrat"),
    axis.text = element_text(size = 12*3, face = "bold", family = "montserrat"),
    # axis.text.y = element_text(margin = margin(r = 14)),
    axis.text.y = element_blank(),
    axis.title.y = element_text(margin = margin(r = 24), family = "montserrat"),
    plot.margin = margin(20, 120, 20, 120)
  )

ggsave(paste0("plots/", year, "/", round_id, "/variacion_areas_r19_r21.png"), width = 12, height = 8, dpi = 300, bg = "white")
