library(tidyverse)
library(ggalluvial)
library(patchwork)

source("DriveFlow/qualcode.R") # Para funcion read speadsheet

b25 <- rio::import("data/processed/acumulada/2025/base_acumulada.csv")

b26 <- rio::import("data/processed/acumulada/2026/base_acumulada.csv")

cols25 <- c("r1_q1", "r1_q2", "r13_q1", "r13_q4", "r27_q4","r27_q5")
cols26 <- c("r7_q1", "r7_q2")

participants25 <- b25 |> 
  select(-which(str_detect(names(b25),"r(\\d+)_q(\\d+)"))) 

participants26 <- b26 |> 
  select(-which(str_detect(names(b26),"r(\\d+)_q(\\d+)"))) 


participants <- participants25 |> 
  full_join(participants26) |> 
  relocate(nombre)


orsi <- b25 |>
  select(numero, all_of(cols25)) |> 
  rename_with(~ paste0(.x, "_25"), -numero) |> 
full_join(
    b26 |> 
      select(numero, all_of(cols26)) |> 
      rename_with(~ paste0(.x, "_26"), -numero), 
  by = "numero") |>
full_join(participants, by = "numero") |> 
  relocate(
    -matches("r(\\d+)_q(\\d+)"),
    .after = numero
  ) 





orsi_aprobación <- orsi|> 
  select(numero, r1_q1_25, r13_q1_25, r27_q4_25, r7_q1_26) |> 
  mutate(
    r1_q1_25 = case_when(
      r1_q1_25 == "Aprueba" ~ "Aprueba",
      r1_q1_25 == "Desaprueba" ~ "Desaprueba",
      r1_q1_25 == "Ni aprue. ni desapru."~ "Ni aprueba, ni desaprueba",
      TRUE ~ NA_character_
    ),
    r13_q1_25 = case_when(
      r13_q1_25 == "Aprueba su desempeño" ~ "Aprueba",
      r13_q1_25 == "Desapr.  su desempeño" ~ "Desaprueba",
      r13_q1_25 == "Ni apr.  ni desapr."~ "Ni aprueba, ni desaprueba",
      r13_q1_25 == "Pref. no contestar"~ "NS/NC",
      TRUE ~ NA_character_
    ),
    r27_q4_25 = case_when(
      r27_q4_25 == 1 ~ "Aprueba",
      r27_q4_25 == 2 ~ "Ni aprueba, ni desaprueba",
      r27_q4_25 == 3 ~ "Desaprueba",
      r27_q4_25 == 4 ~ "NS/NC",
      TRUE ~ NA_character_
    ), 
    r7_q1_26 = if_else(
      r7_q1_26 == "Ni uno, ni otro",
      "Ni aprueba, ni desaprueba",
      r7_q1_26
   )
  ) 
  



  
  vars <- c("r13_q1_25", "r27_q4_25", "r7_q1_26")
  
  orsi_aluvial <- orsi_aprobación |> 
    mutate(across(all_of(vars), ~ factor(
      .x,
      levels = c("Aprueba", "Ni aprueba, ni desaprueba", "Desaprueba", "NS/NC")
    ))) |> 
    drop_na(vars) |> 
    select(all_of(vars)) |>
    count(across(everything()), name = "n")
  
p1 <- ggplot(
    orsi_aluvial,
    aes(
      axis1 = r13_q1_25,
      axis2 = r27_q4_25,
      axis3 = r7_q1_26,
      y = n
    )
  ) +
  geom_alluvium(
    aes(fill = r13_q1_25),
    width = 1/10,
    alpha = 0.70,
    knot.pos = 0.35
  ) +
  geom_stratum(
    aes(fill = after_stat(stratum)),
    width = 1/10,
    color = "white"
  )+
  geom_text(
    stat = "stratum",
    aes(
      label = after_stat(
        ifelse(stratum != "NS/NC",
               count,
               "")
      )
    ),
    size = 3,
    fontface = "bold",                  # ← alineado hacia la derecha
    color = "white"
    )+
  geom_text(
    stat = "stratum",
    aes(
      label = after_stat(ifelse(x == 1 & stratum != "NS/NC", as.character(stratum), "")),
      x = after_stat(x - 0.105)   # ← mueve a la izquierda
    ),
    angle = 90,
    size = 2.5,
    fontface = "bold",                  # ← alineado hacia la derecha
    color = NULL
  )+
  scale_x_discrete(
    limits = vars,
    labels = c("Agosto\n2025", "Diciembre\n2025", "Abril\n2026"),
    expand = c(.08, .08)
  ) +
  scale_fill_manual(
    values = c(
      "Aprueba" = "#2ca02c",
      "Ni aprueba, ni desaprueba" = "#efca36",
      "Desaprueba" = "#d62728",
      "NS/NC" = "#D9D9D9"
    )
  ) +
  labs(
    x = NULL,
    y = NULL,
    fill = NULL,
    title = "Aprobación de Yamandú Orsi por ronda",
    subtitle = "Base total: 91 personas"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11, color = "grey40"),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 11, face = "bold"),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.margin = margin(12, 18, 12, 18)
  )
  
  
  ggsave(
    "C:/Users/Julian/Desktop/JULIAN/plotorsi5.png",
    width = 2400,
    height = 1800,
    units = "px",
    dpi = 300
  )



# RAZONES 

R27 <- read_spreadsheet(
  folder_url = "https://drive.google.com/drive/u/0/folders/1pYaRO2gJ2MjoF3zqh4lS-PbEcALNYwMT",
  file_name = "transcripcion_R27"
)

R7 <- read_spreadsheet(
  folder_url = "https://drive.google.com/drive/u/0/folders/11TJVEyGNPuupcKpMzJtstV-oeC6Tmoca",
  file_name = "R7_base_de_datos_analisis"
)

R7code <- read_spreadsheet(
  folder_url = "https://drive.google.com/drive/u/0/folders/11TJVEyGNPuupcKpMzJtstV-oeC6Tmoca",
  file_name = "R7_base_de_datos_analisis", 
  sheet = "Copia de q2"
) 

# preguntas <- read_spreadsheet(
#   folder_url = "https://drive.google.com/drive/u/0/folders/1u0VHY2ZJhnCd4ILzAN91To659o3rfkqy",
#   file_name = "Base de preguntas Panel Focus"
# ) 

r7 <- R7

R7code <- R7code |> 
  select(Código, `Etiqueta de Categoría`) |> 
  rename(code = Código, label = `Etiqueta de Categoría`)


r7_recoded <- r7 |>
  select(numero, q1, q2, q2a, q2b, q2c) |> 
  
  pivot_longer(
    cols = matches("q2a|q2b|q2c"),
    names_to = "var",
    values_to = "code"
  ) |> 
  
  left_join(R7code, by = c("code" = "code")) |> 
  
  mutate(code = coalesce(label, code)) |>
  
  select(-label) |>
  
  pivot_wider(
    names_from = var,
    values_from = code
  ) 
    

r27 <- R27
r27 |> count(q4)

r27 <- r27 |> 
  select(numero, q4, q5, codigo_q5)

orsi_razones <- r27 |> 
  mutate(numero = as.character(numero)) |> 
  inner_join(r7_recoded, by = "numero") |> 
  mutate(
    q4= case_when(
      q4 == 1 ~ "Aprueba",
      q4 == 2 ~ "Ni aprueba, ni desaprueba",
      q4 == 3 ~ "Desaprueba",
      q4 == 4 ~ "NS/NC",
      TRUE ~ NA_character_
    ), 
    q1 = if_else(
      q1 == "Ni uno, ni otro",
      "Ni aprueba, ni desaprueba",
      q1
    )
    ) 

        
categorias_25 <- orsi_razones |>
  distinct(q4) |>
  pull(q4)

colores <- c(
  "Aprueba" = "#2ca02c",
  "Ni aprueba, ni desaprueba" = "#efca36",
  "Desaprueba" = "#d62728"
)

library(dplyr)
library(ggplot2)
library(purrr)
library(patchwork)

orsi_razones <- orsi_razones |> 
  filter(q4 != "NS/NC")

categorias_q4 <- orsi_razones |>
  distinct(q4) |>
  pull(q4)

graficas <- map(categorias_q4, \(categoria) {

  base_total <- nrow(orsi_razones)

  base_categoria <- orsi_razones |>
    filter(q4 == categoria) |>
    nrow()

  theme_custom <- function() {
    theme_minimal(base_size = 12) + 
      theme(
        plot.title = element_text(face = "bold", size = 10),
        plot.subtitle = element_text(size = 11, color = "grey40"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 11, face = "bold"),
        panel.grid = element_blank(),
        axis.title.y = element_text(size = 10, face = "bold"),
        legend.position = "bottom",
        legend.title = element_blank(),
        plot.margin = margin(12, 18, 12, 18)
      )
  }


  p1 <- orsi_razones |>
    filter(q4 == categoria) |>
    mutate(codigo_q5 = str_to_title(codigo_q5)) |>
    ggplot(aes(x = codigo_q5, fill = q4)) +
    geom_bar() +
    scale_fill_manual(values = colores, guide = "none") +
    geom_text(
      aes(y = after_stat(count), label = after_stat(count)),
      stat = "count",
      hjust = 1.5,
      size = 5,
      fontface = "bold"
    ) +
    coord_flip() + 
    labs(
      x = "Razones Diciembre 2025",
      y = NULL)+
    theme_custom()

  p2 <- orsi_razones |>
    filter(q4 == categoria) |>
    tidyr::pivot_longer(
      cols = c(q2a, q2b, q2c),
      names_to = "pregunta",
      values_to = "razon_2026"
    ) |>
    mutate(
      q1 = factor(
        q1,
        levels = c("Aprueba", "Ni aprueba, ni desaprueba", "Desaprueba", "NS/NC")
      )
    ) |> 
    filter(!is.na(razon_2026)) |>
    ggplot(aes(x = razon_2026, fill = q1)) +
    geom_bar() +
    scale_fill_manual(values = colores) +
    geom_text(
      aes(y = after_stat(count), label = after_stat(count)),
      stat = "count",
      position = position_stack(vjust = 0.5),
      size = 3,
      fontface = "bold"
    ) +
    coord_flip() + 
    labs(
      x = "Razones Abril 2026",
      y = NULL)+
    theme_custom()

  (p1 / p2) +
    plot_annotation(
      title = paste("Categoría:", categoria, "- Diciembre 2025"),
      caption = paste0(
        "Base total: ", base_total,
        " | N categoría: ", base_categoria
      ),
      theme = theme(
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5)
      )
    )
})

names(graficas) <- categorias_q4


graficas[["Aprueba"]]
graficas[["Ni aprueba, ni desaprueba"]]
graficas[["Desaprueba"]]




walk2(
  graficas,
  names(graficas),
  \(plot, nombre) {
    ggsave(
      filename = paste0("grafica_", nombre, ".png"),
      plot = plot,
      width = 10,
      height = 8,
      units = "in",
      dpi = 300
    )
  }
)




