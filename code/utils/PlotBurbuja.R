
library(dplyr)
library(ggplot2)
library(stringr)
library(readr)
library(tidyr)
library(sysfonts)
library(ggtext)


# Cargar base (Requiere Base recodificada. Todavía no está en el Flujo de trabajo)

# round_id <- "R22"
# path <- file.path("data", "processed", "transcriptions", "output", as.character(year), paste0("transcripcion_", round_id, ".csv"))
# data <- read_csv(path, col_types = cols(.default = "c"))

question <- paste0("codigo_","q4") 
plot_title <- "" 
subtitle_text <- paste0(
  "Ranking de respuestas ( ",
  "<span style='color:#d69e48;'>**Top 1**</span>, ",
  "<span style='color:#a08399;'>**Top 2**</span> y ",
  "<span style='color:#559e98;'>**Top 3**</span>",
  " )"
)


# data <- importar base recodificada local 

# -------------------------------------

# Calculo de totales. 

total <- data |> 
  rename(q = question) |> 
  count(q) |> 
  mutate(
    q = str_replace_all(q, "_", " "),
    q = str_to_title(q)
    ) |> 
  mutate(
    var = "Total") 


# Orden segun numero total. 

orden <- total|>
  arrange(n) |>
  pull(q)


# Para segmento --------------------------------------------

data |> 
  rename(
    q = question,
    var = segmento
    ) |>
  count(q, var) |> 
  mutate(
    q = str_replace_all(q, "_", " "),
    q = str_to_title(q)
    ) |>
  bind_rows(total) |> 
  mutate(
    xpos = case_when(
      var == "Total" ~ -0.5,
      var == "Montevideo" ~ 0.5,
      var == "Canelones" ~ 1,
      var == "Interior Coalición" ~ 1.5,
      var == "Interior Frente Amplio" ~ 2
    )
  ) |> 
  mutate(q = factor(q, levels = orden)) |>
  group_by(var) |> 
  mutate(
    rank = dense_rank(-n), 
    rank = factor(rank),
    rank = if_else(var == "Total", NA, rank)
  ) |> 
  ggplot(aes(x = xpos, y = q, size = n*8)) + 
  geom_point(aes(color = rank), show.legend = FALSE) +
  scale_x_continuous(
    breaks = c(-0.5, 0.5, 1, 1.5, 2),
    labels = str_wrap(c("Total", "Montevideo", "Canelones", "Interior Coalición", "Interior Frente Amplio"), 15),
    expand = expansion(add = c(0.25, 0.25))  
  ) +
  scale_size_continuous(range = c(5, 50)) +
    
  geom_text(
    data = total,
    aes(x = 0, y = q,label = str_wrap(q, 15)), 
    hjust = 0.5, 
    size = 4, 
    fontface = "bold",
    family = "montserrat"
  ) +
  scale_color_manual(
    values = c(
      "1" = "#d69e48",
      "2" = "#a08399",
      "3" = "#559e98"
    ),
    na.value = "gray",  # para ranks >3
    guide = "none"
  ) +
  labs(
    title = str_wrap(plot_title, 111),
    subtitle = subtitle_text
  )+
  theme_void() +
  theme(
    axis.text.x = element_text(size = 15, face = "bold"), 
    text = element_text(family = "Montserrat", size = 13),
    plot.title = element_text(         
      margin = margin(b = 15,l = 20), 
      face  = "bold"
    ),
    plot.subtitle = ggtext::element_markdown(margin = margin(b = 10, l = 21), size = 12),
    plot.margin =  margin(t = 20, b = 20)
)


# Para voto 2 ----------------------------------------------


data |> 
  rename(
    q = question,
    var = voto2
    ) |>
  count(q, var) |> 
  mutate(
    q = str_replace_all(q, "_", " "),
    q = str_to_title(q)
    ) |>
  bind_rows(total) |> 
  mutate(
    xpos = case_when(
      var == "Total" ~ -0.5,
      var == "FA" ~ 0.5,
      var == "CM" ~ 1
    )
  ) |> 
  mutate(q = factor(q, levels = orden)) |>
  group_by(var) |> 
  mutate(
    rank = dense_rank(-n), 
    rank = factor(rank),
    rank = if_else(var == "Total", NA, rank)
  ) |> 
  ggplot(aes(x = xpos, y = q, size = n*10)) + 
  geom_point(aes(color = rank), show.legend = FALSE) +
  scale_x_continuous(
    breaks = c(-0.5, 0.5, 1),
    labels = str_wrap(c("Total", "Frente Amplio", "Coalición Multicolor"), 15),
    expand = expansion(add = c(0.25, 0.25))  
  ) +
  scale_size_continuous(range = c(5, 50)) +
    
  geom_text(
    data = total,
    aes(x = 0, y = q,label = str_wrap(q, 15)), 
    hjust = 0.5, 
    size = 5, 
    fontface = "bold",
    family = "montserrat"
  ) +
  scale_color_manual(
    values = c(
      "1" = "#d69e48",
      "2" = "#a08399",
      "3" = "#559e98"
    ),
    na.value = "gray",  # para ranks >3
    guide = "none" 
  ) +
  labs(
    title = str_wrap(plot_title, 111),
    subtitle = subtitle_text
  )+
  theme_void() +
  theme(
    axis.text.x = element_text(size = 15, face = "bold"), 
    text = element_text(family = "Montserrat", size = 13),
    plot.title = element_text(         
      margin = margin(b = 15,l = 20), 
      face  = "bold"
    ),
    plot.subtitle = ggtext::element_markdown(margin = margin(b = 10, l = 21), size = 12),
    plot.margin =  margin(t = 20, b = 20)
)


# Para etiqueta --------------------------------------------


data |> 
  rename(
    q = question,
    var = etiqueta
    ) |>
  count(q, var) |> 
  mutate(
    q = str_replace_all(q, "_", " "),
    q = str_to_title(q)
    ) |>
  bind_rows(total) |> 
  mutate(var = if_else(var == "NA", NA, var)) |> 
  drop_na() |> 
  mutate(
    xpos = case_when(
      var == "Total" ~ -0.5,
      var == "oficialista_acerrimo" ~ 0.5,
      var == "oficialista_abierto" ~ 1,
      var == "dialoguista" ~ 1.5,
      var == "oposicion_abierta" ~ 2,
      var == "oposicion_cerril" ~ 2.5,
      var == "descreidas/alejadas" ~ 3,
      var == "desinformadas" ~ 3.5,
    )
  ) |> 
  mutate(q = factor(q, levels = orden)) |>
  group_by(var) |> 
  mutate(
    rank = dense_rank(-n), 
    rank = factor(rank),
    rank = if_else(var == "Total", NA, rank)
  ) |> 
  ggplot(aes(x = xpos, y = q, size = n*8)) + 
  geom_point(aes(color = rank), show.legend = FALSE) +
  scale_x_continuous(
    breaks = c(-0.5, 0.5, 1, 1.5, 2, 2.5, 3, 3.5),
    labels = str_wrap(c("Total","oficialista_acerrimo","oficialista_abierto","dialoguista","oposicion_abierta","oposicion_cerril","descreidas/alejadas", "desinformadas")
    , 15),
    expand = expansion(add = c(0.25, 0.25))  
  ) +
  scale_size_continuous(range = c(5, 50)) +
    

  geom_text(
    data = total,
    aes(x = 0, y = q,label = str_wrap(q, 15)), 
    hjust = 0.5, 
    size = 4, 
    fontface = "bold",
    family = "montserrat"
  ) +
  scale_color_manual(
    values = c(
      "1" = "#d69e48",
      "2" = "#a08399",
      "3" = "#559e98"
    ),
    na.value = "gray",  # para ranks >3
    guide = "none"
  ) +
  labs(
    title = str_wrap(plot_title, 111),
    subtitle = subtitle_text
  )+
  theme_void() +
  theme(
    axis.text.x = element_text(hjust = 0.5, angle = 30, size = 15, face = "bold"), 
    text = element_text(family = "Montserrat", size = 13),
    plot.title = element_text(         
      margin = margin(b = 15,l = 20), 
      face  = "bold"
    ),
    plot.subtitle = ggtext::element_markdown(margin = margin(b = 10, l = 21), size = 12),
    plot.margin =  margin(t = 20, b = 20))
