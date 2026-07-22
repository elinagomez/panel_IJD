library(dplyr)
library(readr)
library(ggplot2)

year <- 2025
round <- "R7"
survey <- read_csv(file.path("data", "processed", "transcriptions", "output", as.character(year), paste0("transcripcion_", round, ".csv")))

# recodifico escalas Likert
survey <- survey |> 
  mutate(across(
    c(q1, q5, q9, q11),
    ~ recode(.x, `Muy en desacuerdo` = "Muy en desacuerdo", `2` = "En desacuerdo", `3` = "Ni de acuerdo ni en desacuerdo",
             `4` = "De acuerdo", `Muy de acuerdo` = "Muy de acuerdo")
  ))

# convierto en factores para poder ordenarlos
survey <- survey |> 
  mutate(across(c(q1, q5, q9, q11), factor, levels = c("Muy en desacuerdo", "En desacuerdo", "Ni de acuerdo ni en desacuerdo", "De acuerdo", "Muy de acuerdo")))

# En una escala de 1 al 5, siendo 1 muy en desacuerdo y 5 muy de acuerdo, 
# ¿qué tan de acuerdo o en desacuerdo estás con las siguientes afirmaciones?

# q1: “El Estado debería tener un papel más activo para que todas las personas puedan tener lo básico para vivir”.
# q5: “Un poco de mano dura del gobierno no viene mal al país”.
# q9: “La mayoría de las personas pobres lo son porque no se esfuerzan lo suficiente”.
# q11: “Deberían aumentar los impuestos a las personas más ricas del país”.


# gráfico de dispersión de q5 vs q1, coloreado por voto2
survey |>
  ggplot(
    aes(
      x      = q5,         # “un poco de mano dura…”
      y      = q1,         # “el estado debería tener un papel más activo…”
      colour = voto2
    )
  ) +
  geom_jitter(
    width  = 0.25,
    height = 0.25,
    alpha  = 0.8,
    size   = 3
  ) +
  scale_colour_brewer(
    palette = "Dark2",
    name    = "Voto"
  ) +
  labs(
    title    = "Posturas punitivas y solidarias según voto",
    subtitle = "",
    x        = 'Acuerdo con "un poco de mano dura del gobierno no viene mal"',
    y        = 'Acuerdo con "el estado debería tener un papel más activo"'
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# gráfico de dispersión de q9 vs q1, coloreado por voto2
survey |>
  ggplot(
    aes(
      x      = q9,         # “la mayoría de las personas pobres lo son porque no se esfuerzan lo suficiente”
      y      = q1,         # “el estado debería tener un papel más activo…”
      colour = voto2
    )
  ) +
  geom_jitter(
    width  = 0.25,
    height = 0.25,
    alpha  = 0.8,
    size   = 3
  ) +
  scale_colour_brewer(
    palette = "Dark2",
    name    = "Voto"
  ) +
  labs(
    title    = "Sensibilidad social según voto",
    subtitle = "",
    x        = 'Acuerdo con "la mayoría de las personas pobres lo son porque no se esfuerzan lo suficiente"',
    y        = 'Acuerdo con "el estado debería tener un papel más activo"'
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# gráfico de dispersión de q9 vs q11, coloreado por voto2
survey |>
  ggplot(
    aes(
      x      = q9,         # “la mayoría de las personas pobres lo son porque no se esfuerzan lo suficiente”
      y      = q11,        # “deberían aumentar los impuestos a las personas más ricas del país”
      colour = voto2
    )
  ) +
  geom_jitter(
    width  = 0.25,
    height = 0.25,
    alpha  = 0.8,
    size   = 3
  ) +
  scale_colour_brewer(
    palette = "Dark2",
    name    = "Voto"
  ) +
  labs(
    title    = "Sensibilidad social según voto",
    subtitle = "",
    x        = 'Acuerdo con "la mayoría de las personas pobres lo son porque no se esfuerzan lo suficiente"',
    y        = 'Acuerdo con "deberían aumentar los impuestos a las personas más ricas del país"'
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
