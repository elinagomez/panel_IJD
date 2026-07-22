library(tidyverse)
library(googlesheets4)
library(googledrive)
library(glue)
library(googledrive)
library(googlesheets4)

participantes <- read_csv("data/raw/contacts/20260504.csv")


# questions (Base personal, la diferencia con la oficial es que esta desagregado por id ronda etc)
questions <- drive_get(
  "https://drive.google.com/drive/u/0/folders/1Cmr-xzG4noYTjz36dI3jQNhIuWovg3OR"
) |>
  drive_ls() |>
  filter(name == "questions") |>
  pull(id) |>
  googlesheets4::read_sheet() |>
  filter(CLIENTE == "ANEP")

  
  # importo bases
paths <- list(
    R7  = file.path("data", "processed", "analysis", "2026", "R7", "R7_codificada.xlsx"),  
    R10 = file.path("data", "processed", "analysis", "2026", "R10", "R10_codificada.xlsx"), 
    R11 = file.path("data", "processed", "analysis", "2026", "R11", "R11_codificada.xlsx"), 
    R14 = file.path("data", "processed", "analysis", "2026", "R14", "R14_codificada_corregida.xlsx")
  )
  
bases <- map(paths, readxl::read_excel)
  
questions_anep <- split(questions$Pregunta_id, questions$Ronda_id)
  
# Filtro las preguntas de cada ronda ANEP
bases <- imap(
  bases,
  \(base, name) {
  
    q <- questions_anep[[name]]
    
    if (is.null(q)) {
      warning("No hay preguntas para ", name)
      return(base |> select(numero))
    }

    patron <- paste0(
      "(^|_)", q, "($|[a-z]|_)",
      collapse = "|"
    )
    base |>
      select(numero, matches(patron))
  }
)

# Le cambio los nombres por el id de ronda + pregunta
bases <- imap(
  bases,
  \(base, name) {

    dic <- questions |>
      filter(Ronda_id == name) |>
      select(Pregunta_id, id)

    base |> 
      rename_with(
        \(x) {
          reduce2(
            dic$Pregunta_id,
            dic$id,
            \(nm, old, new)
              stringr::str_replace(nm, old, new),
            .init = x)
        }
      )
  }
)

# Cambio categorias
# R7
cat_r7_q10 <- questions |> 
  filter(id == "r7_q10") |>
  pull(Categorias) |> 
  str_split("\n") |> 
  unlist() |> 
  str_squish() |> 
  tibble(raw = _) |> 
  mutate(
    texto = str_replace(raw, "^[A-Za-z0-9]+[:.=]\\s*", ""),
    categoria = LETTERS[row_number()]
  )


bases$R7 <- bases$R7 |> 
  mutate(
    r7_q10 = cat_r7_q10$texto[
      match(str_squish(r7_q10), cat_r7_q10$categoria)
    ]
  )

# Base Final
ANEP <- participantes |> 
  mutate(numero = as.character(numero)) |> 
  full_join(bases$R7, by = "numero") |>
  full_join(bases$R10, by = "numero") |>
  full_join(bases$R11, by = "numero") |>
  full_join(bases$R14, by = "numero") |> 
  filter(!if_all(contains("q"), is.na)) 
    
outpath <- "data/processed/analysis/2026/ANEP/BaseAnep.xlsx"

writexl::write_xlsx(ANEP, outpath)
