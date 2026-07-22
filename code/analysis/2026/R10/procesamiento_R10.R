
library(tidyverse)

raw <- rio::import("data/processed/transcriptions/output/2026/transcripcion_R10.xlsx")

# raw |> count() > 133

# R10 procesada 
R10 <- raw |>
  mutate( 
    q4 = case_when(
   is.na(q3) ~ NA,
   !q3 %in% c(1,2) ~ "99",
  TRUE ~ q4),
  q10 = case_when(
   is.na(q9) ~ NA,
   !q9 %in% c("Sí", "Más o menos") ~ "99",
   TRUE ~ q10
  ),
  q10 = case_when(
   !is.na(q9) & is.na(q10) ~ " ",
   is.na(q10) ~ NA,
   TRUE ~ q10
  ),
  q12 = case_when(
   !is.na(q11) & is.na(q12) ~ " ",
   is.na(q12) ~ NA,
   TRUE ~ q12
  )
 ) |>
 select(- c(q13, q14)) 


# Elimino las personas que abandonaron
R10_NA <- R10 |> drop_na(starts_with("q")) 

# R10_NA |> count() > 111
  
# Diferencia
Dropped <- R10 |> anti_join(R10_NA, by = "numero") 

# count(R10) - count(R10_NA) == count(Dropped) > TRUE

rio::export(R10_NA, "data/processed/transcriptions/output/2026/transcripcion_R10_1.xlsx", format = "xlsx")

# Frankenstein con analytics

wise <- read_csv("C:/Users/Julian/Downloads/r10_20260427_analytics.csv") |> 
  mutate(numero = as.character(numero))

wise |> View()



cols <- intersect(names(wise), names(R10))
cols <- setdiff(cols, "numero")

resultado <- wise |>
  left_join(R10, by = "numero", suffix = c("", "_y"))

for (col in cols) {
  col_y <- paste0(col, "_y")
  
  resultado[[col]] <- as.character(resultado[[col]])
  resultado[[col_y]] <- as.character(resultado[[col_y]])
  
  resultado[[col]] <- ifelse(
    resultado[[col]] == "Ha enviado un Audio",
    resultado[[col_y]],
    resultado[[col]]
  )
}

resultado <- resultado |>
  select(-ends_with("_y"))


out <- resultado |> 
  select(-q2) |> 
  left_join(R10 |> select(numero, q2), by = "numero") |> 
  relocate(q2, .after = q1) 



out <- out |> 
  mutate( 
    q4 = case_when(
   is.na(q3) ~ NA,
   !q3 %in% c(1,2) ~ "99",
  TRUE ~ q4),
  q10 = case_when(
   is.na(q9) ~ NA,
   !q9 %in% c("Sí", "Más o menos") ~ "99",
   TRUE ~ q10
  ),
  q10 = case_when(
   !is.na(q9) & is.na(q10) ~ " ",
   is.na(q10) ~ NA,
   TRUE ~ q10
  ),
  q12 = case_when(
   !is.na(q11) & is.na(q12) ~ " ",
   is.na(q12) ~ NA,
   TRUE ~ q12
  )
)

rio::export(out, "data/processed/transcriptions/output/2026/transcripcion_R10_2.xlsx", format = "xlsx")

out |> count(segmento)

