dir <- "/Users/simonherrera/Documents/git/whatpanel/data/clean/postulantes/data"
files <- list.files(dir)
load(paste0(dir, "/", files[1]))
load(paste0(dir, "/", files[2]))
load(paste0(dir, "/", files[3]))
load(paste0(dir, "/", files[4]))
load(paste0(dir, "/", files[5]))
load(paste0(dir, "/", files[6]))
load(paste0(dir, "/", files[7]))
load(paste0(dir, "/", files[8]))
load(paste0(dir, "/", files[9]))
load(paste0(dir, "/", files[10]))
load(paste0(dir, "/", files[11]))

contacts <- list(f0, f00, f1, f2, f3, f4, f5, f6, f7, f8, f9) |> 
  purrr::map(~ dplyr::mutate(.x, marca_temporal = as.character(
     marca_temporal))) |>
      dplyr::bind_rows()

active <- readr::read_csv("data/acumulada/base_acumulada.csv")

# substract the instances that coincide in contacts from active by numbers in active$numero from contacts$contact
contacts2024 <- contacts |>
  dplyr::filter(!(contact %in% active$numero))

canelones <- contacts2024 |>
  dplyr::filter(depto_residencia == "CANELONES") |> 
  dplyr::filter(!is.na(contact))

readr::write_csv(canelones, "data/processed/canelones_2024.csv")
