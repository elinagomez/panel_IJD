library(readr)
library(dplyr)

campaign <- "r14_20260526_analytics"
survey <- read_csv(paste0("data/raw/campaigns_wcx/", campaign, ".csv"))

month <- "20260504"
contacts <- read_csv(paste0("data/raw/contacts/", month, ".csv"))

s <- contacts %>%
  inner_join(survey, by = c("numero" = "Teléfono de Envío"))

# s <- s %>% select(-c(12))
s <- s %>% filter(!is.na(`Caso: q16`))

names(s) <- sub("^Caso: ", "", names(s))

s %>% group_by(segmento) %>% count()

dir.create(file.path("data", "processed", "matched"), recursive = TRUE, showWarnings = FALSE)
write_csv(s, paste0("data/processed/matched/", campaign, ".csv"))
