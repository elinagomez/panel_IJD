library(readr)
library(dplyr)

campaign <- "r14_20250905"
survey <- read_csv(paste0("data/raw/campaigns_wcx/", campaign,".csv"))

month <- "20250907"
contacts <- read_csv(paste0("data/raw/contacts/", month, ".csv"))

s <- contacts %>%
  inner_join(survey, by = "numero")

s <- s %>% select(-c(q0))
s <- s %>% filter(!is.na(q10))

s %>% group_by(segmento) %>% count()

write_csv(s, paste0("data/processed/matched/", campaign, ".csv"))
