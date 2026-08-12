# ==============================================================================
# matching.R  —  Paso 1 del flujo
#
# Cruza el export de la plataforma de WhatsApp con la base de contactos y deja
# un CSV listo para transcribir.
#
# Diferencias con el matching.R del repo panel_IJD:
#   - el export de esta cuenta es la vista COMPLETA de casos (70 columnas:
#     canal, fechas, SLA, contacto...), no la vista "analytics"
#     (Teléfono de Envío + Caso: qN). Se joinea por `Contacto: Teléfono`.
#   - se usa left_join desde la encuesta (no inner_join) para no perder los
#     audios de quienes todavía no están en la base de LimeSurvey; quedan
#     marcados con match_lime = 0.
#   - una persona puede tener varios casos (reenvíos): se conserva su caso más
#     completo, uno por teléfono.
#
# Input : data/raw/campaigns_wcx/<campaign>_casos.csv
#         data/raw/contacts/<fecha_corte>.csv
# Output: data/processed/matched/<campaign>.csv
#
# Correr desde la raíz del proyecto:  Rscript code/matching.R
# ==============================================================================

library(readr)
library(dplyr)
library(yaml)

cfg         <- yaml::read_yaml("project.yml")
campaign    <- cfg$ronda$campaign      # "r1_20260807"
fecha_corte <- "20260807"              # archivo de contactos a usar

survey   <- read_csv(paste0("data/raw/campaigns_wcx/", campaign, ".csv"),
                     col_types = cols(.default = "c"))
contacts <- read_csv(paste0("data/raw/contacts/", fecha_corte, ".csv"),
                     col_types = cols(.default = "c"))

# ---- clasificar respuestas (mismas reglas que code/analisis_panel.R) ---------
options(panel.skip.main = TRUE)
source("code/analisis_panel.R")

cols_q <- grep(PARAMS$patron_preguntas, names(survey), value = TRUE)
cols_q <- cols_q[order(as.numeric(sub(".*?([0-9]+)$", "\\1", cols_q)))]
clases <- as.data.frame(lapply(survey[cols_q], clasificar_celda))
names(clases) <- cols_q

# preguntas que nunca tuvieron datos (ej. q10) y casos de prueba
activas  <- cols_q[colSums(clases == "audio" | clases == "texto") > 0]
es_test  <- rowSums(clases == "placeholder") >= PARAMS$min_placeholders_test

s <- survey[!es_test, ]
cl <- clases[!es_test, activas, drop = FALSE]
s$n_resp  <- rowSums(cl == "audio" | cl == "texto")
s$n_audio <- rowSums(cl == "audio")

# clave de cruce: últimos 8 dígitos del teléfono
tel     <- gsub("\\D", "", s[[grep("^Contacto: Tel", names(s), value = TRUE)[1]]])
s$key   <- substr(tel, nchar(tel) - 7, nchar(tel))

# ---- quedarse con quienes respondieron algo, un caso por persona ------------
s <- s |>
  filter(n_resp > 0) |>
  arrange(key, desc(n_resp)) |>
  distinct(key, .keep_all = TRUE)

m <- s |>
  left_join(contacts, by = "key") |>
  mutate(
    numero       = coalesce(numero, paste0("598", key)),
    caso_id      = .data[["Caso: #"]],   # este reporte de analytics no trae "Caso: ID #"
    fecha_caso   = Creadas,
    etiqueta_wcx = .data[["Caso: Etiquetas"]],
    completa     = as.integer(n_resp == length(activas)),
    match_lime   = as.integer(!is.na(id_lime))
  )

# nombres de preguntas sin el prefijo "Caso: ", como en el repo original
names(m) <- sub("^Caso: ", "", names(m))

cols_final <- c(setdiff(names(contacts), "key"),
                "caso_id", "fecha_caso", "etiqueta_wcx",
                "n_resp", "completa", "match_lime",
                sub("^Caso: ", "", activas))
m <- m[, cols_final]

dir.create("data/processed/matched", recursive = TRUE, showWarnings = FALSE)
write_csv(m, paste0("data/processed/matched/", campaign, ".csv"))

message(sprintf(
  "matched: %d personas | %d completas | %d sin match en LimeSurvey | %d audios a transcribir",
  nrow(m), sum(m$completa), sum(m$match_lime == 0),
  sum(sapply(m[sub("^Caso: ", "", activas)],
             function(x) sum(grepl("^https?://", x, ignore.case = TRUE))))
))

m |> count(departamento, sort = TRUE) |> print(n = 20)
