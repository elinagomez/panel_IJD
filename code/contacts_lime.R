# ==============================================================================
# contacts_lime.R  —  Paso 0 del flujo
#
# Construye la base de contactos del panel a partir de la encuesta telefónica
# de LimeSurvey (línea de base CISCo-IJD). Esa encuesta es el reclutamiento del
# panel: de ahí salen el celular y todos los demográficos con los que después se
# cruzan las respuestas de WhatsApp.
#
# Input : data/raw/limesurvey/resultados_acumulados_depuracion.xlsx
#         (hoja "Base consolidada"; la primera fila es un segundo encabezado)
# Output: data/raw/contacts/<AAAAMMDD>.csv
#
# Correr desde la raíz del proyecto:  Rscript code/contacts_lime.R
# ==============================================================================

library(readxl)
library(dplyr)
library(readr)

fecha_corte <- "20260807"   # fecha de la extracción a la que corresponde este corte
xlsx  <- "data/raw/limesurvey/resultados_acumulados_depuracion.xlsx"
salida <- paste0("data/raw/contacts/", fecha_corte, ".csv")

bc <- read_excel(xlsx, sheet = "Base consolidada", col_types = "text")
bc <- bc[-1, ]   # la fila 1 repite los nombres de variable en formato "etiqueta"

# celular en la base viene con 8 dígitos; el export de WhatsApp usa 598XXXXXXXX
key <- gsub("\\D", "", bc$celular)
key <- substr(key, nchar(key) - 7, nchar(key))

contacts <- tibble(
  numero            = paste0("598", key),
  key               = key,
  id_lime           = bc$id,
  fecha_encuesta    = bc$fecha,
  nombre            = bc$nombre,
  edad              = bc$edad,
  genero            = bc$S1,      # ¿Cuál es su género?
  sexo              = bc$S4,      # sexo asignado al nacer
  ascendencia       = bc$S5,
  pais_nac          = bc$S3,
  departamento      = bc$S6,
  barrio            = bc$S7,
  n_educativo       = bc$ES1,
  situacion_laboral = bc$T1,
  ideologia         = bc$V1,      # autoubicación izquierda(1)-derecha(10)
  voto              = bc$V2,      # partido votado en octubre 2024
  correo            = bc$correo,
  otro_numero       = bc$otro_numero,
  consent_wsp       = bc$consentimiento_2,  # consiente ser recontactado por WhatsApp
  completada        = bc$completada
) |>
  filter(nchar(key) == 8) |>
  distinct(key, .keep_all = TRUE)   # ante celulares duplicados se conserva el primer registro

dir.create(dirname(salida), recursive = TRUE, showWarnings = FALSE)
write_csv(contacts, salida)

message(sprintf("contactos escritos: %s (%d filas)", salida, nrow(contacts)))
