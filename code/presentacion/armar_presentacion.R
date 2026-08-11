# =============================================================================
# code/presentacion/armar_presentacion.R
# Genera la presentacion de resultados de una ronda, entera, desde la base
# codificada. Para otra ronda: cambiar el bloque `ronda:` de project.yml.
#
# Salida: presentaciones/<ronda>_resultados.pptx
#         (10 x 5,625 pulgadas = el tamano de Google Slides, para subirlo
#          a la carpeta Presentaciones y abrirlo con Slides)
#
# Correr desde la raiz del proyecto:
#     Rscript code/presentacion/armar_presentacion.R
# =============================================================================

source("code/presentacion/presentacion.R")

library(yaml)
library(readr)

cfg   <- yaml::read_yaml("project.yml")
RONDA <- cfg$ronda$round_id
ANIO  <- cfg$ronda$year
DIR   <- file.path("data/processed/analysis", ANIO, RONDA)

MESES <- c("enero", "febrero", "marzo", "abril", "mayo", "junio",
           "julio", "agosto", "setiembre", "octubre", "noviembre", "diciembre")

# Cuanto detalle por pregunta. Con todos los cortes y todas las citas la
# presentacion pasa las 150 diapositivas y se vuelve inmanejable.
N_CORTES        <- cfg$presentacion$n_cortes         %||% 2   # cortes por pregunta
N_CITAS         <- cfg$presentacion$n_citas          %||% 2   # citas por codigo
N_CODIGOS_CITAS <- cfg$presentacion$n_codigos_citas  %||% 5   # codigos con citas

# ---- 1. Insumos -------------------------------------------------------------

f_base <- file.path(DIR, paste0(RONDA, "_codificada.csv"))
if (!file.exists(f_base)) stop("Falta la base codificada: ", f_base)

d <- read_csv(f_base, col_types = cols(.default = "c")) |> recodificar()

# El enunciado de cada pregunta y el codebook salen de la copia congelada que
# dejo la codificacion: asi la presentacion es reproducible sin tocar Drive.
ultimo <- function(patron) {
  archivos <- list.files(file.path(DIR, "insumos"), pattern = patron, full.names = TRUE)
  if (!length(archivos)) stop("No hay snapshot de insumos que matchee ", patron,
                              ": correr antes la codificacion")
  archivos[which.max(file.mtime(archivos))]
}

questions <- read_csv(ultimo("^questions_"), col_types = cols(.default = "c"))
codebook  <- read_csv(ultimo("^codebook_"),  col_types = cols(.default = "c"))

# nombre corto opcional del codigo, si el equipo agrego una columna al codebook
col_corta <- intersect(c("etiqueta_corta", "corta", "nombre"), names(codebook))
diccionario <- if (length(col_corta)) {
  codebook |> transmute(etiqueta, corta = .data[[col_corta[1]]])
} else NULL

# preguntas codificadas, en el orden de la hoja de preguntas
codificadas <- names(d)[str_detect(names(d), "^codigo_")] |> str_remove("^codigo_")
orden_hoja  <- tolower(str_squish(questions$Pregunta_id))
preguntas   <- intersect(orden_hoja, codificadas)

enunciado <- function(q) {
  i <- which(orden_hoja == q)
  if (!length(i)) return(q)
  questions$Pregunta[[i[1]]]
}

message("ronda ", RONDA, " | ", nrow(d), " personas | preguntas: ", paste(preguntas, collapse = ", "))

# ---- 2. Indicadores del panel ----------------------------------------------
# Se toman del reporte de cobertura si ya se corrio; si no, se calculan de la base.

leer_resumen <- function(archivo) {
  f <- file.path(DIR, archivo)
  if (file.exists(f)) read_csv(f, col_types = cols(.default = "c")) else NULL
}

resumen_personas <- leer_resumen("resumen_personas.csv")
resumen_casos    <- leer_resumen("resumen_casos.csv")

valor <- function(tabla, metrica) {
  if (is.null(tabla)) return(NA_character_)
  v <- tabla$valor[str_detect(tabla$metrica, fixed(metrica))]
  if (!length(v)) NA_character_ else v[1]
}

indicadores <- tibble::tribble(
  ~indicador,                              ~valor,
  "Personas contactadas",                  valor(resumen_personas, "contactadas"),
  "Respondieron al menos una pregunta",    valor(resumen_personas, "Respondieron parcial"),
  "Completaron el cuestionario",           valor(resumen_personas, "Completaron"),
  "Tasa de respuesta (%)",                 valor(resumen_personas, "Tasa de respuesta"),
  "Tasa de completitud (%)",               valor(resumen_personas, "Tasa de completitud"),
  "Respuestas totales",                    valor(resumen_casos, "Respuestas totales"),
  "  de las cuales, en audio",             valor(resumen_casos, "en audio")
) |>
  filter(!is.na(valor))

# ---- 3. Armado --------------------------------------------------------------

pres <- abrir_plantilla()

secciones <- c("Participantes del panel",
               "Indicadores de la ronda",
               paste0("Módulo ", seq_along(preguntas), ": ", toupper(preguntas)))

pres <- slide_caratula(
  pres,
  titulo    = "Resultados",
  subtitulo = paste0("Paneles longitudinales cualitativos · Ronda ", str_remove(RONDA, "^R")),
  fecha     = paste0(MESES[as.integer(format(Sys.Date(), "%m"))], " de ", format(Sys.Date(), "%Y")),
  logos     = c("assets/logo_ijd_azul.png")
)

pres <- slide_indice(pres, secciones)

# --- Participantes ---
pres <- slide_seccion(pres, "1", "Participantes del panel")

for (v in names(CORTES)) {
  sin_dato <- sum(is.na(d[[v]]))
  pres <- slide_grafico(
    pres,
    titulo = CORTES[[v]],
    gg     = g_composicion(d, v),
    bajada = paste0("Personas que respondieron la ronda (N = ", sum(!is.na(d[[v]])), ")"),
    nota   = if (sin_dato > 0) paste0(sin_dato, " personas sin dato en esta variable") else NULL
  )
}

# --- Indicadores ---
if (nrow(indicadores)) {
  pres <- slide_seccion(pres, "2", "Indicadores de la ronda")
  pres <- nueva(pres)
  pres <- encabezado(pres, "Cobertura de la ronda")
  pres <- texto(pres, paste0(indicadores$indicador, ":  ", indicadores$valor),
                left = 0.9, top = 1.5, width = 8.2, height = 3.4,
                size = 14, color = "grey20", interlinea = 1.8)
}

# --- Un modulo por pregunta ---
for (k in seq_along(preguntas)) {

  q <- preguntas[k]
  message("  modulo ", q)

  n_resp <- sum(!is.na(d[[paste0("codigo_", q)]]) & d[[paste0("codigo_", q)]] != "ERROR")

  # la diapositiva con el enunciado abre el modulo, no hace falta separador aparte
  pres <- slide_pregunta(pres, paste0("Módulo ", k, " · ", toupper(q)), enunciado(q), n_resp)

  # distribucion general
  pres <- slide_grafico(
    pres,
    titulo = "Códigos asignados",
    gg     = g_general(d, q, diccionario),
    bajada = paste0("Frecuencia de menciones · ", n_resp, " respuestas, hasta 3 códigos cada una"),
    nota   = "El total de menciones supera al de respuestas porque una respuesta puede recibir más de un código."
  )

  # cortes: solo los dos que mas diferencian, para no repetir el mismo grafico
  cortes <- cortes_relevantes(d, q, n = N_CORTES)
  message("    cortes elegidos: ", paste(cortes, collapse = ", "))

  for (v in cortes) {
    gg <- g_corte(d, q, v, diccionario)
    if (!is.null(gg)) {
      pres <- slide_grafico(pres, paste0("Códigos según ", tolower(CORTES[[v]])), gg,
                            bajada = "Códigos con 5 o más menciones")
    }
  }

  # Correspondencias: se prueba con los cortes en orden de asociacion y se usa el
  # primero viable. Una variable de dos categorias, como sexo, no da un plano
  # informativo, asi que se pasa a la siguiente.
  for (v in cortes_relevantes(d, q, n = 4)) {
    gg <- ca_pregunta(d, q, v, diccionario)
    if (!is.null(gg)) {
      inercia <- attr(gg, "inercia")
      pres <- slide_grafico(
        pres,
        titulo = paste0("Correspondencias: códigos y ", tolower(CORTES[[v]])),
        gg     = gg,
        bajada = paste0("Dim 1: ", inercia[1], "% de la inercia · Dim 2: ", inercia[2], "%"),
        nota   = "La proximidad indica asociación entre un código y una categoría, no causalidad."
      )
      break
    }
  }

  # citas de los codigos principales
  pres <- slide_citas(pres, "Citas por código",
                      citas(d, q, n_por_codigo = N_CITAS, n_codigos = N_CODIGOS_CITAS))
}

# --- Cierre ---
pres <- nueva(pres)
pres <- encabezado(pres, "Nota metodológica")
pres <- texto(pres, c(
  "Las respuestas de audio fueron transcritas automáticamente y las citas se reproducen sin editar.",
  "Las respuestas abiertas se codificaron contra un codebook definido por el equipo; cada respuesta puede recibir hasta tres códigos, ordenados por relevancia.",
  "Una muestra del 20% de la codificación fue revisada manualmente.",
  "Los gráficos muestran frecuencias absolutas (N), no porcentajes."
), left = 0.7, top = 1.4, width = 8.6, height = 3.6, size = 12, color = "grey25", interlinea = 1.7)

# ---- 4. Guardar -------------------------------------------------------------

dir.create("presentaciones", showWarnings = FALSE)
salida <- file.path("presentaciones", paste0(RONDA, "_resultados.pptx"))
print(pres, target = salida)

message("\nlisto: ", salida, " (", length(pres), " diapositivas)")
message("Subirlo a la carpeta Presentaciones en Drive y abrirlo con Google Slides.")

# Subida opcional a Drive
if (isTRUE(cfg$presentacion$subir_a_drive) && nzchar(cfg$drive$folder_presentaciones_id %||% "")) {
  googledrive::drive_auth(email = cfg$drive$account_email)
  googledrive::drive_upload(salida, path = googledrive::as_id(cfg$drive$folder_presentaciones_id),
                            overwrite = TRUE)
  message("subida a Drive")
}

