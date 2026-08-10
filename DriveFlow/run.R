# =============================================================================
# DriveFlow/run.R
# Codifica las preguntas abiertas de una ronda y deja la muestra de revision.
#
# Antes de correr:
#   1. ollama serve            (deja el servidor escuchando en localhost:11434)
#   2. ollama pull qwen3:8b
#   3. revisar data/raw/codebook/questions.xlsx y BookR1.xlsx
#
# Correr desde la raiz del proyecto:  Rscript DriveFlow/run.R
# o abriendo este archivo en RStudio y ejecutando por bloques.
# =============================================================================

source("DriveFlow/qualcode.R")

CONFIG <- load_config("DriveFlow/R1.yml")

# --- modo prueba -------------------------------------------------------------
# SOLO: preguntas a codificar. NULL = todas las abiertas.
#       Para el primer test conviene una sola: q5 es corta y tiene dependencia,
#       asi se prueba tambien el mecanismo de contexto de la cerrada previa.
# N_FILAS: cuantas filas usar. NULL = todas.
SOLO    <- c("q5")
N_FILAS <- 20

# ---- 1. Insumos -------------------------------------------------------------

ins       <- leer_insumos(CONFIG)
raw       <- ins$raw
questions <- ins$questions
codebook  <- ins$codebook

cfg <- make_cfg(raw = raw, questions = questions, codebook = codebook)

if (!is.null(SOLO)) {
  faltan <- setdiff(SOLO, names(cfg))
  if (length(faltan)) stop("SOLO pide preguntas que no estan configuradas: ", paste(faltan, collapse = ", "))
  cfg <- cfg[SOLO]
}

message("\nmodelo: ", CONFIG$provider, " / ", CONFIG$model)
message("preguntas a codificar: ", paste(names(cfg), collapse = ", "))
for (q in names(cfg)) {
  message("  ", q, ": ", length(cfg[[q]]$etiquetas), " etiquetas | dependencia: ",
          ifelse(is.na(cfg[[q]]$dependencia), "no", cfg[[q]]$dependencia))
}

# ---- 2. Verificar que el modelo responde -----------------------------------

q_test <- names(cfg)[1]
message("\nprueba de conexion con una sola respuesta de ", q_test, "...")

texto_test <- raw[[q_test]] |> na.omit() |> (\(v) v[nzchar(trimws(v))])() |> head(1)

if (length(texto_test)) {
  t0 <- Sys.time()
  res_test <- codificar_lote(
    prompt_sistema = make_prompt(cfg[[q_test]]),
    textos         = texto_test,
    etiquetas      = cfg[[q_test]]$etiquetas,
    CONFIG         = CONFIG
  )
  message("  respuesta: ", substr(texto_test, 1, 90), "...")
  message("  codigos:   ", res_test)
  message("  tiempo:    ", round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1), "s por respuesta")
  if (identical(res_test, "ERROR")) stop("el modelo no devolvio codigos validos: revisar que ollama serve este corriendo")
}

# ---- 3. Codificar -----------------------------------------------------------

out <- if (is.null(N_FILAS)) raw else head(raw, N_FILAS)

for (q in names(cfg)) {

  message("\nclasificando ", q, " (", nrow(out), " filas)")
  t0 <- Sys.time()

  col_codigo  <- paste0("codigo_", q)
  out[[col_codigo]] <- codificar_pregunta(raw = out, cfg = cfg[[q]], CONFIG = CONFIG)
  out <- dplyr::relocate(out, dplyr::all_of(col_codigo), .after = dplyr::all_of(q))

  message("  ", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), " min | ",
          sum(out[[col_codigo]] == "ERROR", na.rm = TRUE), " errores del modelo | ",
          sum(is.na(out[[col_codigo]])), " sin respuesta")

  print(frecuencia_codigos(out, q))
}

# ---- 4. Guardar -------------------------------------------------------------

dir.create(CONFIG$out_dir, recursive = TRUE, showWarnings = FALSE)

archivo_base <- file.path(CONFIG$out_dir, paste0(CONFIG$round, "_codificada"))
readr::write_csv(out, paste0(archivo_base, ".csv"))

if (requireNamespace("writexl", quietly = TRUE)) {
  writexl::write_xlsx(out, paste0(archivo_base, ".xlsx"))
}

# muestra para revisar a mano: completar la columna codigo_<q>_rev
revision <- muestra_revision(out, cfg, prop = 0.20)
archivo_rev <- file.path(CONFIG$out_dir, paste0("QA_", CONFIG$round, "_sample20.csv"))
readr::write_csv(revision, archivo_rev)

message("\nguardado:")
message("  ", paste0(archivo_base, ".csv"))
message("  ", archivo_rev, "  <- completar codigo_<q>_rev a mano")

# ---- 5. Subir a Drive (opcional) -------------------------------------------
# Requiere folder_url en DriveFlow/R1.yml y drive_auth() hecho.

if (isTRUE(CONFIG$subir_a_drive) && nzchar(CONFIG$folder_url %||% "")) {
  save_drive(out,      CONFIG$folder_url, CONFIG$output_spreadsheet, CONFIG$output_sheet)
  save_drive(revision, CONFIG$folder_url, paste0("QA_", CONFIG$round), "sample_20")
}

# ---- 6. Despues de revisar a mano ------------------------------------------
# Una vez completadas las columnas codigo_<q>_rev en el CSV de QA:
#
#   revisado <- readr::read_csv(archivo_rev, col_types = readr::cols(.default = "c"))
#   medir_acuerdo(revisado)
#
# exacto       = % de respuestas donde el modelo y la revision coinciden en todo
# algun_codigo = % donde comparten al menos un codigo
# jaccard      = solapamiento promedio entre los dos conjuntos de codigos
