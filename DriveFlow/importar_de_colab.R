# =============================================================================
# DriveFlow/importar_de_colab.R   —  paso 3 de 3 de la codificacion en Colab
#
# Baja los codigos que dejo el notebook, los pega a la base de la ronda y deja
# la muestra del 20% para revisar a mano.
#
#   Análisis/C/<ronda>/codigos_<ronda>.csv  ->  data/processed/analysis/...
#
# Correr desde la raiz del proyecto:  Rscript DriveFlow/importar_de_colab.R
# =============================================================================

source("DriveFlow/qualcode.R")

CONFIG <- load_config("DriveFlow/R1.yml")

# ---- 1. Bajar el resultado del notebook -------------------------------------

autenticar(CONFIG)

id_ronda <- carpeta_ronda(CONFIG$drive$folder_codificacion_id, CONFIG$round)
nombre   <- paste0("codigos_", CONFIG$round, ".csv")

archivos <- googledrive::drive_ls(googledrive::as_id(id_ronda))
hit <- archivos[archivos$name == nombre, ]
if (nrow(hit) == 0) stop("No encontre ", nombre, " en Drive: ¿ya corrio el notebook?")

dir_local <- file.path(CONFIG$out_dir, "colab")
dir.create(dir_local, recursive = TRUE, showWarnings = FALSE)
f_codigos <- file.path(dir_local, nombre)

googledrive::drive_download(googledrive::as_id(hit$id[[1]]), path = f_codigos, overwrite = TRUE)

codigos <- readr::read_csv(f_codigos, col_types = readr::cols(
  fila = readr::col_integer(), pregunta = readr::col_character(), codigos = readr::col_character()
))

message(nrow(codigos), " respuestas codificadas por el notebook")

# ---- 2. Pegar a la base de la ronda ----------------------------------------

ins <- leer_insumos(CONFIG)
raw <- ins$raw
cfg <- make_cfg(raw = raw, questions = ins$questions, codebook = ins$codebook, CONFIG = CONFIG)

preguntas <- intersect(unique(codigos$pregunta), names(cfg))
cfg <- cfg[preguntas]

out <- raw

for (q in preguntas) {

  col_codigo <- paste0("codigo_", q)
  sub <- codigos[codigos$pregunta == q, ]

  # los codigos vuelven indexados por numero de fila de la base
  valores <- rep(NA_character_, nrow(out))
  fuera   <- sub$fila < 1 | sub$fila > nrow(out)
  if (any(fuera)) stop("el notebook devolvio filas fuera de rango para ", q)
  valores[sub$fila] <- sub$codigos

  out[[col_codigo]] <- valores
  out <- dplyr::relocate(out, dplyr::all_of(col_codigo), .after = dplyr::all_of(q))

  n_error <- sum(valores == "ERROR", na.rm = TRUE)
  n_ok    <- sum(!is.na(valores) & valores != "ERROR")
  message("\n", q, ": ", n_ok, " codificadas | ", n_error, " errores | ",
          sum(is.na(valores)), " sin respuesta")

  # ninguna etiqueta puede caer fuera del codebook
  usadas   <- unique(trimws(unlist(strsplit(valores[!is.na(valores) & valores != "ERROR"], "\\s*;\\s*"))))
  invalidas <- setdiff(usadas, cfg[[q]]$etiquetas)
  if (length(invalidas)) {
    warning(q, ": codigos que no estan en el codebook: ", paste(invalidas, collapse = ", "))
  }

  print(frecuencia_codigos(out, q))
}

# ---- 3. Guardar y armar la muestra de revision ------------------------------

dir.create(CONFIG$out_dir, recursive = TRUE, showWarnings = FALSE)

archivo_base <- file.path(CONFIG$out_dir, paste0(CONFIG$round, "_codificada"))
readr::write_csv(out, paste0(archivo_base, ".csv"))
if (requireNamespace("writexl", quietly = TRUE)) {
  writexl::write_xlsx(out, paste0(archivo_base, ".xlsx"))
}

revision <- muestra_revision(out, cfg, prop = 0.20)
archivo_rev <- file.path(CONFIG$out_dir, paste0("QA_", CONFIG$round, "_sample20.csv"))
readr::write_csv(revision, archivo_rev)

message("\nguardado:")
message("  ", paste0(archivo_base, ".csv"))
message("  ", archivo_rev, "  <- completar codigo_<q>_rev a mano")

if (isTRUE(CONFIG$subir_a_drive)) {
  folder <- CONFIG$drive$folder_analisis_id
  save_drive(out,      folder, CONFIG$output_spreadsheet, CONFIG$output_sheet)
  save_drive(revision, folder, paste0("QA_", CONFIG$round), "sample_20")
}

# Despues de completar a mano las columnas codigo_<q>_rev:
#   revisado <- readr::read_csv(archivo_rev, col_types = readr::cols(.default = "c"))
#   medir_acuerdo(revisado)
