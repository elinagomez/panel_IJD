# =============================================================================
# DriveFlow/exportar_para_colab.R   —  paso 1 de 3 de la codificacion en Colab
#
# Deja en Drive todo lo que el notebook necesita para codificar, ya resuelto:
# los prompts armados y una fila por respuesta a clasificar. Toda la logica
# (codebook, dependencias, validaciones, contexto de las cerradas) queda de
# este lado, en R. El notebook solo ejecuta el modelo.
#
#   R  ->  Análisis/C/<ronda>/prompts_<ronda>.csv
#          Análisis/C/<ronda>/tareas_<ronda>.csv
#
# Correr desde la raiz del proyecto:  Rscript DriveFlow/exportar_para_colab.R
# =============================================================================

source("DriveFlow/qualcode.R")

CONFIG <- load_config("DriveFlow/R1.yml")

# preguntas a codificar (NULL = todas las abiertas) y filas (NULL = todas)
SOLO    <- NULL
N_FILAS <- NULL

# ---- 1. Insumos y validacion ------------------------------------------------

ins <- leer_insumos(CONFIG)
validar_insumos(ins, CONFIG)
guardar_snapshot(ins, CONFIG)

raw <- if (is.null(N_FILAS)) ins$raw else head(ins$raw, N_FILAS)
cfg <- make_cfg(raw = raw, questions = ins$questions, codebook = ins$codebook, CONFIG = CONFIG)

if (!is.null(SOLO)) {
  faltan <- setdiff(SOLO, names(cfg))
  if (length(faltan)) stop("SOLO pide preguntas que no estan configuradas: ", paste(faltan, collapse = ", "))
  cfg <- cfg[SOLO]
}

message("\npreguntas a exportar: ", paste(names(cfg), collapse = ", "))

# ---- 2. Armar tareas y prompts ----------------------------------------------
# Una tarea = una respuesta a clasificar. El "grupo" identifica el prompt que
# le corresponde: para las preguntas con dependencia hay un prompt por opcion
# de la cerrada previa.

tareas  <- list()
prompts <- list()

for (q in names(cfg)) {

  cfg_q <- cfg[[q]]
  texto <- as.character(raw[[q]])
  vacio <- is.na(texto) | trimws(texto) == ""

  con_dep <- !is.na(cfg_q$dependencia) && cfg_q$dependencia %in% names(raw)
  grupo   <- if (con_dep) as.character(raw[[cfg_q$dependencia]]) else rep("__todos__", nrow(raw))
  grupo[is.na(grupo)] <- "__sin_dato__"

  for (g in unique(grupo[!vacio])) {

    idx <- which(!vacio & grupo == g)

    categoria <- if (con_dep && g != "__sin_dato__") etiqueta_cerrada(cfg_q, g) else NULL

    prompts[[length(prompts) + 1]] <- tibble(
      pregunta       = q,
      grupo          = g,
      etiquetas      = paste(cfg_q$etiquetas, collapse = "|"),
      prompt_sistema = as.character(make_prompt(cfg_q, categoria_cerrada = categoria))
    )

    tareas[[length(tareas) + 1]] <- tibble(
      fila     = idx,
      pregunta = q,
      grupo    = g,
      texto    = texto[idx]
    )
  }
}

tareas  <- dplyr::bind_rows(tareas)
prompts <- dplyr::bind_rows(prompts)

message(nrow(tareas), " respuestas a codificar | ", nrow(prompts), " prompts distintos")
print(dplyr::count(tareas, pregunta, name = "respuestas"))

# ---- 3. Guardar local y subir a Drive --------------------------------------

dir_local <- file.path(CONFIG$out_dir, "colab")
dir.create(dir_local, recursive = TRUE, showWarnings = FALSE)

f_tareas  <- file.path(dir_local, paste0("tareas_",  CONFIG$round, ".csv"))
f_prompts <- file.path(dir_local, paste0("prompts_", CONFIG$round, ".csv"))

readr::write_csv(tareas,  f_tareas)
readr::write_csv(prompts, f_prompts)

autenticar(CONFIG)

id_c      <- CONFIG$drive$folder_codificacion_id
id_ronda  <- carpeta_ronda(id_c, CONFIG$round, crear = TRUE)

subir <- function(ruta, destino_id) {
  ya <- googledrive::drive_ls(googledrive::as_id(destino_id))
  viejo <- ya[ya$name == basename(ruta), ]
  if (nrow(viejo)) googledrive::drive_trash(googledrive::as_id(viejo$id[[1]]))
  googledrive::drive_upload(ruta, path = googledrive::as_id(destino_id), overwrite = FALSE)
  message("subido a Drive: ", basename(ruta))
}

subir(f_tareas,  id_ronda)
subir(f_prompts, id_ronda)

message("\nlisto. Abrir el notebook codificacion_colab.ipynb en Colab, ",
        "correrlo con GPU, y despues volver con DriveFlow/importar_de_colab.R")
