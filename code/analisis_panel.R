# ==============================================================================
# analisis_panel.R
# Analisis de respuestas de panel (export de casos WhatsApp con columnas q1..qN)
#
# USO RAPIDO (desde la RAIZ del proyecto):
#     Rscript code/analisis_panel.R
#     Rscript code/analisis_panel.R data/raw/campaigns_wcx/otra_base.csv
#
# Toma el CSV mas reciente de data/raw/campaigns_wcx/ (o el que se pase como
# argumento), calcula todas las metricas y guarda las tablas en
# data/processed/analysis/<year>/<round_id>/
#
# Para el reporte HTML: quarto render code/reporte_panel.qmd
# ==============================================================================

# ---- Parametros -------------------------------------------------------------

PARAMS <- list(
  # Columnas de preguntas: se detectan por este patron sobre los nombres
  patron_preguntas = "q[0-9]+$",
  # Valores que NO son respuestas reales (placeholders / pruebas del formulario)
  patron_placeholder = "^(q[0-9]+|text|undefined|null|na|nan|-|\\.)$",
  # Un caso se considera de PRUEBA si tiene al menos este nro de placeholders
  min_placeholders_test = 5,
  # Patrones (regex) para identificar a la persona: se usa el primero que exista
  id_persona = c("^Contacto: Tel", "^Contacto: ID", "^Contacto: Email"),
  col_fecha = "Creadas",
  col_caso  = "Caso: ID #",
  col_etiqueta = "Caso: Etiquetas",
  # Excluir del denominador de completitud las preguntas 100% vacias
  ignorar_preguntas_vacias = TRUE
)

# ---- Utilidades -------------------------------------------------------------

#' Encuentra el CSV de export mas reciente
#' @param carpeta candidatas; se usa la primera que exista
csv_mas_reciente <- function(carpeta = c("data/raw/campaigns_wcx", ".")) {
  carpeta <- carpeta[dir.exists(carpeta)][1]
  if (is.na(carpeta)) stop("No existe la carpeta de exports")
  archivos <- list.files(carpeta, pattern = "\\.csv$", full.names = TRUE)
  archivos <- archivos[!grepl("_analytics\\.csv$", archivos)]  # el slim es derivado
  if (!length(archivos)) stop("No hay ningun .csv en: ", normalizePath(carpeta))
  archivos[which.max(file.mtime(archivos))]
}

#' Lee el export cuidando BOM y acentos
cargar_base <- function(ruta) {
  df <- utils::read.csv(
    ruta,
    check.names = FALSE, colClasses = "character",
    na.strings = c("", "NA"), fileEncoding = "UTF-8-BOM"
  )
  names(df) <- trimws(names(df))
  df
}

#' Clasifica el contenido de una celda de respuesta
#' @return "vacio" | "placeholder" | "audio" | "texto"
clasificar_celda <- function(x) {
  s <- trimws(ifelse(is.na(x), "", as.character(x)))
  out <- rep("texto", length(s))
  out[s == ""] <- "vacio"
  out[grepl(PARAMS$patron_placeholder, s, ignore.case = TRUE)] <- "placeholder"
  out[grepl("^https?://", s, ignore.case = TRUE)] <- "audio"
  out
}

# porcentaje vectorizado y seguro ante denominador 0
pct <- function(x, n) ifelse(is.na(n) | n == 0, 0, round(100 * x / n, 1))

# ---- Motor de analisis ------------------------------------------------------

#' Analiza una base de respuestas de panel
#' @param df data.frame crudo del export
#' @return lista con tablas de resumen (todas data.frame, listas para exportar)
analizar_panel <- function(df) {

  cols_q <- grep(PARAMS$patron_preguntas, names(df), value = TRUE)
  if (!length(cols_q)) stop("No se encontraron columnas de preguntas (patron: ", PARAMS$patron_preguntas, ")")
  # ordena q1, q2, ... q10 numericamente
  cols_q <- cols_q[order(as.numeric(sub(".*?([0-9]+)$", "\\1", cols_q)))]

  # matriz de clases (una fila por caso, una columna por pregunta)
  clases <- as.data.frame(lapply(df[cols_q], clasificar_celda),
                          stringsAsFactors = FALSE)
  names(clases) <- cols_q

  es_resp <- as.matrix(clases) %in% c("audio", "texto")
  es_resp <- matrix(es_resp, nrow = nrow(clases), dimnames = list(NULL, cols_q))

  # --- preguntas sin ningun dato (ej: q10 nunca se envio) ---
  preguntas_vacias <- cols_q[colSums(es_resp) == 0]
  cols_activas <- if (PARAMS$ignorar_preguntas_vacias) setdiff(cols_q, preguntas_vacias) else cols_q
  n_preguntas <- length(cols_activas)

  # --- deteccion de casos de prueba ---
  n_ph <- rowSums(clases == "placeholder")
  es_test <- n_ph >= PARAMS$min_placeholders_test

  id_col <- NA
  for (p in PARAMS$id_persona) {
    hit <- grep(p, names(df), value = TRUE)
    if (length(hit)) { id_col <- hit[1]; break }
  }
  if (is.na(id_col)) stop("No se encontro columna identificatoria de persona")

  casos_test <- data.frame(
    caso      = if (PARAMS$col_caso %in% names(df)) df[[PARAMS$col_caso]][es_test] else which(es_test),
    persona   = df[[id_col]][es_test],
    fecha     = if (PARAMS$col_fecha %in% names(df)) df[[PARAMS$col_fecha]][es_test] else rep(NA_character_, sum(es_test)),
    n_placeholders = n_ph[es_test],
    stringsAsFactors = FALSE
  )

  # --- base limpia ---
  keep    <- !es_test
  base    <- df[keep, , drop = FALSE]
  clases  <- clases[keep, , drop = FALSE]
  cl_act  <- clases[, cols_activas, drop = FALSE]

  n_resp  <- rowSums(cl_act == "audio" | cl_act == "texto")
  n_audio <- rowSums(cl_act == "audio")
  n_texto <- rowSums(cl_act == "texto")

  # ============================ NIVEL CASO ==================================
  n_casos <- nrow(base)
  resumen_casos <- data.frame(
    metrica = c("Envios (casos) totales", "Casos con al menos 1 respuesta",
                "Casos sin ninguna respuesta", "Casos completos",
                "Respuestas totales", "  - en audio", "  - en texto",
                "Celdas posibles (casos x preguntas)", "Tasa de llenado de celdas (%)",
                "Preguntas activas", "Preguntas sin ningun dato",
                "Casos de prueba excluidos"),
    valor = c(n_casos, sum(n_resp > 0), sum(n_resp == 0), sum(n_resp == n_preguntas),
              sum(n_resp), sum(n_audio), sum(n_texto),
              n_casos * n_preguntas, pct(sum(n_resp), n_casos * n_preguntas),
              n_preguntas, length(preguntas_vacias), sum(es_test)),
    stringsAsFactors = FALSE
  )

  # ============================ NIVEL PERSONA ===============================
  # Una persona puede tener varios casos (reenvios). Se toma su MEJOR caso:
  # el que tenga mas respuestas completadas.
  persona <- base[[id_col]]
  det <- data.frame(persona = persona, n_resp = n_resp, n_audio = n_audio,
                    n_texto = n_texto, stringsAsFactors = FALSE)
  ord <- det[order(det$persona, -det$n_resp), ]
  mejor <- ord[!duplicated(ord$persona), ]
  casos_x_persona <- as.data.frame(table(persona = det$persona),
                                   stringsAsFactors = FALSE)
  names(casos_x_persona)[2] <- "casos"
  mejor <- merge(mejor, casos_x_persona, by = "persona", all.x = TRUE)

  n_personas <- nrow(mejor)
  completo   <- mejor$n_resp == n_preguntas
  parcial    <- mejor$n_resp > 0 & mejor$n_resp < n_preguntas
  cero       <- mejor$n_resp == 0

  resumen_personas <- data.frame(
    metrica = c("Personas contactadas (unicas)",
                sprintf("Completaron las %d preguntas", n_preguntas),
                "Respondieron parcialmente", "No respondieron nada",
                "Tasa de respuesta (%)", "Tasa de completitud (%)",
                "Completitud entre quienes respondieron (%)",
                "Personas con mas de un envio"),
    valor = c(n_personas, sum(completo), sum(parcial), sum(cero),
              pct(sum(!cero), n_personas), pct(sum(completo), n_personas),
              pct(sum(completo), sum(!cero)), sum(mejor$casos > 1)),
    stringsAsFactors = FALSE
  )

  # Distribucion: cuantas preguntas contesto cada respondente
  resp <- mejor[!cero, , drop = FALSE]
  dist <- as.data.frame(table(preguntas_respondidas = resp$n_resp),
                        stringsAsFactors = FALSE)
  names(dist)[2] <- "personas"
  dist$preguntas_respondidas <- as.integer(dist$preguntas_respondidas)
  dist$pct_respondentes <- pct(dist$personas, nrow(resp))

  # Modalidad de respuesta por persona
  modalidad <- data.frame(
    modalidad = c("Solo texto", "Solo audio", "Mixto (audio + texto)"),
    personas = c(sum(resp$n_texto > 0 & resp$n_audio == 0),
                 sum(resp$n_audio > 0 & resp$n_texto == 0),
                 sum(resp$n_audio > 0 & resp$n_texto > 0)),
    stringsAsFactors = FALSE
  )
  modalidad$pct_respondentes <- pct(modalidad$personas, nrow(resp))

  # ============================ POR PREGUNTA ================================
  por_pregunta <- data.frame(
    pregunta = cols_q,
    audio    = sapply(clases, function(x) sum(x == "audio")),
    texto    = sapply(clases, function(x) sum(x == "texto")),
    placeholder = sapply(clases, function(x) sum(x == "placeholder")),
    vacio    = sapply(clases, function(x) sum(x == "vacio")),
    stringsAsFactors = FALSE
  )
  por_pregunta$respuestas <- por_pregunta$audio + por_pregunta$texto
  por_pregunta$pct_sobre_casos <- pct(por_pregunta$respuestas, n_casos)
  por_pregunta$pct_audio <- pct(por_pregunta$audio, por_pregunta$respuestas)
  por_pregunta$activa <- por_pregunta$pregunta %in% cols_activas
  rownames(por_pregunta) <- NULL

  # Embudo: cuantos casos llegaron hasta cada pregunta
  embudo <- por_pregunta[por_pregunta$activa, c("pregunta", "respuestas")]
  embudo$pct_vs_q1 <- pct(embudo$respuestas, embudo$respuestas[1])
  embudo$caida_vs_anterior <- c(NA, diff(embudo$respuestas))
  rownames(embudo) <- NULL

  # ============================ POR FECHA ===================================
  por_fecha <- NULL
  if (PARAMS$col_fecha %in% names(base)) {
    f <- as.Date(substr(base[[PARAMS$col_fecha]], 1, 10))
    por_fecha <- aggregate(list(casos = rep(1, length(f)),
                                con_respuesta = as.integer(n_resp > 0),
                                completos = as.integer(n_resp == n_preguntas),
                                respuestas = n_resp,
                                audios = n_audio),
                           by = list(fecha = f), FUN = sum)
    por_fecha$pct_respuesta <- pct(por_fecha$con_respuesta, por_fecha$casos)
  }

  # ============================ ETIQUETAS ===================================
  por_etiqueta <- NULL
  if (PARAMS$col_etiqueta %in% names(base)) {
    e <- ifelse(is.na(base[[PARAMS$col_etiqueta]]), "(sin etiqueta)",
                base[[PARAMS$col_etiqueta]])
    por_etiqueta <- aggregate(list(casos = rep(1, length(e)),
                                   con_respuesta = as.integer(n_resp > 0),
                                   completos = as.integer(n_resp == n_preguntas)),
                              by = list(etiqueta = e), FUN = sum)
    por_etiqueta$pct_respuesta <- pct(por_etiqueta$con_respuesta, por_etiqueta$casos)
  }

  # Detalle persona a persona (para revisar casos puntuales)
  detalle <- mejor[order(-mejor$n_resp, mejor$persona), ]
  detalle$estado <- ifelse(detalle$n_resp == n_preguntas, "completo",
                    ifelse(detalle$n_resp > 0, "parcial", "sin respuesta"))
  rownames(detalle) <- NULL

  list(
    n_preguntas = n_preguntas, cols_activas = cols_activas,
    preguntas_vacias = preguntas_vacias, id_col = id_col,
    resumen_casos = resumen_casos, resumen_personas = resumen_personas,
    distribucion = dist, modalidad = modalidad, por_pregunta = por_pregunta,
    embudo = embudo, por_fecha = por_fecha, por_etiqueta = por_etiqueta,
    detalle_personas = detalle, casos_test = casos_test
  )
}

#' Carpeta de salidas de la ronda vigente segun project.yml
dir_salidas <- function(cfg_path = "project.yml") {
  if (file.exists(cfg_path) && requireNamespace("yaml", quietly = TRUE)) {
    cfg <- yaml::read_yaml(cfg_path)
    if (!is.null(cfg$ronda$year) && !is.null(cfg$ronda$round_id))
      return(file.path("data/processed/analysis", cfg$ronda$year, cfg$ronda$round_id))
  }
  "salidas"
}

#' Guarda todas las tablas del analisis como CSV
exportar <- function(res, carpeta = dir_salidas()) {
  dir.create(carpeta, showWarnings = FALSE, recursive = TRUE)
  tablas <- c("resumen_casos", "resumen_personas", "distribucion", "modalidad",
              "por_pregunta", "embudo", "por_fecha", "por_etiqueta",
              "detalle_personas", "casos_test")
  for (t in tablas) {
    if (!is.null(res[[t]])) {
      utils::write.csv(res[[t]], file.path(carpeta, paste0(t, ".csv")),
                       row.names = FALSE, na = "")
    }
  }
  invisible(carpeta)
}

#' Imprime el resumen en consola
imprimir <- function(res) {
  linea <- function(t) cat("\n", t, "\n", strrep("-", nchar(t)), "\n", sep = "")
  linea("RESUMEN POR ENVIO (CASO)");    print(res$resumen_casos, row.names = FALSE)
  linea("RESUMEN POR PERSONA");         print(res$resumen_personas, row.names = FALSE)
  linea("PREGUNTAS RESPONDIDAS x PERSONA"); print(res$distribucion, row.names = FALSE)
  linea("MODALIDAD (audio vs texto)");  print(res$modalidad, row.names = FALSE)
  linea("POR PREGUNTA");                print(res$por_pregunta, row.names = FALSE)
  linea("EMBUDO DE ABANDONO");          print(res$embudo, row.names = FALSE)
  if (!is.null(res$por_fecha))    { linea("POR FECHA");   print(res$por_fecha, row.names = FALSE) }
  if (!is.null(res$por_etiqueta)) { linea("POR ETIQUETA"); print(res$por_etiqueta, row.names = FALSE) }
  if (nrow(res$casos_test)) {
    linea("CASOS DE PRUEBA EXCLUIDOS"); print(res$casos_test, row.names = FALSE)
  }
  if (length(res$preguntas_vacias)) {
    cat("\nAVISO: sin ningun dato ->", paste(res$preguntas_vacias, collapse = ", "),
        "(excluidas del denominador)\n")
  }
}

# ---- Ejecucion directa (Rscript analisis_panel.R [archivo.csv]) --------------
# El reporte .qmd hace options(panel.skip.main = TRUE) antes del source() para
# reusar solo las funciones. En RStudio (interactive()) tampoco corre solo.

if (!isTRUE(getOption("panel.skip.main")) && !interactive()) {
  args  <- commandArgs(trailingOnly = TRUE)
  ruta  <- if (length(args) >= 1) args[1] else csv_mas_reciente()
  cat("Base:", ruta, "\n")
  res <- analizar_panel(cargar_base(ruta))
  imprimir(res)
  destino <- exportar(res)
  cat("\nTablas guardadas en", destino, "\n")
}
