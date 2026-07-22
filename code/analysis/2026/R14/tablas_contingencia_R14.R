
library(tidyverse)
library(openxlsx)
library(glue)
library(readxl)
# =========================================================
# CONFIG
# =========================================================

analysis_dir <- file.path(
  "data", "processed", "analysis",
  CONFIG$year, CONFIG$round
)

output_xlsx <- file.path(
  analysis_dir,
  paste0(CONFIG$round, "_tablas_contingencia.xlsx")
)

dir.create(analysis_dir, recursive = TRUE, showWarnings = FALSE)

segmento_cols <- c(
  "Canelones",
  "Interior Coalición",
  "Interior Frente Amplio",
  "Montevideo"
)

voto2_cols <- c("CM", "FA")

cross_cols <- c(segmento_cols, voto2_cols, "Total")

# =========================================================
# HELPERS
# =========================================================

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

split_codes <- function(x) {
  if (is.na(x) || !nzchar(str_trim(x))) {
    return(character())
  }

  str_split(x, ";", simplify = FALSE)[[1]] |>
    str_trim() |>
    discard(~ !nzchar(.x))
}

normalise_closed <- function(x, spec) {
  x <- str_trim(as.character(x))

  if (!is.null(spec$value_map)) {
    mapped <- unname(spec$value_map[x])
    x <- ifelse(!is.na(mapped), mapped, x)
  }

  x
}

# =========================================================
# QUESTION SPECS DESDE QUESTIONS + CFG + OUT
# =========================================================

make_question_specs <- function(questions, cfg, out) {

  specs_df <- questions |>
    mutate(
      Pregunta_id = as.character(Pregunta_id),
      tipo_norm = str_to_lower(Tipo),
      var = if_else(
        tipo_norm == "abierta",
        paste0("codigo_", Pregunta_id),
        Pregunta_id
      ),
      row_header = if_else(tipo_norm == "abierta", "Código", "Respuesta"),
      source = if_else(
        tipo_norm == "abierta",
        paste0("codebook_", Pregunta_id),
        "questions"
      )
    ) |>
    filter(var %in% names(out))

  specs_lst <- specs_df |>
    group_by(Pregunta_id) |>
    group_split()

  names(specs_lst) <- map_chr(specs_lst, ~ .x$Pregunta_id[[1]])

  specs_lst |>
    map(function(x) {

      q <- x$Pregunta_id[[1]]
      tipo <- str_to_lower(x$Tipo[[1]])

      spec <- list(
        var = x$var[[1]],
        tipo = tipo,
        row_header = x$row_header[[1]],
        source = x$source[[1]],
        texto = x$Pregunta[[1]]
      )

      if (tipo == "cerrada") {
        spec$niveles <- parse_categorias(x$Categorias[[1]])
      }

      spec
    })
}

read_codebook_cfg <- function(q, cfg) {
  if (is.null(cfg[[q]])) {
    return(tibble(codigo = character(), descripcion = character()))
  }

  cfg[[q]]$diccionario |>
    transmute(
      codigo = etiqueta,
      descripcion = descripcion
    )
}

# =========================================================
# LONG DATA
# =========================================================

build_long <- function(base, spec) {

  if (spec$tipo == "cerrada") {
    return(
      base |>
        transmute(
          valor = normalise_closed(.data[[spec$var]], spec),
          segmento,
          voto2
        ) |>
        filter(!is.na(valor), nzchar(valor))
    )
  }

  base |>
    transmute(
      valor = map(.data[[spec$var]], split_codes),
      segmento,
      voto2
    ) |>
    unnest_longer(valor, values_to = "valor", keep_empty = FALSE) |>
    filter(!is.na(valor), nzchar(valor))
}

ordered_values <- function(long, spec, q, cfg) {

  present <- sort(unique(long$valor))

  if (spec$tipo == "cerrada") {
    niveles <- spec$niveles %||% character()
    return(c(niveles, setdiff(present, niveles)))
  }

  codebook <- read_codebook_cfg(q, cfg)

  c(codebook$codigo, setdiff(present, codebook$codigo))
}

# =========================================================
# TABLAS
# =========================================================

count_one <- function(long, values, group_col, group_values) {

  long |>
    filter(.data[[group_col]] %in% group_values) |>
    count(valor, grupo = .data[[group_col]], name = "n") |>
    complete(
      valor = values,
      grupo = group_values,
      fill = list(n = 0)
    ) |>
    mutate(
      valor = factor(valor, levels = values),
      grupo = factor(grupo, levels = group_values)
    ) |>
    arrange(valor, grupo) |>
    pivot_wider(names_from = grupo, values_from = n) |>
    mutate(valor = as.character(valor))
}

build_count_table <- function(long, values) {

  segmento_counts <- count_one(long, values, "segmento", segmento_cols)
  voto_counts <- count_one(long, values, "voto2", voto2_cols)

  total_counts <- long |>
    count(valor, name = "Total") |>
    complete(valor = values, fill = list(Total = 0)) |>
    mutate(valor = factor(valor, levels = values)) |>
    arrange(valor) |>
    mutate(valor = as.character(valor))

  body <- segmento_counts |>
    left_join(voto_counts, by = "valor") |>
    left_join(total_counts, by = "valor") |>
    rename(`__valor__` = valor) |>
    select(`__valor__`, all_of(cross_cols))

  totals <- body |>
    summarise(
      `__valor__` = "Total",
      across(all_of(cross_cols), ~ sum(.x, na.rm = TRUE))
    )

  bind_rows(body, totals)
}

build_pct_table <- function(count_table) {

  totals <- count_table |>
    filter(`__valor__` == "Total") |>
    select(all_of(cross_cols))

  pct <- count_table

  for (col in cross_cols) {
    denom <- totals[[col]][[1]]
    pct[[col]] <- if (is.na(denom) || denom == 0) NA_real_ else pct[[col]] / denom
  }

  pct
}

make_table_parts <- function(var_name, row_header, table_df) {

  body <- table_df |>
    rename(!!row_header := `__valor__`) |>
    as.data.frame(check.names = FALSE)

  list(
    header_1 = c(var_name, "segmento", "", "", "", "voto2", "", "Total"),
    header_2 = c(row_header, cross_cols),
    body = body
  )
}

definitions_matrix <- function(spec, q, cfg) {

  if (spec$tipo == "abierta") {
    codebook <- read_codebook_cfg(q, cfg)

    return(
      bind_rows(
        tibble(codigo = paste0("Códigos - ", q), descripcion = ""),
        tibble(codigo = "Código", descripcion = "Definición"),
        codebook
      )
    )
  }

  tibble(
    respuesta = c(
      paste0("Opciones - ", q),
      "Respuesta",
      spec$niveles %||% character()
    )
  )
}

# =========================================================
# ESCRITURA EXCEL
# =========================================================

write_table_parts <- function(wb, sheet, parts, start_row, start_col) {

  writeData(
    wb, sheet,
    x = as.data.frame(t(parts$header_1), check.names = FALSE),
    startRow = start_row,
    startCol = start_col,
    colNames = FALSE,
    rowNames = FALSE,
    keepNA = FALSE
  )

  writeData(
    wb, sheet,
    x = as.data.frame(t(parts$header_2), check.names = FALSE),
    startRow = start_row + 1L,
    startCol = start_col,
    colNames = FALSE,
    rowNames = FALSE,
    keepNA = FALSE
  )

  writeData(
    wb, sheet,
    x = parts$body,
    startRow = start_row + 2L,
    startCol = start_col,
    colNames = FALSE,
    rowNames = FALSE,
    keepNA = FALSE
  )
}

style_sheet <- function(wb, sheet, count_start, count_nrows,
                        pct_title_row, pct_start, pct_nrows,
                        def_nrows, open_defs) {

  title_style <- createStyle(
    fgFill = "#111827",
    fontColour = "#FFFFFF",
    textDecoration = "bold",
    fontSize = 13
  )

  meta_style <- createStyle(fontColour = "#4B5563", fontSize = 9)

  section_style <- createStyle(
    fgFill = "#D1D5DB",
    textDecoration = "bold",
    fontSize = 11
  )

  header_style <- createStyle(
    fgFill = "#E5E7EB",
    textDecoration = "bold",
    halign = "center",
    valign = "center",
    wrapText = TRUE,
    border = "TopBottomLeftRight",
    borderColour = "#D1D5DB"
  )

  subheader_style <- createStyle(
    fgFill = "#F3F4F6",
    textDecoration = "bold",
    halign = "center",
    valign = "center",
    wrapText = TRUE,
    border = "TopBottomLeftRight",
    borderColour = "#D1D5DB"
  )

  body_style <- createStyle(
    border = "TopBottomLeftRight",
    borderColour = "#D1D5DB",
    valign = "center",
    wrapText = TRUE
  )

  center_style <- createStyle(
    border = "TopBottomLeftRight",
    borderColour = "#D1D5DB",
    halign = "center",
    valign = "center"
  )

  pct_style <- createStyle(
    border = "TopBottomLeftRight",
    borderColour = "#D1D5DB",
    numFmt = "0.0%",
    halign = "center",
    valign = "center"
  )

  addStyle(wb, sheet, title_style, rows = 1, cols = 1:8, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet, meta_style, rows = 2:4, cols = 1, stack = TRUE)
  addStyle(wb, sheet, section_style, rows = c(5, pct_title_row), cols = 1:8, gridExpand = TRUE, stack = TRUE)

  addStyle(wb, sheet, header_style, rows = count_start, cols = 1:8, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet, subheader_style, rows = count_start + 1, cols = 1:8, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet, body_style, rows = (count_start + 2):(count_start + count_nrows - 1), cols = 1, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet, center_style, rows = (count_start + 2):(count_start + count_nrows - 1), cols = 2:8, gridExpand = TRUE, stack = TRUE)

  addStyle(wb, sheet, header_style, rows = pct_start, cols = 1:8, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet, subheader_style, rows = pct_start + 1, cols = 1:8, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet, body_style, rows = (pct_start + 2):(pct_start + pct_nrows - 1), cols = 1, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet, pct_style, rows = (pct_start + 2):(pct_start + pct_nrows - 1), cols = 2:8, gridExpand = TRUE, stack = TRUE)

  def_cols <- if (open_defs) 10:11 else 10

  addStyle(wb, sheet, header_style, rows = 5, cols = def_cols, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet, subheader_style, rows = 6, cols = def_cols, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet, body_style, rows = 7:(5 + def_nrows - 1), cols = def_cols, gridExpand = TRUE, stack = TRUE)

  setColWidths(wb, sheet, cols = 1, widths = 34)
  setColWidths(wb, sheet, cols = 2:8, widths = 15)
  setColWidths(wb, sheet, cols = 9, widths = 3)
  setColWidths(wb, sheet, cols = 10, widths = 34)
  setColWidths(wb, sheet, cols = 11, widths = 90)
}

# =========================================================
# GENERAR EXCEL
# =========================================================

# base <- out

base <- read_excel("data/processed/analysis/2026/R14/R14_codificada_corregida.xlsx")
#   mutate(
#     across(
#       c(q2, q3, q11),
#       ~ factor(
#         recode(
#           as.character(.),
#           "1" = "Muy de acuerdo",
#           "2" = "De acuerdo",
#           "3" = "Ni de acuerdo, ni desacuerdo",
#           "4" = "En desacuerdo",
#           "5" = "Muy en desacuerdo"
#         ),
#         levels = c(
#           "Muy de acuerdo",
#           "De acuerdo",
#           "Ni de acuerdo, ni desacuerdo",
#           "En desacuerdo",
#           "Muy en desacuerdo"
#         ),
#         ordered = TRUE
#       )
#     )
#   ) |> 
#   mutate(
#     q9 = recode(
#       as.character(q9),
#       "1" = "La protección de los niños, niñas y adolescentes es, antes que nada, responsabilidad de las familias. El Estado sólo debería intervenir en casos extremos.",
#       "2" = "La protección de los niños, niñas y adolescentes es una responsabilidad compartida entre las familias, el Estado y la sociedad en su conjunto.",
#       "3" = "El Estado es el principal responsable de garantizar la protección de los niños, niñas y adolescentes, independientemente del rol de las familias."
#   ),
#     q13 = recode(
#       as.character(q13),
#       "1" = "Cuidar el ambiente es un derecho, y el Estado debe garantizarlo aunque afecte a algunos sectores productivos.",
#       "2" = "Hay que encontrar un equilibrio entre el cuidado del ambiente y el desarrollo económico del país.",
#       "3" = "Primero está el trabajo y la producción; el cuidado del ambiente viene después."
#   )
# )

question_specs <- make_question_specs(
  questions = questions,
  cfg = cfg,
  out = base
)

required_cols <- c("segmento", "voto2", map_chr(question_specs, "var"))

missing_cols <- setdiff(required_cols, names(base))

if (length(missing_cols) > 0) {
  stop("Faltan columnas en la base codificada: ", paste(missing_cols, collapse = ", "))
}

wb <- createWorkbook()

for (q in names(question_specs)) {

  spec <- question_specs[[q]]

  addWorksheet(wb, q, gridLines = FALSE)

  long <- build_long(base, spec)

  values <- ordered_values(
    long = long,
    spec = spec,
    q = q,
    cfg = cfg
  )

  count_table <- build_count_table(long, values)
  pct_table <- build_pct_table(count_table)

  counts_parts <- make_table_parts(spec$var, spec$row_header, count_table)
  pct_parts <- make_table_parts(spec$var, spec$row_header, pct_table)

  defs <- definitions_matrix(
    spec = spec,
    q = q,
    cfg = cfg
  )

  count_start <- 7L
  count_nrows <- nrow(counts_parts$body) + 2L
  pct_nrows <- nrow(pct_parts$body) + 2L
  pct_title_row <- count_start + count_nrows + 2L
  pct_start <- pct_title_row + 2L

  writeData(wb, q, paste0(CONFIG$round, " ", CONFIG$year, " - ", q), startRow = 1, startCol = 1, colNames = FALSE)
  writeData(wb, q, paste0("Casos en base: ", nrow(base)), startRow = 2, startCol = 1, colNames = FALSE)
  writeData(wb, q, paste0("Pregunta: ", spec$texto), startRow = 3, startCol = 1, colNames = FALSE)
  writeData(wb, q, paste0("Fuente bloque: ", spec$source), startRow = 4, startCol = 1, colNames = FALSE)

  writeData(wb, q, paste0(q, " - Conteos"), startRow = 5, startCol = 1, colNames = FALSE)
  write_table_parts(wb, q, counts_parts, count_start, 1)

  writeData(wb, q, paste0(q, " - Porcentajes por columna"), startRow = pct_title_row, startCol = 1, colNames = FALSE)
  write_table_parts(wb, q, pct_parts, pct_start, 1)

  writeData(
    wb, q, defs,
    startRow = 5,
    startCol = 10,
    colNames = FALSE,
    rowNames = FALSE,
    keepNA = FALSE
  )

  style_sheet(
    wb = wb,
    sheet = q,
    count_start = count_start,
    count_nrows = count_nrows,
    pct_title_row = pct_title_row,
    pct_start = pct_start,
    pct_nrows = pct_nrows,
    def_nrows = nrow(defs),
    open_defs = spec$tipo == "abierta"
  )
}

analysis_dir <- file.path(
  "data",
  "processed",
  "analysis",
  CONFIG$year,
  CONFIG$round
)

dir.create(
  analysis_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

output_xlsx <- file.path(
  analysis_dir,
  paste0(CONFIG$round, "_tablas_contingencia.xlsx")
)


saveWorkbook(wb, output_xlsx, overwrite = TRUE)

cat("Archivo generado: ", output_xlsx, "\n", sep = "")
cat("Base usada: objeto `out`\n")
cat("Hojas: ", paste(names(question_specs), collapse = ", "), "\n", sep = "")

# base |>
#   janitor::tabyl(q2, segmento) |>
#   janitor::adorn_totals("row") 

# janitor::adorn_percentages("col") 
  # janitor::adorn_pct_formatting(digits = 1)
