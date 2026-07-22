library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(openxlsx)
library(jsonlite)

config <- list(
  year = 2026,
  round = "R11",
  analysis_dir = file.path("data", "processed", "analysis", "2026", "R11"),
  code_dir = file.path("code", "analysis", "2026", "R11"),
  base_xlsx = file.path("data", "processed", "analysis", "2026", "R11", "R11_codificada.xlsx"),
  output_xlsx = file.path("data", "processed", "analysis", "2026", "R11", "R11_tablas_contingencia.xlsx")
)

segmento_cols <- c("Canelones", "Interior Coalición", "Interior Frente Amplio", "Montevideo")
voto2_cols <- c("CM", "FA")
cross_cols <- c(segmento_cols, voto2_cols, "Total")

definitions_raw <- fromJSON(file.path(config$code_dir, "definitions.json"), simplifyDataFrame = FALSE)

write_codebook_csvs <- function(definitions_raw) {
  iwalk(definitions_raw, function(items, q) {
    codebook <- map_dfr(items, \(item) {
      tibble(
        codigo = item$category,
        descripcion = item$definition
      )
    })

    write_csv(
      codebook,
      file.path(config$analysis_dir, paste0("codigos_", q, ".csv")),
      na = ""
    )
  })
}

read_codebook <- function(q) {
  items <- definitions_raw[[q]]
  if (is.null(items)) {
    stop("No hay definiciones para ", q)
  }

  map_dfr(items, \(item) {
    tibble(
      codigo = item$category,
      descripcion = item$definition
    )
  })
}

question_specs <- list(
  q1 = list(
    var = "q1",
    tipo = "cerrada",
    row_header = "Respuesta",
    source = "Opciones de la pauta R11",
    niveles = c(
      "Si, conozco lo que hay en mi zona",
      "Conozco bastante, no todo",
      "Conozco algunos centros",
      "No conozco nada"
    )
  ),
  q2 = list(
    var = "q2",
    tipo = "cerrada",
    row_header = "Respuesta",
    source = "Opciones de la pauta R11",
    niveles = c("Sí", "Más o menos", "No", "No sé")
  ),
  q3 = list(
    var = "codigo_q3",
    tipo = "abierta",
    row_header = "Código",
    source = file.path(config$analysis_dir, "codigos_q3.csv")
  ),
  q4 = list(
    var = "q4",
    tipo = "cerrada",
    row_header = "Respuesta",
    source = "Opciones de la pauta R11",
    niveles = c("Sí", "No")
  ),
  q5 = list(
    var = "codigo_q5",
    tipo = "abierta",
    row_header = "Código",
    source = file.path(config$analysis_dir, "codigos_q5.csv")
  ),
  q6 = list(
    var = "codigo_q6",
    tipo = "abierta",
    row_header = "Código",
    source = file.path(config$analysis_dir, "codigos_q6.csv")
  ),
  q7 = list(
    var = "codigo_q7",
    tipo = "abierta",
    row_header = "Código",
    source = file.path(config$analysis_dir, "codigos_q7.csv")
  ),
  q8 = list(
    var = "q8",
    tipo = "cerrada",
    row_header = "Respuesta",
    source = "Opciones de la pauta R11",
    niveles = c(
      "Muy en desacuerdo",
      "En desacuerdo",
      "Ni de acuerdo ni en desacuerdo",
      "De acuerdo",
      "Muy de acuerdo"
    ),
    value_map = c(
      "Ni acuerdo  ni descuerdo" = "Ni de acuerdo ni en desacuerdo",
      "Ni acuerdo ni descuerdo" = "Ni de acuerdo ni en desacuerdo"
    )
  ),
  q9 = list(
    var = "codigo_q9",
    tipo = "abierta",
    row_header = "Código",
    source = file.path(config$analysis_dir, "codigos_q9.csv")
  ),
  q10 = list(
    var = "q10",
    tipo = "cerrada",
    row_header = "Respuesta",
    source = "Opciones de la pauta R11",
    niveles = c(
      "Confío plenamente",
      "Confío",
      "Ni una cosa ni la otra",
      "No confío",
      "No confío en absoluto"
    )
  ),
  q11 = list(
    var = "q11",
    tipo = "cerrada",
    row_header = "Respuesta",
    source = "Opciones de la pauta R11",
    niveles = c("Sí", "No")
  ),
  q12 = list(
    var = "codigo_q12",
    tipo = "abierta",
    row_header = "Código",
    source = file.path(config$analysis_dir, "codigos_q12.csv")
  ),
  q13 = list(
    var = "codigo_q13",
    tipo = "abierta",
    row_header = "Código",
    source = file.path(config$analysis_dir, "codigos_q13.csv")
  )
)

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

ordered_values <- function(long, spec, q) {
  present <- sort(unique(long$valor))

  if (spec$tipo == "cerrada") {
    return(c(spec$niveles, setdiff(present, spec$niveles)))
  }

  codebook <- read_codebook(q)
  c(codebook$codigo, setdiff(present, codebook$codigo))
}

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

definitions_matrix <- function(spec, q) {
  if (spec$tipo == "abierta") {
    codebook <- read_codebook(q)
    return(
      bind_rows(
        tibble(codigo = paste0("Códigos - ", q), descripcion = ""),
        tibble(codigo = "Código", descripcion = "Definición"),
        codebook
      )
    )
  }

  tibble(
    respuesta = c(paste0("Opciones - ", q), "Respuesta", spec$niveles)
  )
}

write_table_parts <- function(wb, sheet, parts, start_row, start_col) {
  writeData(
    wb,
    sheet = sheet,
    x = as.data.frame(t(parts$header_1), check.names = FALSE),
    startRow = start_row,
    startCol = start_col,
    colNames = FALSE,
    rowNames = FALSE,
    keepNA = FALSE
  )
  writeData(
    wb,
    sheet = sheet,
    x = as.data.frame(t(parts$header_2), check.names = FALSE),
    startRow = start_row + 1L,
    startCol = start_col,
    colNames = FALSE,
    rowNames = FALSE,
    keepNA = FALSE
  )
  writeData(
    wb,
    sheet = sheet,
    x = parts$body,
    startRow = start_row + 2L,
    startCol = start_col,
    colNames = FALSE,
    rowNames = FALSE,
    keepNA = FALSE
  )
}

style_sheet <- function(wb, sheet, count_start, count_nrows, pct_title_row, pct_start, pct_nrows, def_nrows, open_defs) {
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

clean_orphan_drawing_relationships <- function(xlsx_path) {
  tmp_dir <- tempfile("xlsx_unzip_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  utils::unzip(xlsx_path, exdir = tmp_dir)

  rel_dir <- file.path(tmp_dir, "xl", "worksheets", "_rels")
  if (!dir.exists(rel_dir)) {
    return(invisible(TRUE))
  }

  rel_files <- list.files(
    rel_dir,
    pattern = "\\.rels$",
    full.names = TRUE
  )

  for (rel_file in rel_files) {
    rel <- paste(readLines(rel_file, warn = FALSE), collapse = "")
    rel <- gsub(
      '<Relationship[^>]+Type="[^"]*/drawing"[^>]*/>',
      "",
      rel
    )
    rel <- gsub(
      '<Relationship[^>]+Type="[^"]*/vmlDrawing"[^>]*/>',
      "",
      rel
    )
    writeLines(rel, rel_file, useBytes = TRUE)
  }

  files <- list.files(tmp_dir, all.files = TRUE, recursive = TRUE, no.. = TRUE)
  tmp_zip <- tempfile(fileext = ".xlsx")

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(tmp_dir)
  utils::zip(zipfile = tmp_zip, files = files, flags = "-r9Xq")
  setwd(old_wd)

  file.copy(tmp_zip, xlsx_path, overwrite = TRUE)
  unlink(tmp_zip)
}

dir.create(config$analysis_dir, recursive = TRUE, showWarnings = FALSE)
write_codebook_csvs(definitions_raw)

base <- read_excel(config$base_xlsx, sheet = 1, col_types = "text")

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
  values <- ordered_values(long, spec, q)
  count_table <- build_count_table(long, values)
  pct_table <- build_pct_table(count_table)

  counts_parts <- make_table_parts(spec$var, spec$row_header, count_table)
  pct_parts <- make_table_parts(spec$var, spec$row_header, pct_table)
  defs <- definitions_matrix(spec, q)

  count_start <- 7L
  count_nrows <- nrow(counts_parts$body) + 2L
  pct_nrows <- nrow(pct_parts$body) + 2L
  pct_title_row <- count_start + count_nrows + 2L
  pct_start <- pct_title_row + 2L

  writeData(wb, q, paste0("R11 2026 - ", q), startRow = 1, startCol = 1, colNames = FALSE)
  writeData(wb, q, paste0("Base usada para cruces: ", config$base_xlsx), startRow = 2, startCol = 1, colNames = FALSE)
  writeData(wb, q, paste0("Casos en base: ", nrow(base)), startRow = 3, startCol = 1, colNames = FALSE)
  writeData(wb, q, paste0("Fuente bloque: ", spec$source), startRow = 4, startCol = 1, colNames = FALSE)
  writeData(wb, q, paste0(q, " - Conteos"), startRow = 5, startCol = 1, colNames = FALSE)
  write_table_parts(wb, q, counts_parts, count_start, 1)
  writeData(wb, q, paste0(q, " - Porcentajes por columna"), startRow = pct_title_row, startCol = 1, colNames = FALSE)
  write_table_parts(wb, q, pct_parts, pct_start, 1)
  writeData(
    wb,
    q,
    defs,
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

saveWorkbook(wb, config$output_xlsx, overwrite = TRUE)
clean_orphan_drawing_relationships(config$output_xlsx)

cat("Archivo generado: ", config$output_xlsx, "\n", sep = "")
cat("Base usada: ", config$base_xlsx, "\n", sep = "")
cat("Hojas: ", paste(names(question_specs), collapse = ", "), "\n", sep = "")
