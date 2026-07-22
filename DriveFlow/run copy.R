# =========================================================
# EJECUCIÓN
# =========================================================

source("DriveFlow/qualcode copy.R")

CONFIG <- load_config("DriveFlow/R14.yml")

raw <- read_spreadsheet(
  folder_url = CONFIG$folder_url,
  file_name = paste0("transcripcion_", CONFIG$round)
)

# raw |> View()

questions <- read_spreadsheet(
  folder_url = CONFIG$questions_url,
  file_name = "questions"
) |>
  filter(Ronda_id == CONFIG$round)


codebook <- read_spreadsheet(
  folder_url = CONFIG$folder_url,
  file_name = paste0("Book", CONFIG$round),
  sheet = CONFIG$round
)

cfg <- make_cfg(
  raw = raw,
  questions = questions,
  codebook = codebook
)

out <- raw

for (q in names(cfg)) {

  message("Clasificando ", q)

  col_codigo <- paste0("codigo_", q)

  out[[col_codigo]] <- classify_question(
    raw = out,
    cfg = cfg[[q]],
    chunk_size = CONFIG$chunk_size %||% 20L,
    model = CONFIG$model %||% "gpt-5-nano",
    provider = CONFIG$provider %||% "openai"
  )

  out <- relocate(
    out,
    all_of(col_codigo),
    .after = all_of(q)
  )
}

sample_review <- out |>
  select(numero, starts_with("q"), starts_with("codigo_")) |>
  slice_sample(prop = 0.20)

for (q in names(cfg)) {
  col_codigo <- paste0("codigo_", q)

  if (q %in% names(sample_review) && col_codigo %in% names(sample_review)) {
    sample_review <- sample_review |>
      relocate(all_of(col_codigo), .after = all_of(q))
  }
}


save_drive <- function(out, folder_url, spreadsheet, sheetname) {
  
  folder <- drive_get(as_id(folder_url))
  if (nrow(folder) == 0) stop(glue("No se encontró la carpeta en Drive."))
  
  ss <- drive_ls(path = as_id(folder_url)) |> filter(name == spreadsheet)
  
  if (nrow(ss) > 0) {
    ss_id <- ss$id[[1]]
    message(glue("Spreadsheet encontrado: '{spreadsheet}'"))
    
    # Sobreescribir sheet si ya existe
    existing <- sheet_names(ss_id)
    if (sheetname %in% existing) {
      sheet_delete(ss_id, sheetname)
      warning(glue("Sheet '{sheetname}' ya existía, se sobreescribió."))
    }
    sheet_add(ss_id, sheet = sheetname)
    
  } else {
    # Crear spreadsheet y renombrar la hoja por defecto en vez de agregar una nueva
    ss_new <- gs4_create(spreadsheet)
    drive_mv(ss_new, path = as_id(folder_url))
    ss_id <- as.character(ss_new)
    sheet_rename(ss_id, sheet = 1, new_name = sheetname)  # renombra "Sheet1"
    message(glue("Spreadsheet creado: '{spreadsheet}'"))
  }
  
  write_sheet(data = out, ss = ss_id, sheet = sheetname)
  
  message(glue("Sheet '{sheetname}' guardado en '{spreadsheet}'."))
  invisible(out)
}

save_drive(
  out = out,
  folder_url = CONFIG$folder_url,
  spreadsheet = CONFIG$output_spreadsheet,
  sheetname = CONFIG$output_sheet
)

save_drive(
  out = sample_review,
  folder_url = CONFIG$folder_url,
  spreadsheet = paste0("QA_", CONFIG$round),
  sheetname = "sample_20"
)

rio::export(out, file = "data/processed/analysis/2026/R14/R14_codificada.xlsx", format = "xlsx")
