source("DriveFlow/qualcode.R")

CONFIG <- load_config("DriveFlow/R9.yml")

raw <- read_spreadsheet(
  folder_url = CONFIG$folder_url,
  file_name = paste0("transcripcion_", CONFIG$round)
  )

questions <- read_spreadsheet(
  folder_url = CONFIG$questions_url,
  file_name = "questions",
) |> filter(Ronda_id == CONFIG$round)

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

  out[[paste0("codigo_", q)]] <- classify_question(
    raw = out,
    cfg = cfg[[q]],
    chunk_size = 20L
  )

  out <- dplyr::relocate(out, all_of(col_codigo), .after = all_of(q))

}