library(vitals)
library(ellmer)
library(dplyr)
library(readr)
library(stringr)
library(yardstick)
library(tibble)
year <- 2025
round_id <- "R14"


# -------------------------------------------------------------------
# Logs (silencia el warning)
# -------------------------------------------------------------------
vitals::vitals_log_dir_set("./logs")  # crea ./logs si no existe

# -------------------------------------------------------------------
# Cargar archivo ya codificado
# -------------------------------------------------------------------
file_eval <- paste0("data/processed/analysis/", year, "/", round_id, "/R14_eval_q2.csv")
raw <- read_csv(file_eval, show_col_types = FALSE)

pick_first <- function(opts, nms) {
  x <- intersect(opts, nms)
  if (!length(x)) stop("No encontré ninguna de: ", paste(opts, collapse=", "))
  x[[1]]
}
col_manual <- pick_first(c("codigo_manual_q2","codigo_manual_llm","codigo_manual"), names(raw))
col_pred   <- pick_first(c("codigo_llm_q2","codigo_politico_q2","codigo_politico"), names(raw))
col_input  <- intersect(c("q2","texto","respuesta","comment"), names(raw))
col_input  <- if (length(col_input)) col_input[[1]] else NULL

datos <- raw |>
  filter(!is.na(.data[[col_manual]]), !is.na(.data[[col_pred]])) |>
  mutate(id = row_number())

dataset <- datos |>
  transmute(
    id,
    input   = if (!is.null(col_input)) .data[[col_input]] else as.character(.data[[col_pred]]),
    target  = as.character(.data[[col_manual]]),
    replay_ = as.character(.data[[col_pred]])  # guardo las preds para el solver
  )

# -------------------------------------------------------------------
# Solver "replay" CON turns sintéticos
#  - crea un Chat por fila
#  - setea turns: user = input; assistant = pred
# -------------------------------------------------------------------
pred_vector <- dataset$replay_

base_chat <- ellmer::chat_openai(
  model = "gpt-5-nano",
  system_prompt = "Replay solver (no API call)",
  api_key = Sys.getenv("OPENAI_API_KEY"),
  base_url = "https://api.openai.com/v1"
)

solver_replay <- function(inputs, ...) {
  n <- length(inputs)
  chats <- vector("list", n)
  for (i in seq_len(n)) {
    ch <- base_chat$clone(deep = TRUE)
    # Construir turns sintéticos
    user_turn <- ellmer::Turn("user", list(ellmer::ContentText(inputs[[i]])))
    asst_turn <- ellmer::Turn("assistant", list(ellmer::ContentText(pred_vector[[i]])))
    ch$set_turns(list(user_turn, asst_turn))  # historial con 1 intercambio
    chats[[i]] <- ch
  }
  list(
    result = pred_vector[seq_along(inputs)],
    solver_chat = chats
  )
}

# -------------------------------------------------------------------
# Scorer: exact match (case-insensitive)
# -------------------------------------------------------------------
scorer <- vitals::detect_exact(case_sensitive = FALSE)  # devuelve I/C listos para métricas. :contentReference[oaicite:0]{index=0}

metrics_task <- list(accuracy = function(score) mean(score == "C", na.rm = TRUE))

# -------------------------------------------------------------------
# Task + eval (solve -> score -> log)
# -------------------------------------------------------------------
tsk <- vitals::Task$new(
  dataset = dplyr::select(dataset, input, target),
  solver  = solver_replay,
  scorer  = scorer,
  metrics = metrics_task,
  name    = "R14_q2_codigos_replay"
)

tsk$eval(view = FALSE)  # orquesta solve/score/measure/log/view. :contentReference[oaicite:1]{index=1}

samples <- tsk$get_samples() |>
  mutate(result = str_squish(result),
         target = str_squish(target))

# -------------------------------------------------------------------
# Métricas macro + matriz de confusión
# -------------------------------------------------------------------
niveles <- sort(unique(c(samples$target, samples$result)))
dfm <- samples |>
  transmute(
    truth    = factor(target,   levels = niveles),
    estimate = factor(result,   levels = niveles)
  )

acc  <- accuracy(dfm, truth, estimate)
prec <- precision(dfm, truth, estimate, estimator = "macro")
rec  <- recall(dfm, truth, estimate, estimator = "macro")
f1   <- f_meas(dfm, truth, estimate, estimator = "macro")

cat("Accuracy:", acc$.estimate, "\n")
cat("Precision (macro):", prec$.estimate, "\n")
cat("Recall (macro):", rec$.estimate, "\n")
cat("F1 (macro):", f1$.estimate, "\n\n")

print(conf_mat(dfm, truth, estimate))
