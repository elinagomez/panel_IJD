# Paquetes
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(ggplot2)
library(sysfonts)
library(showtext)
library(purrr)
year <- 2025
round_id <- "R23"


pal_base <- c(
  "#4E79A7", "#59A14F", "#E15759", "#76B7B2", "#9C755F",
  "#B07AA1", "#F28E2B", "#EDC948", "#FF9DA7", "#BAB0AC",
  "#1F77B4", "#2CA02C", "#D62728", "#17BECF", "#8C564B",
  "#9467BD", "#BCBD22", "#7F7F7F", "#AEC7E8", "#98DF8A"
)

# Tipografía Montserrat con fallback sin red
base_font <- "montserrat"
try({
  font_add_google("Montserrat", "montserrat")
  showtext_auto()
}, silent = TRUE)
if (!("montserrat" %in% sysfonts::font_families())) {
  base_font <- "sans"
}

theme_set(
  theme_minimal(base_family = base_font) +
    theme(
      plot.title = element_text(face = "bold", size = 40),
      axis.text.x = element_text(angle = 20, hjust = 1, size = 30),
      axis.text.y = element_text(size = 30),
      axis.title = element_text(size = 35),
      legend.title = element_text(size = 35),
      legend.text = element_text(size = 30)
    )
)

dict_actuacion <- c(
  "actuacion correcta" = "Está actuando de manera correcta",
  "esta actuando de manera correcta" = "Está actuando de manera correcta",
  "actuacion incorrecta" = "No está actuando de manera correcta",
  "no esta actuando de manera correcta" = "No está actuando de manera correcta",
  "no sabe/no contesta" = "No tengo una opinión formada",
  "no tengo una opinion formada" = "No tengo una opinión formada"
)

# Metadatos de preguntas cerradas R23
preguntas_info <- list(
  q2 = list(
    titulo = "¿Cuál de estos tópicos del sistema de salud es el más relevante para ti?",
    niveles = c(
      "Acceso a medicamentos a través de los prestadores de salud",
      "Reducción de los tiempos de espera para consulta médica, exámenes, etc.",
      "Incompatibilidad de que Álvaro Danza trabaje en ASSE y en mutualistas",
      "Ampliación de los servicios de salud mental",
      "Beneficios que tuvo Círculo Católico en el gobierno anterior"
    ),
    paleta = c(
      "Acceso a medicamentos a través de los prestadores de salud" = "#4E79A7",
      "Reducción de los tiempos de espera para consulta médica, exámenes, etc." = "#59A14F",
      "Incompatibilidad de que Álvaro Danza trabaje en ASSE y en mutualistas"   = "#E15759",
      "Ampliación de los servicios de salud mental"                            = "#EDC948",
      "Beneficios que tuvo Círculo Católico en el gobierno anterior"           = "#9C755F"
    ),
    dict = c(
      "1" = "Acceso a medicamentos a través de los prestadores de salud",
      "2" = "Reducción de los tiempos de espera para consulta médica, exámenes, etc.",
      "3" = "Incompatibilidad de que Álvaro Danza trabaje en ASSE y en mutualistas",
      "4" = "Ampliación de los servicios de salud mental",
      "5" = "Beneficios que tuvo Círculo Católico en el gobierno anterior"
    )
  ),
  q4 = list(
    titulo = "¿Cómo evalúas el accionar de la oposición en el caso Danza?",
    niveles = c(
      "Está actuando de manera correcta",
      "No está actuando de manera correcta",
      "No tengo una opinión formada"
    ),
    paleta = c(
      "Está actuando de manera correcta"   = "#59A14F",
      "No está actuando de manera correcta" = "#E15759",
      "No tengo una opinión formada"        = "#BAB0AC"
    ),
    dict = dict_actuacion
  ),
  q6 = list(
    titulo = "¿Cómo evalúas el accionar del gobierno en el caso Danza?",
    niveles = c(
      "Está actuando de manera correcta",
      "No está actuando de manera correcta",
      "No tengo una opinión formada"
    ),
    paleta = c(
      "Está actuando de manera correcta"   = "#59A14F",
      "No está actuando de manera correcta" = "#E15759",
      "No tengo una opinión formada"        = "#BAB0AC"
    ),
    dict = dict_actuacion
  ),
  q8 = list(
    titulo = "¿Cómo debería resolverse definitivamente la situación de Álvaro Danza?",
    niveles = c(
      "Álvaro Danza debería dejar el cargo de presidente de ASSE",
      "Puede continuar aun con otros empleos privados en salud",
      "Puede continuar solo si deja sus empleos privados en salud",
      "No tengo una posición tomada"
    ),
    paleta = c(
      "Álvaro Danza debería dejar el cargo de presidente de ASSE" = "#E15759",
      "Puede continuar aun con otros empleos privados en salud"    = "#59A14F",
      "Puede continuar solo si deja sus empleos privados en salud" = "#F28E2B",
      "No tengo una posición tomada"                              = "#BAB0AC"
    ),
    dict = c(
      "1" = "Álvaro Danza debería dejar el cargo de presidente de ASSE",
      "2" = "Puede continuar aun con otros empleos privados en salud",
      "3" = "Puede continuar solo si deja sus empleos privados en salud",
      "4" = "No tengo una posición tomada"
    )
  ),
  q10 = list(
    titulo = "¿Cómo evalúas el papel de la JUTEP en este caso?",
    niveles = c(
      "Actuó con independencia y criterio técnico",
      "Fue presionada o influida políticamente",
      "No tengo una opinión formada"
    ),
    paleta = c(
      "Actuó con independencia y criterio técnico" = "#59A14F",
      "Fue presionada o influida políticamente"    = "#E15759",
      "No tengo una opinión formada"              = "#BAB0AC"
    ),
    dict = c(
      "1" = "Actuó con independencia y criterio técnico",
      "2" = "Fue presionada o influida políticamente",
      "3" = "No tengo una opinión formada"
    )
  )
)

preguntas <- names(preguntas_info)

# Helpers sin regex
norm_simple <- function(x) {
  x <- tolower(trimws(x))
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x <- gsub("'", "", x, fixed = TRUE)
  squash_one <- function(s) {
    if (is.na(s)) return(NA_character_)
    parts <- strsplit(s, " ", fixed = TRUE)[[1]]
    parts <- parts[parts != ""]
    if (length(parts) == 0) "" else paste(parts, collapse = " ")
  }
  vapply(x, squash_one, character(1))
}

map_by_dict <- function(v, dict) {
  if (is.null(dict) || length(dict) == 0) return(v)
  names(dict) <- norm_simple(names(dict))
  vn <- norm_simple(v)
  matches <- match(vn, names(dict))
  out <- v
  replace_idx <- !is.na(matches)
  out[replace_idx] <- dict[matches[replace_idx]]
  out
}

extend_dict_with_levels <- function(dict, levels_vec) {
  if (is.null(levels_vec)) return(dict)
  level_dict <- setNames(levels_vec, norm_simple(levels_vec))
  if (is.null(dict) || length(dict) == 0) {
    return(level_dict)
  }
  missing <- !names(level_dict) %in% names(dict)
  c(dict, level_dict[missing])
}

prepare_closed_questions <- function(data, specs) {
  for (q in names(specs)) {
    if (!q %in% names(data)) next
    spec <- specs[[q]]
    data[[q]] <- as.character(data[[q]])
    dict_full <- extend_dict_with_levels(spec$dict, spec$niveles)
    data[[q]] <- map_by_dict(data[[q]], dict_full)
    if (!is.null(spec$niveles)) {
      data[[q]] <- factor(data[[q]], levels = spec$niveles)
    }
  }
  data
}

get_palette <- function(q, cat_levels) {
  spec <- preguntas_info[[q]]
  pal <- spec$paleta
  if (is.null(cat_levels) || length(cat_levels) == 0) return(NULL)
  if (is.null(pal)) {
    n_cols <- min(length(cat_levels), length(pal_base))
    out <- pal_base[seq_len(n_cols)]
    names(out) <- cat_levels[seq_len(n_cols)]
    return(out)
  }
  pal <- pal[cat_levels]
  missing <- is.na(pal)
  if (any(missing)) {
    fallback <- pal_base[seq_len(sum(missing))]
    pal[missing] <- fallback
  }
  names(pal) <- cat_levels
  pal
}

# Datos R23
input_csv  <- paste0("data/processed/transcriptions/output/", year, "/transcripcion_", round_id, ".csv")
input_xlsx <- paste0("data/processed/transcriptions/output/", year, "/transcripcion_", round_id, ".xlsx")

if (file.exists(input_csv)) {
  df <- suppressMessages(readr::read_csv(input_csv, show_col_types = FALSE)) |> as_tibble()
} else if (file.exists(input_xlsx) && requireNamespace("openxlsx", quietly = TRUE)) {
  df <- as_tibble(openxlsx::read.xlsx(input_xlsx))
} else {
  stop("No se encontró el archivo de entrada de R23 en CSV ni XLSX.")
}

df <- prepare_closed_questions(df, preguntas_info)

dir.create(paste0("plots/", year, "/", round_id, ""), recursive = TRUE, showWarnings = FALSE)

stacked_plot <- function(data, q, group_var, file_stub) {
  if (!q %in% names(data) || !group_var %in% names(data)) return(invisible(NULL))
  spec <- preguntas_info[[q]]

  long <- data |>
    select(all_of(c(group_var, q))) |>
    rename(categoria = all_of(q)) |>
    filter(!is.na(categoria), !is.na(.data[[group_var]]))

  if (nrow(long) == 0) return(invisible(NULL))

  if (is.factor(long$categoria)) {
    cat_levels <- levels(long$categoria)
  } else if (!is.null(spec$niveles)) {
    cat_levels <- spec$niveles
  } else {
    cat_levels <- long |>
      count(categoria, name = "tot") |>
      arrange(desc(tot)) |>
      pull(categoria)
  }

  grp_order <- long |>
    count(.data[[group_var]], name = "tot") |>
    arrange(desc(tot)) |>
    pull(1)

  counts <- long |>
    count(.data[[group_var]], categoria, name = "n") |>
    mutate(categoria = factor(categoria, levels = cat_levels))

  counts[[group_var]] <- factor(counts[[group_var]], levels = grp_order)

  pal_vals <- get_palette(q, levels(counts$categoria))

  titulo <- if (!is.null(spec$titulo)) spec$titulo else q

  p <- ggplot(counts, aes(x = .data[[group_var]], y = n, fill = categoria)) +
    geom_col() +
    scale_fill_manual(values = pal_vals, limits = levels(counts$categoria), drop = FALSE) +
    labs(
      x = str_to_title(group_var),
      y = "Frecuencia",
      title = titulo,
      fill = "Categoría"
    ) +
    guides(fill = guide_legend(reverse = FALSE)) +
    theme(
      plot.title = element_text(face = "bold", lineheight = 0.25),
      legend.position = "right"
    )

  ggsave(
    filename = paste0("plots/", year, "/", round_id, "/", file_stub, "_", q, ".png"),
    plot = p,
    width = 12,
    height = 7,
    dpi = 300
  )
}

walk(preguntas, ~ stacked_plot(df, .x, "segmento", "R23_stack_segmento"))
walk(preguntas, ~ stacked_plot(df, .x, "etiqueta", "R23_stack_etiqueta"))
