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
round_id <- "R17"


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

# Títulos de preguntas (usados como título del gráfico)
q_titles <- c(
  q2  = "¿Cómo evalúas la gestión de Alejandro ‘Pacha’ Sánchez como Secretario de la Presidencia?",
  q4  = "En estos días la ONU trata el conflicto Israel–Hamas. ¿Usted ha escuchado hablar de este problema?",
  q6  = "Según su información, ¿cuán de acuerdo está con la posición del gobierno uruguayo?",
  q8  = "Casupá: ¿Usted escuchó hablar de la propuesta de una nueva represa?",
  q10 = "Casupá: ¿Es justo pagar a propietarios y avanzar, o buscar otra solución?"
)

# Definir el orden correcto de las categorías para cada pregunta cerrada
categoria_orden <- list(
  q2  = c("Apruebo", "Ni apruebo ni desapruebo", "Desapruebo", "No sé"),
  q4  = c(
    "Escuché hablar y siento que tengo mucha información sobre el tema",
    "Escuché hablar y siento que NO tengo mucha información sobre el tema",
    "No escuché hablar"
  ),
  q6  = c("Acuerdo", "Ni uno  ni otro", "Desacuerdo", "No sé"),
  q8  = c(
    "Escuché hablar y siento que tengo mucha información sobre el tema",
    "Escuché hablar y siento que NO tengo mucha información sobre el tema",
    "No escuché hablar"
  ),
  q10 = c(
    "Es justo que se pague a propietarios y se avance con el proyecto",
    "Debería buscarse otra solución",
    "No sé"
  )
)

# Paletas específicas por pregunta para asegurar correspondencia color-opción
paletas_pregunta <- list(
  q2 = c(
    "Apruebo"                          = "#59A14F",
    "Ni apruebo ni desapruebo"         = "#EDC948",
    "Desapruebo"                       = "#E15759",
    "No sé"                            = "#BAB0AC"
  ),
  q4 = c(
    "Escuché hablar y siento que tengo mucha información sobre el tema"      = "#59A14F",
    "Escuché hablar y siento que NO tengo mucha información sobre el tema"   = "#F28E2B",
    "No escuché hablar"                                                     = "#BAB0AC"
  ),
  q6 = c(
    "Acuerdo"                           = "#59A14F",
    "Ni uno  ni otro"                   = "#EDC948",
    "Desacuerdo"                        = "#E15759",
    "No sé"                             = "#BAB0AC"
  ),
  q8 = c(
    "Escuché hablar y siento que tengo mucha información sobre el tema"      = "#59A14F",
    "Escuché hablar y siento que NO tengo mucha información sobre el tema"   = "#F28E2B",
    "No escuché hablar"                                                     = "#BAB0AC"
  ),
  q10 = c(
    "Es justo que se pague a propietarios y se avance con el proyecto" = "#59A14F",
    "Debería buscarse otra solución"                                   = "#E15759",
    "No sé"                                                            = "#BAB0AC"
  )
)

# Helpers sin regex
# - norm_simple: minúsculas, sin acentos, espacios colapsados
# - map_by_dict: mapea por igualdad tras normalizar
norm_simple <- function(x) {
  # a minúsculas y bordes
  x <- tolower(trimws(x))
  # quitar acentos
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  # colapsar espacios múltiples sin regex
  squash_one <- function(s) {
    if (is.na(s)) return(NA_character_)
    parts <- strsplit(s, " ", fixed = TRUE)[[1]]
    parts <- parts[parts != ""]
    if (length(parts) == 0) "" else paste(parts, collapse = " ")
  }
  vapply(x, squash_one, character(1))
}

map_by_dict <- function(v, dict) {
  vn <- norm_simple(v)
  out <- v
  idx <- vn %in% names(dict)
  out[idx] <- unname(dict[vn[idx]])
  out
}

# Datos: lectura robusta (XLSX si existe; si no, CSV)
input_csv  <- paste0("data/processed/transcriptions/output/", year, "/transcripcion_", round_id, ".csv")
input_xlsx <- paste0("data/processed/transcriptions/output/", year, "/transcripcion_", round_id, ".xlsx")

if (!file.exists(input_xlsx)) {
  stop("No se encontró el archivo XLSX: ", input_xlsx)
}
if (!requireNamespace("openxlsx", quietly = TRUE)) {
  stop("Falta el paquete 'openxlsx'. Instálalo para leer el XLSX.")
}
df <- as_tibble(openxlsx::read.xlsx(input_xlsx))

# Asegurar que columnas de interés sean character
for (v in c("q2", "q4", "q6", "q8", "q10")) {
  if (v %in% names(df)) df[[v]] <- as.character(df[[v]])
}

# Normalización sin regex: solo q2 y q6 (q4, q8, q10 ya están tipificadas en XLSX)
dict_q2 <- c(
  "apruebo" = "Apruebo",
  "aprueba" = "Apruebo",
  "desapruebo" = "Desapruebo",
  "desaprueba" = "Desapruebo",
  "ni apruebo ni desapruebo" = "Ni apruebo ni desapruebo",
  "no se" = "No sé",
  "ns" = "No sé"
)

dict_q6 <- c(
  "acuerdo" = "Acuerdo",
  "desacuerdo" = "Desacuerdo",
  "ni uno ni otro" = "Ni uno  ni otro",
  "ni de acuerdo ni en desacuerdo" = "Ni uno  ni otro",
  "no se" = "No sé",
  "ns" = "No sé"
)

df <- df |>
  mutate(
    q2 = map_by_dict(q2, dict_q2),
    q6 = map_by_dict(q6, dict_q6)
  )

# ORDENAR CATEGORÍAS DESPUÉS DE LEER LOS DATOS
df <- df %>%
  mutate(
    q2  = factor(q2,  levels = categoria_orden$q2),
    q4  = factor(q4,  levels = categoria_orden$q4),
    q6  = factor(q6,  levels = categoria_orden$q6),
    q8  = factor(q8,  levels = categoria_orden$q8),
    q10 = factor(q10, levels = categoria_orden$q10)
  )

# Carpeta de salida	n
dir.create(paste0("plots/", year, "/", round_id, ""), recursive = TRUE, showWarnings = FALSE)

# Helper para apiladas por variable de agrupación (segmento o etiqueta)
stacked_plot <- function(data, q, group_var, file_stub) {
  # Trabajamos con la columna de la pregunta cerrada
  if (!q %in% names(data)) return(invisible(NULL))
  if (!group_var %in% names(data)) return(invisible(NULL))

  long <- data |>
    select(all_of(c(group_var, q))) |>
    rename(categoria = all_of(q)) |>
    filter(!is.na(categoria), !is.na(.data[[group_var]]))

  if (nrow(long) == 0) return(invisible(NULL))

  # Mantener el orden de factores ya definido para las categorías
  # Si la variable ya es factor, mantener sus niveles
  if (is.factor(long$categoria)) {
    cat_levels <- levels(long$categoria)
  } else {
  # Si no es factor, usar el orden definido para esta pregunta
    escala_tipo <- categoria_orden[[q]]
    if (!is.null(escala_tipo)) {
      cat_levels <- escala_tipo
    } else {
      # Fallback: orden por frecuencia
      cat_levels <- long |>
        count(categoria, name = "tot") |>
        arrange(desc(tot)) |>
        pull(categoria)
    }
  }

  # Orden de grupos por total
  grp_order <- long |>
    count(.data[[group_var]], name = "tot") |>
    arrange(desc(tot)) |>
    pull(1)

  # Conteos absolutos y factores ordenados
  counts <- long |>
    count(.data[[group_var]], categoria, name = "n") |>
    mutate(
      categoria = factor(categoria, levels = cat_levels)
    )
  
  # Factorizar el grupo
  counts[[group_var]] <- factor(counts[[group_var]], levels = grp_order)

  # Paleta mapeada a niveles presentes
  levs <- levels(counts$categoria)
  if (!is.null(paletas_pregunta[[q]])) {
    pal_vals <- paletas_pregunta[[q]][levs]
    if (any(is.na(pal_vals))) {
      faltantes <- which(is.na(pal_vals))
      pal_vals[faltantes] <- pal_base[seq_len(length(faltantes))]
    }
    pal_vals <- setNames(pal_vals, levs)
  } else {
    n_cols <- min(length(levs), length(pal_base))
    pal_vals <- setNames(pal_base[seq_len(n_cols)], levs[seq_len(n_cols)])
  }

  # Título desde el objeto de preguntas; fallback al nombre de la columna
  titulo <- if (!is.null(q_titles[[q]])) q_titles[[q]] else q

  p <- ggplot(counts, aes(x = .data[[group_var]], y = n, fill = categoria)) +
    geom_col() +
    scale_fill_manual(values = pal_vals, limits = levs, drop = FALSE) +
    labs(x = str_to_title(group_var), y = "Frecuencia", title = titulo, fill = "Categoría") +
    guides(fill = guide_legend(reverse = FALSE)) +
    theme(plot.title = element_text(face = "bold", lineheight = 0.25),
          legend.position = "right")

  ggsave(
    filename = paste0("plots/", year, "/", round_id, "/", file_stub, "_", q, ".png"),
    plot = p, width = 12, height = 7, dpi = 300
  )
}

# Loop de preguntas y salida (preguntas cerradas de la pauta)
preguntas <- c("q2", "q4", "q6", "q8", "q10")

walk(preguntas, ~ stacked_plot(df, .x, "segmento", "R17_stack_segmento"))
walk(preguntas, ~ stacked_plot(df, .x, "etiqueta", "R17_stack_etiqueta"))
