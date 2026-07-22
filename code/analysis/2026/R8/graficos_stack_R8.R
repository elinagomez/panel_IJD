library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(sysfonts)
library(showtext)
library(stringr)
library(purrr)

year <- 2026
round_id <- "R8"
input_file <- file.path(
  "data", "processed", "analysis", year, round_id, "R8_codificada.xlsx"
)
output_dir <- file.path("plots", year, round_id)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

group_order <- c(
  "Canelones",
  "Interior Coalición",
  "Interior Frente Amplio",
  "Montevideo",
  "CM",
  "FA"
)

dark_green <- "#4C7A2E"
mid_green <- "#668E38"
light_green <- "#7EA347"
pale_green <- "#A7BF67"
soft_green <- "#C7D990"
beige <- "#F2DEC1"
soft_red <- "#E7B8AF"
mid_red <- "#D99694"
dark_red <- "#C47C7A"
gray <- "#D9D9D9"
gray_dark <- "#BEBEBE"

registrar_montserrat <- function() {
  try({
    sysfonts::font_add_google(
      name = "Montserrat",
      family = "montserrat_google_bold",
      regular.wt = 500,
      bold.wt = 700
    )
    return("montserrat_google_bold")
  }, silent = TRUE)

  ff <- sysfonts::font_files()
  regular_hit <- ff[
    grepl("^Montserrat\\.ttf$", ff$file, ignore.case = TRUE),
  ]
  bold_hit <- ff[
    grepl("Montserrat-VariableFont_wght\\.ttf$", ff$file, ignore.case = TRUE),
  ]

  if (nrow(bold_hit) > 0) {
    bold_path <- file.path(bold_hit$path[1], bold_hit$file[1])
    sysfonts::font_add("montserrat_google_bold", regular = bold_path)
    return("montserrat_google_bold")
  }

  if (nrow(regular_hit) > 0) {
    regular_path <- file.path(regular_hit$path[1], regular_hit$file[1])
    sysfonts::font_add("montserrat_google_bold", regular = regular_path)
    return("montserrat_google_bold")
  }

  "sans"
}

base_font <- registrar_montserrat()
showtext_auto()
showtext_opts(dpi = 320)

theme_set(
  theme_minimal(base_family = base_font) +
    theme(
      text = element_text(family = base_font, face = "bold", colour = "black"),
      plot.title = element_text(size = 24, hjust = 0),
      plot.subtitle = element_text(size = 13, hjust = 0),
      axis.text.x = element_text(size = 12, angle = 18, hjust = 1),
      axis.text.y = element_blank(),
      axis.title = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      legend.title = element_blank(),
      legend.text = element_text(size = 10.5),
      legend.position = "right",
      plot.margin = margin(12, 16, 12, 12)
    )
)

normalize_value <- function(x) {
  x_chr <- trimws(as.character(x))
  x_chr[x_chr %in% c("", "NA")] <- NA_character_
  x_chr
}

format_base_n <- function(n) {
  prettyNum(
    as.integer(round(n)),
    big.mark = ".",
    decimal.mark = ",",
    preserve.width = "none"
  )
}

hex_luminance <- function(hex) {
  clean_hex <- gsub("#", "", hex)
  if (is.na(clean_hex) || nchar(clean_hex) != 6) {
    return(0)
  }

  rgb <- c(
    substr(clean_hex, 1, 2),
    substr(clean_hex, 3, 4),
    substr(clean_hex, 5, 6)
  )
  rgb_num <- strtoi(rgb, base = 16L) / 255
  sum(rgb_num * c(0.299, 0.587, 0.114))
}

label_colour_for_fill <- function(fill_hex) {
  if (hex_luminance(fill_hex) > 0.62) "black" else "white"
}

build_grouped_counts <- function(df, value_col, levels) {
  valid_df <- df |>
    mutate(categoria = normalize_value(.data[[value_col]])) |>
    filter(!is.na(categoria))

  base_n <- nrow(valid_df)
  unknown_values <- setdiff(sort(unique(valid_df$categoria)), levels)

  if (length(unknown_values) > 0) {
    stop(
      "Categorias sin configurar en ", value_col, ": ",
      paste(unknown_values, collapse = ", ")
    )
  }

  counts_segmento <- valid_df |>
    filter(segmento %in% group_order[1:4]) |>
    count(grupo = segmento, categoria, name = "n")

  counts_voto <- valid_df |>
    filter(voto2 %in% group_order[5:6]) |>
    count(grupo = voto2, categoria, name = "n")

  bind_rows(counts_segmento, counts_voto) |>
    mutate(
      grupo = factor(grupo, levels = group_order),
      categoria = factor(categoria, levels = levels)
    ) |>
    complete(grupo, categoria, fill = list(n = 0)) |>
    group_by(grupo) |>
    mutate(
      total_grupo = sum(n),
      prop = if_else(total_grupo > 0, n / total_grupo, 0)
    ) |>
    ungroup()
}

plot_stack <- function(df, spec) {
  if (!setequal(spec$levels, names(spec$palette))) {
    stop("La paleta no coincide con los niveles de ", spec$value_col)
  }

  counts <- build_grouped_counts(df, spec$value_col, spec$levels) |>
    mutate(
      fill_hex = unname(spec$palette[as.character(categoria)]),
      label_colour = vapply(fill_hex, label_colour_for_fill, character(1))
    )

  subtitle_text <- str_glue(
    "{spec$subtitle_prefix} (n = {format_base_n(spec$base_n)})"
  )

  bar_data <- counts |>
    filter(n > 0)

  label_data <- counts |>
    filter(n > 0)

  p <- ggplot(counts, aes(x = grupo, y = prop, fill = categoria)) +
    geom_col(
      data = bar_data,
      width = 0.82,
      color = "white",
      linewidth = 0.3
    ) +
    geom_text(
      data = label_data,
      aes(label = n, color = label_colour, group = categoria),
      position = position_stack(vjust = 0.5),
      family = base_font,
      size = 3.5,
      fontface = "bold",
      show.legend = FALSE
    ) +
    scale_fill_manual(
      values = spec$palette[spec$levels],
      breaks = spec$levels,
      labels = function(x) str_wrap(x, width = 28),
      drop = FALSE
    ) +
    scale_color_identity() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
    labs(
      title = str_wrap(spec$title, width = 52),
      subtitle = str_wrap(subtitle_text, width = 78)
    ) +
    coord_cartesian(clip = "off")

  ggsave(
    filename = file.path(output_dir, spec$output_name),
    plot = p,
    width = 13.5,
    height = 7.5,
    dpi = 320,
    bg = "white"
  )
}

plot_stack_q9_12 <- function(df, spec) {
  if (!setequal(spec$levels, names(spec$palette))) {
    stop("La paleta no coincide con los niveles de ", spec$value_col)
  }

  counts <- build_grouped_counts(df, spec$value_col, spec$levels) |>
    mutate(
      fill_hex = unname(spec$palette[as.character(categoria)]),
      label_colour = vapply(fill_hex, label_colour_for_fill, character(1)),
      label_text = if_else(n > 0, as.character(n), NA_character_)
    )

  subtitle_text <- str_glue(
    "{spec$subtitle_prefix} (n = {format_base_n(spec$base_n)})"
  )

  bar_data <- counts |>
    filter(n > 0)

  p <- ggplot(counts, aes(x = grupo, y = prop, fill = categoria)) +
    geom_col(
      data = bar_data,
      width = 0.82,
      color = "white",
      linewidth = 0.3
    ) +
    geom_text(
      aes(label = label_text, color = label_colour, group = categoria),
      position = position_stack(vjust = 0.5),
      family = base_font,
      size = 3.5,
      fontface = "bold",
      na.rm = TRUE,
      show.legend = FALSE
    ) +
    scale_fill_manual(
      values = spec$palette[spec$levels],
      breaks = spec$levels,
      labels = function(x) str_wrap(x, width = 28),
      drop = FALSE
    ) +
    scale_color_identity() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
    labs(
      title = str_wrap(spec$title, width = 52),
      subtitle = str_wrap(subtitle_text, width = 78)
    ) +
    coord_cartesian(clip = "off")

  ggsave(
    filename = file.path(output_dir, spec$output_name),
    plot = p,
    width = 13.5,
    height = 7.5,
    dpi = 320,
    bg = "white"
  )
}

plot_specs <- list(
  list(
    output_name = "R8_q1_nivel_exposicion_stack_abs.png",
    value_col = "nivel_exposicion_q1",
    title = "Nivel de exposición a la interpelación",
    subtitle_prefix = "Entre quienes respondieron la pregunta",
    levels = c(
      "Exposición Alta",
      "Exposición Parcial",
      "No vio pero sabía",
      "No vio ni sabía"
    ),
    palette = c(
      "Exposición Alta" = dark_green,
      "Exposición Parcial" = pale_green,
      "No vio pero sabía" = beige,
      "No vio ni sabía" = gray
    )
  ),
  list(
    output_name = "R8_q1_recuerdos_stack_abs.png",
    value_col = "dimension_tematica_q1",
    title = "Qué se recuerda de la interpelación",
    subtitle_prefix = "Entre quienes recordaron un contenido identificable de la interpelación",
    levels = c(
      "Crisis de Seguridad",
      "Cifras y Datos",
      "Sistema Carcelario",
      "Caso Marset",
      "Gestión Comparada"
    ),
    palette = c(
      "Crisis de Seguridad" = dark_green,
      "Cifras y Datos" = mid_green,
      "Sistema Carcelario" = pale_green,
      "Caso Marset" = beige,
      "Gestión Comparada" = soft_red
    )
  ),
  list(
    output_name = "R8_q1_tono_stack_abs.png",
    value_col = "tono_q1",
    title = "Tono del recuerdo sobre la interpelación",
    subtitle_prefix = "Entre quienes expresaron una valoración identificable sobre la interpelación",
    levels = c(
      "Favorable al Ministro",
      "Neutral / Descriptivo",
      "Escéptico / Cinismo",
      "Favorable al Interpelante"
    ),
    palette = c(
      "Favorable al Ministro" = dark_green,
      "Neutral / Descriptivo" = beige,
      "Escéptico / Cinismo" = gray_dark,
      "Favorable al Interpelante" = dark_red
    )
  ),
  list(
    output_name = "R8_q2_postura_general_stack_abs.png",
    value_col = "postura_general_q2",
    title = "Debate sobre evitar cárcel para delitos leves: postura general",
    subtitle_prefix = "Entre quienes respondieron la pregunta",
    levels = c(
      "Pro-Gobierno / Ministro",
      "Mixta / Punto Medio",
      "Pro-Bordaberry / Oposición",
      "Rechazo / Punitivismo extremo",
      "NS/NC / Desinformado"
    ),
    palette = c(
      "Pro-Gobierno / Ministro" = dark_green,
      "Mixta / Punto Medio" = beige,
      "Pro-Bordaberry / Oposición" = soft_red,
      "Rechazo / Punitivismo extremo" = dark_red,
      "NS/NC / Desinformado" = gray
    )
  ),
  list(
    output_name = "R8_q2_percepcion_sistema_stack_abs.png",
    value_col = "percepcion_sistema_q2",
    title = "Debate sobre evitar cárcel para delitos leves: percepción del sistema carcelario",
    subtitle_prefix = "Entre quienes aludieron al sistema carcelario en su respuesta",
    levels = c(
      "Cárcel como \"Escuela\"",
      "Hacinamiento / Saturación",
      "Ineficacia de la Pena",
      "Falta de Rehabilitación"
    ),
    palette = c(
      "Cárcel como \"Escuela\"" = dark_green,
      "Hacinamiento / Saturación" = mid_green,
      "Ineficacia de la Pena" = pale_green,
      "Falta de Rehabilitación" = beige
    )
  ),
  list(
    output_name = "R8_q2_argumentos_temores_stack_abs.png",
    value_col = "argumentos_temores_q2",
    title = "Debate sobre evitar cárcel para delitos leves: argumentos y temores predominantes",
    subtitle_prefix = "Entre quienes formularon un argumento o temor identificable",
    levels = c(
      "Efecto Contagio",
      "Debilidad / Impunidad",
      "Desconfianza Política"
    ),
    palette = c(
      "Efecto Contagio" = dark_green,
      "Debilidad / Impunidad" = soft_red,
      "Desconfianza Política" = beige
    )
  ),
  list(
    output_name = "R8_q2_propuestas_solucion_stack_abs.png",
    value_col = "propuestas_solucion_q2",
    title = "Debate sobre evitar cárcel para delitos leves: soluciones propuestas",
    subtitle_prefix = "Entre quienes propusieron una salida concreta al problema",
    levels = c(
      "Penas Alternativas / Comunitarias",
      "Cárceles Diferenciadas",
      "Enfoque en Salud/Social",
      "Inversión en Infraestructura"
    ),
    palette = c(
      "Penas Alternativas / Comunitarias" = dark_green,
      "Cárceles Diferenciadas" = light_green,
      "Enfoque en Salud/Social" = pale_green,
      "Inversión en Infraestructura" = beige
    )
  ),
  list(
    output_name = "R8_q3_stack_abs.png",
    value_col = "q3",
    title = "Evaluación del accionar de la oposición en el caso",
    subtitle_prefix = "Entre quienes respondieron la pregunta",
    levels = c(
      "Está actuando de manera correcta",
      "No está actuando de manera correcta",
      "No tengo una opinión formada"
    ),
    palette = c(
      "Está actuando de manera correcta" = dark_green,
      "No está actuando de manera correcta" = soft_red,
      "No tengo una opinión formada" = gray
    )
  ),
  list(
    output_name = "R8_q4_postura_debate_stack_abs.png",
    value_col = "postura_debate_q4",
    title = "Fundamentos de la evaluación de la oposición: postura en el debate Negro-Bordaberry",
    subtitle_prefix = "Entre quienes dieron una postura codificable sobre el debate",
    levels = c(
      "Pro-Ministro",
      "Mixta / Gris",
      "Pro-Bordaberry",
      "Punitivismo Radical",
      "NS / NC"
    ),
    palette = c(
      "Pro-Ministro" = dark_green,
      "Mixta / Gris" = beige,
      "Pro-Bordaberry" = soft_red,
      "Punitivismo Radical" = dark_red,
      "NS / NC" = gray
    )
  ),
  list(
    output_name = "R8_q4_evaluacion_oposicion_stack_abs.png",
    value_col = "evaluacion_oposicion_q4",
    title = "Fundamentos de la evaluación de la oposición: evaluación de la oposición",
    subtitle_prefix = "Entre quienes emitieron una evaluación codificable del accionar opositor",
    levels = c("Correcta", "Incorrecta", "Sin Definir"),
    palette = c(
      "Correcta" = dark_green,
      "Incorrecta" = soft_red,
      "Sin Definir" = gray
    )
  ),
  list(
    output_name = "R8_q4_argumentos_justificacion_stack_abs.png",
    value_col = "argumentos_justificacion_q4",
    title = "Fundamentos de la evaluación de la oposición: tipo de justificación",
    subtitle_prefix = "Entre quienes desarrollaron una justificación identificable",
    levels = c(
      "Control Democrático",
      "Sentido Común / Justicia",
      "Falta de Aportes",
      "Incoherencia / Pasado",
      "Electoralismo / \"Circo\""
    ),
    palette = c(
      "Control Democrático" = dark_green,
      "Sentido Común / Justicia" = pale_green,
      "Falta de Aportes" = beige,
      "Incoherencia / Pasado" = mid_red,
      "Electoralismo / \"Circo\"" = dark_red
    )
  ),
  list(
    output_name = "R8_q4_percepcion_sistema_stack_abs.png",
    value_col = "percepcion_sistema_q4",
    title = "Fundamentos de la evaluación de la oposición: percepción del sistema carcelario",
    subtitle_prefix = "Entre quienes vincularon su justificación con el funcionamiento del sistema carcelario",
    levels = c(
      "Cárcel = Escuela",
      "Hacinamiento",
      "Diferenciación"
    ),
    palette = c(
      "Cárcel = Escuela" = dark_green,
      "Hacinamiento" = pale_green,
      "Diferenciación" = beige
    )
  ),
  list(
    output_name = "R8_q5_stack_abs.png",
    value_col = "q5",
    title = "Credibilidad de las cifras comunicadas por el Ministerio del Interior",
    subtitle_prefix = "Entre quienes respondieron la pregunta",
    levels = c(
      "Me resultaron creíbles",
      "No me resultaron creíbles",
      "No sabría decir"
    ),
    palette = c(
      "Me resultaron creíbles" = dark_green,
      "No me resultaron creíbles" = soft_red,
      "No sabría decir" = gray
    )
  ),
  list(
    output_name = "R8_q6_evaluacion_stack_abs.png",
    value_col = "evaluacion_q6",
    title = "Credibilidad de las cifras del Ministerio del Interior: evaluación general",
    subtitle_prefix = "Entre quienes fundamentaron su respuesta",
    levels = c("Creíbles", "No creíbles", "No sabe / Dudoso"),
    palette = c(
      "Creíbles" = dark_green,
      "No creíbles" = soft_red,
      "No sabe / Dudoso" = gray
    )
  ),
  list(
    output_name = "R8_q6_argumentos_desconfianza_stack_abs.png",
    value_col = "argumentos_desconfianza_q6",
    title = "Credibilidad de las cifras del Ministerio del Interior: argumentos de desconfianza",
    subtitle_prefix = "Entre quienes expresaron una desconfianza codificable",
    levels = c(
      "Maquillaje / Manipulación",
      "Falta de Denuncia",
      "Contraste con Realidad",
      "Sesgo de Selección"
    ),
    palette = c(
      "Maquillaje / Manipulación" = dark_red,
      "Falta de Denuncia" = mid_red,
      "Contraste con Realidad" = soft_red,
      "Sesgo de Selección" = beige
    )
  ),
  list(
    output_name = "R8_q6_argumentos_confianza_stack_abs.png",
    value_col = "argumentos_confianza_q6",
    title = "Credibilidad de las cifras del Ministerio del Interior: argumentos de confianza",
    subtitle_prefix = "Entre quienes expresaron una confianza codificable",
    levels = c(
      "Institucionalidad",
      "Lógica de Riesgo",
      "Gestión / Resultados"
    ),
    palette = c(
      "Institucionalidad" = dark_green,
      "Lógica de Riesgo" = light_green,
      "Gestión / Resultados" = pale_green
    )
  ),
  list(
    output_name = "R8_q6_influencia_externa_stack_abs.png",
    value_col = "influencia_externa_q6",
    title = "Credibilidad de las cifras del Ministerio del Interior: influencia de medios u otros factores externos",
    subtitle_prefix = "Entre quienes mencionaron factores externos al evaluar las cifras",
    levels = c(
      "Sesgo Mediático (Alarma)",
      "Sesgo Mediático (Veracidad)"
    ),
    palette = c(
      "Sesgo Mediático (Alarma)" = beige,
      "Sesgo Mediático (Veracidad)" = soft_red
    )
  ),
  list(
    output_name = "R8_q6_contexto_otros_stack_abs.png",
    value_col = "contexto_otros_q6",
    title = "Credibilidad de las cifras del Ministerio del Interior: contexto y otros encuadres",
    subtitle_prefix = "Entre quienes introdujeron un marco contextual o justificaron no poder evaluar",
    levels = c(
      "Subjetividad / Sensación",
      "Desinterés / Falta de Info"
    ),
    palette = c(
      "Subjetividad / Sensación" = beige,
      "Desinterés / Falta de Info" = gray
    )
  ),
  list(
    output_name = "R8_q7_stack_abs.png",
    value_col = "q7",
    title = "Prioridad de inversión del Estado en materia carcelaria",
    subtitle_prefix = "Entre quienes respondieron la pregunta",
    levels = c(
      "en la construcción de nuevas cárceles",
      "en la mejora de las cárceles ya existentes",
      "tanto en la construcción de cárceles como en la mejora de las ya existentes",
      "NO es prioritario que el Estado uruguayo invierta en cárceles."
    ),
    palette = c(
      "en la construcción de nuevas cárceles" = mid_green,
      "en la mejora de las cárceles ya existentes" = pale_green,
      "tanto en la construcción de cárceles como en la mejora de las ya existentes" = dark_green,
      "NO es prioritario que el Estado uruguayo invierta en cárceles." = soft_red
    )
  ),
  list(
    output_name = "R8_q8_prioridad_inversion_stack_abs.png",
    value_col = "prioridad_inversion_q8",
    title = "Inversión del Estado en cárceles: prioridad principal",
    subtitle_prefix = "Entre quienes dieron una respuesta codificable sobre prioridad de inversión",
    levels = c(
      "Construcción (Nuevas)",
      "Mejora (Existentes)",
      "Ambas (Nuevas + Mejora)",
      "No Invertir / Otras áreas"
    ),
    palette = c(
      "Construcción (Nuevas)" = mid_green,
      "Mejora (Existentes)" = pale_green,
      "Ambas (Nuevas + Mejora)" = dark_green,
      "No Invertir / Otras áreas" = soft_red
    )
  ),
  list(
    output_name = "R8_q8_justificacion_rehabilitacion_stack_abs.png",
    value_col = "justificacion_rehabilitacion_q8",
    title = "Inversión del Estado en cárceles: justificaciones rehabilitadoras",
    subtitle_prefix = "Entre quienes justificaron su postura desde la rehabilitación o reinserción",
    levels = c(
      "Dignidad Humana",
      "Reincidencia / Escuela",
      "Clasificación de Presos"
    ),
    palette = c(
      "Dignidad Humana" = dark_green,
      "Reincidencia / Escuela" = pale_green,
      "Clasificación de Presos" = beige
    )
  ),
  list(
    output_name = "R8_q8_justificacion_punitivismo_stack_abs.png",
    value_col = "justificacion_punitivismo_q8",
    title = "Inversión del Estado en cárceles: justificaciones punitivas",
    subtitle_prefix = "Entre quienes justificaron su postura desde una lógica punitiva",
    levels = c(
      "Trabajo Obligatorio",
      "Máxima Seguridad / Castigo",
      "Hartazgo / Ciudadano de bien"
    ),
    palette = c(
      "Trabajo Obligatorio" = beige,
      "Máxima Seguridad / Castigo" = dark_red,
      "Hartazgo / Ciudadano de bien" = mid_red
    )
  ),
  list(
    output_name = "R8_q8_gestion_recursos_stack_abs.png",
    value_col = "gestion_recursos_q8",
    title = "Inversión del Estado en cárceles: gestión y recursos",
    subtitle_prefix = "Entre quienes enfocaron su respuesta en gestión o uso de recursos",
    levels = c(
      "Mala Administración",
      "Privatización / Tercerización",
      "Ineficiencia del Gasto"
    ),
    palette = c(
      "Mala Administración" = soft_red,
      "Privatización / Tercerización" = beige,
      "Ineficiencia del Gasto" = dark_red
    )
  ),
  list(
    output_name = "R8_q9_stack_abs.png",
    value_col = "q9",
    title = "Importancia de las políticas de rehabilitación para personas privadas de libertad",
    subtitle_prefix = "Entre quienes respondieron la pregunta",
    levels = c(
      "Muy importante",
      "Algo importante",
      "Ni una cosa ni la otra",
      "Poco importante",
      "Nada importante",
      "No tengo opinión"
    ),
    palette = c(
      "Muy importante" = dark_green,
      "Algo importante" = pale_green,
      "Ni una cosa ni la otra" = beige,
      "Poco importante" = soft_red,
      "Nada importante" = dark_red,
      "No tengo opinión" = gray
    )
  ),
  list(
    output_name = "R8_q10_stack_abs.png",
    value_col = "codigo_q10",
    title = "Importancia de las políticas de rehabilitación: fundamento principal",
    subtitle_prefix = "Entre quienes fundamentaron su opinión sobre rehabilitación",
    levels = c(
      "Reducción de Reincidencia",
      "Reinserción Laboral/Educativa",
      "Función Institucional (Deber Ser)",
      "Derecho y Humanidad",
      "Costo-Beneficio Social",
      "Crítica al Sistema Actual",
      "Escepticismo / Voluntad Individual",
      "Punitivismo / Inutilidad"
    ),
    palette = c(
      "Reducción de Reincidencia" = dark_green,
      "Reinserción Laboral/Educativa" = mid_green,
      "Función Institucional (Deber Ser)" = light_green,
      "Derecho y Humanidad" = pale_green,
      "Costo-Beneficio Social" = soft_green,
      "Crítica al Sistema Actual" = beige,
      "Escepticismo / Voluntad Individual" = soft_red,
      "Punitivismo / Inutilidad" = dark_red
    )
  ),
  list(
    output_name = "R8_q11_stack_abs.png",
    value_col = "q11",
    title = "Importancia de ofrecer trabajo y estudio en la cárcel",
    subtitle_prefix = "Entre quienes respondieron la pregunta",
    levels = c(
      "Muy importante",
      "Algo importante",
      "Ni una cosa ni la otra",
      "Poco importante",
      "Nada importante",
      "No tengo opinión"
    ),
    palette = c(
      "Muy importante" = dark_green,
      "Algo importante" = pale_green,
      "Ni una cosa ni la otra" = beige,
      "Poco importante" = soft_red,
      "Nada importante" = dark_red,
      "No tengo opinión" = gray
    )
  ),
  list(
    output_name = "R8_q12_stack_abs.png",
    value_col = "codigo_q12",
    title = "Importancia de ofrecer trabajo y estudio en la cárcel: fundamento principal",
    subtitle_prefix = "Entre quienes fundamentaron su opinión sobre trabajo y estudio en prisión",
    levels = c(
      "Herramientas de Reinserción",
      "Hábitos y Disciplina",
      "Dignificación y Autoestima",
      "Justicia Social / Brecha Educativa",
      "Combate al Ocio",
      "Meritocracia y Selección",
      "Escepticismo sobre el Cambio",
      "Injusticia Percibida (Exclusión)"
    ),
    palette = c(
      "Herramientas de Reinserción" = dark_green,
      "Hábitos y Disciplina" = mid_green,
      "Dignificación y Autoestima" = light_green,
      "Justicia Social / Brecha Educativa" = pale_green,
      "Combate al Ocio" = soft_green,
      "Meritocracia y Selección" = beige,
      "Escepticismo sobre el Cambio" = soft_red,
      "Injusticia Percibida (Exclusión)" = dark_red
    )
  )
)

df <- read_excel(input_file)

required_cols <- unique(c(
  "segmento",
  "voto2",
  map_chr(plot_specs, "value_col")
))

missing_cols <- setdiff(required_cols, names(df))
if (length(missing_cols) > 0) {
  stop(
    "Faltan columnas en la base de entrada: ",
    paste(missing_cols, collapse = ", ")
  )
}

plot_specs <- map(plot_specs, function(spec) {
  spec$base_n <- sum(!is.na(normalize_value(df[[spec$value_col]])))
  spec
})

special_value_cols <- c("q9", "codigo_q10", "q11", "codigo_q12")

plot_specs_core <- keep(
  plot_specs,
  ~ !(.x$value_col %in% special_value_cols)
)
plot_specs_q9_12 <- keep(
  plot_specs,
  ~ .x$value_col %in% special_value_cols
)

walk(plot_specs_core, ~ plot_stack(df, .x))
walk(plot_specs_q9_12, ~ plot_stack_q9_12(df, .x))

cat("Gráficos guardados en:", output_dir, "\n")
