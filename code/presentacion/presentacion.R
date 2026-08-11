# =============================================================================
# code/presentacion/presentacion.R
# Funciones para armar la presentacion de resultados de una ronda.
#
# La presentacion se genera entera desde la base codificada: cambia la ronda en
# project.yml y sale la misma estructura con los datos nuevos.
#
# Se usa desde code/presentacion/armar_presentacion.R
# =============================================================================

library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(purrr)
library(ggplot2)
library(officer)
library(glue)

`%||%` <- function(x, y) if (is.null(x)) y else x

# ---- 1. Identidad visual ----------------------------------------------------

AZUL   <- "#004A82"   # azul institucional, tomado del logo
AZUL_2 <- "#3B7EA8"
GRIS   <- "#5A6570"
GRIS_C <- "#D8DDE2"
NARANJA <- "#C1611F"  # solo para destacar

PALETA_CORTE <- c(AZUL, NARANJA, AZUL_2, GRIS, "#7A9E3F", "#8B5E83")

FUENTE <- "Montserrat"   # si no esta instalada, cae en la de sistema

tema_panel <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_line(colour = GRIS_C),
      axis.title         = element_blank(),
      axis.text          = element_text(colour = "grey25"),
      legend.position    = "top",
      legend.title       = element_blank(),
      plot.title         = element_blank(),
      plot.margin        = margin(4, 12, 4, 4)
    )
}

# ---- 2. Recodificacion de las variables de corte ----------------------------

#' Agrega las variables de corte de la presentacion.
#' Los -1 de LimeSurvey (no responde) pasan a NA.
recodificar <- function(d) {

  # La conversion se hace DENTRO del mutate: si se guardara en una variable
  # llamada `edad`, la columna `edad` del data frame la taparia (data masking).
  num <- function(x) { v <- suppressWarnings(as.numeric(x)); v[v < 0] <- NA; v }

  d |>
    mutate(
      edad_num = num(edad),
      ideo_num = num(ideologia),

      sexo = case_when(
        str_detect(coalesce(genero, ""), "^Mujer|^Femenino") ~ "Mujer",
        str_detect(coalesce(genero, ""), "^Var|^Masculino")  ~ "Varón",
        !is.na(genero)                                        ~ "Otro",
        TRUE                                                  ~ NA_character_
      ),

      edad_tramo = cut(edad_num, breaks = c(17, 29, 44, 59, Inf),
                       labels = c("18-29", "30-44", "45-59", "60 y más")),

      region = case_when(
        departamento == "Montevideo" ~ "Montevideo",
        departamento == "Canelones"  ~ "Canelones",
        !is.na(departamento)         ~ "Interior",
        TRUE                         ~ NA_character_
      ),

      ideologia_rec = cut(ideo_num, breaks = c(0, 4, 6, 10),
                          labels = c("Izquierda (1-4)", "Centro (5-6)", "Derecha (7-10)")),

      educacion_rec = case_when(
        str_detect(coalesce(n_educativo, ""), regex("universi|terciaria|posgrado", TRUE)) ~ "Terciaria",
        str_detect(coalesce(n_educativo, ""), regex("bachillerato", TRUE))                ~ "Media",
        !is.na(n_educativo)                                                                ~ "Hasta ciclo básico",
        TRUE                                                                               ~ NA_character_
      ),

      voto_rec = case_when(
        voto == "Frente Amplio" ~ "Frente Amplio",
        voto %in% c("Partido Nacional", "Partido Colorado",
                    "Cabildo Abierto", "Partido Independiente") ~ "Coalición",
        voto %in% c("En blanco", "Anulado", "No votó") ~ "No votó / blanco",
        !is.na(voto) ~ "Otros",
        TRUE ~ NA_character_
      )
    ) |>
    mutate(
      across(c(edad_tramo, ideologia_rec), as.character),
      region        = factor(region, levels = c("Montevideo", "Canelones", "Interior")),
      educacion_rec = factor(educacion_rec, levels = c("Hasta ciclo básico", "Media", "Terciaria")),
      voto_rec      = factor(voto_rec, levels = c("Frente Amplio", "Coalición", "No votó / blanco", "Otros"))
    )
}

CORTES <- c(sexo = "Sexo", edad_tramo = "Edad", region = "Región",
            ideologia_rec = "Autoubicación ideológica", educacion_rec = "Nivel educativo",
            voto_rec = "Voto 2024")

# ---- 3. Codigos ------------------------------------------------------------

#' Etiqueta legible de un codigo. Si el codebook trae una columna con nombre
#' corto (etiqueta_corta o nombre), se usa esa; si no, se embellece el snake_case.
bonito <- function(x, diccionario = NULL) {
  if (!is.null(diccionario) && all(c("etiqueta", "corta") %in% names(diccionario))) {
    hit <- diccionario$corta[match(x, diccionario$etiqueta)]
    x <- ifelse(is.na(hit) | !nzchar(hit), x, hit)
  }
  x <- str_squish(str_replace_all(x, "_", " "))
  # mayuscula inicial sin depender de str_replace con funcion, que necesita
  # stringr >= 1.5
  paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
}

#' Formato largo: una fila por (caso, codigo) de una pregunta
codigos_largo <- function(d, q) {
  cc <- paste0("codigo_", q)
  d |>
    select(all_of(c(cc, names(CORTES)))) |>
    rename(codigos = all_of(cc)) |>
    filter(!is.na(codigos), codigos != "ERROR") |>
    mutate(fila = row_number(),
           cod = str_split(codigos, "\\s*;\\s*")) |>
    unnest(cod) |>
    mutate(cod = str_squish(cod)) |>
    filter(nzchar(cod))
}

#' Orden de los codigos: por frecuencia, con el de no clasificable al final
orden_codigos <- function(largo) {
  tab <- largo |> count(cod, sort = TRUE)
  nc  <- str_detect(tab$cod, "no_clasific|no_aplica|no_respuesta")
  c(tab$cod[!nc], tab$cod[nc])
}

# ---- 4. Graficos -----------------------------------------------------------

#' Barras horizontales con el N al final de cada barra
g_general <- function(d, q, diccionario = NULL) {

  largo <- codigos_largo(d, q)
  ord   <- rev(orden_codigos(largo))

  datos <- largo |>
    count(cod) |>
    mutate(cod = factor(cod, levels = ord),
           etiqueta = bonito(as.character(cod), diccionario))

  ggplot(datos, aes(x = n, y = cod)) +
    geom_col(fill = AZUL, width = 0.7) +
    geom_text(aes(label = n), hjust = -0.35, size = 3.4, colour = "grey25") +
    scale_y_discrete(labels = function(v) bonito(v, diccionario)) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
    tema_panel()
}

#' Barras agrupadas por una variable de corte, con N
g_corte <- function(d, q, corte, diccionario = NULL, min_n = 5) {

  largo <- codigos_largo(d, q) |> filter(!is.na(.data[[corte]]))
  if (!nrow(largo)) return(NULL)

  # solo los codigos con presencia suficiente, para que el grafico se lea
  frec <- largo |> count(cod) |> filter(n >= min_n)
  largo <- largo |> filter(cod %in% frec$cod)
  if (!nrow(largo)) return(NULL)

  ord <- rev(orden_codigos(largo))

  datos <- largo |>
    count(cod, grupo = .data[[corte]]) |>
    mutate(cod = factor(cod, levels = ord))

  ggplot(datos, aes(x = n, y = cod, fill = grupo)) +
    geom_col(position = position_dodge(width = 0.78), width = 0.72) +
    geom_text(aes(label = n), position = position_dodge(width = 0.78),
              hjust = -0.35, size = 2.9, colour = "grey30") +
    scale_y_discrete(labels = function(v) bonito(v, diccionario)) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.14))) +
    scale_fill_manual(values = PALETA_CORTE) +
    tema_panel()
}

#' Composicion de la muestra para una variable
g_composicion <- function(d, var) {
  datos <- d |>
    filter(!is.na(.data[[var]])) |>
    count(cat = .data[[var]]) |>
    mutate(cat = factor(cat, levels = rev(sort(unique(as.character(cat))))))

  ggplot(datos, aes(x = n, y = cat)) +
    geom_col(fill = AZUL, width = 0.65) +
    geom_text(aes(label = n), hjust = -0.4, size = 3.4, colour = "grey25") +
    scale_x_continuous(expand = expansion(mult = c(0, 0.14))) +
    tema_panel()
}

#' V de Cramer de una tabla de contingencia
cramer_v <- function(tab) {
  if (nrow(tab) < 2 || ncol(tab) < 2 || sum(tab) == 0) return(NA_real_)
  chi <- suppressWarnings(stats::chisq.test(tab, correct = FALSE)$statistic)
  as.numeric(sqrt(chi / (sum(tab) * (min(dim(tab)) - 1))))
}

#' Elige las variables de corte que mas diferencian los codigos de una pregunta.
#' Evita llenar la presentacion de graficos que dicen lo mismo.
cortes_relevantes <- function(d, q, n = 2, min_menciones = 5) {

  largo <- codigos_largo(d, q)
  frec  <- largo |> count(cod) |> filter(n >= min_menciones)
  largo <- largo |> filter(cod %in% frec$cod)
  if (!nrow(largo)) return(character(0))

  fuerza <- map_dbl(names(CORTES), function(v) {
    sub <- largo |> filter(!is.na(.data[[v]]))
    if (!nrow(sub)) return(NA_real_)
    tab <- table(sub$cod, sub[[v]])
    tab <- tab[rowSums(tab) > 0, colSums(tab) > 0, drop = FALSE]
    if (nrow(tab) < 2 || ncol(tab) < 2) return(NA_real_)
    cramer_v(tab)
  })
  names(fuerza) <- names(CORTES)

  fuerza <- sort(fuerza[!is.na(fuerza)], decreasing = TRUE)
  head(names(fuerza), n)
}

#' Analisis de correspondencias simple, calculado con SVD en R base.
#'
#' Se hace a mano y no con FactoMineR/factoextra a proposito: esas dependencias
#' arrastran media docena de paquetes y exigen versiones recientes de dplyr, lo
#' que rompe la instalacion en R viejos. El calculo es el estandar de Greenacre:
#' residuos estandarizados de la tabla de contingencia y descomposicion en
#' valores singulares. Devuelve coordenadas principales de filas y columnas
#' (mapa simetrico) y el porcentaje de inercia de cada dimension.
ca_svd <- function(tab) {

  N <- as.matrix(tab)
  P <- N / sum(N)
  r <- rowSums(P)
  s <- colSums(P)

  if (any(r == 0) || any(s == 0)) return(NULL)

  Dr <- diag(1 / sqrt(r), nrow = length(r))
  Ds <- diag(1 / sqrt(s), nrow = length(s))

  descomp <- svd(Dr %*% (P - outer(r, s)) %*% Ds)

  k <- min(2, sum(descomp$d > 1e-8))
  if (k < 2) return(NULL)

  Dk <- diag(descomp$d[1:k], nrow = k)
  coord_filas <- Dr %*% descomp$u[, 1:k, drop = FALSE] %*% Dk
  coord_cols  <- Ds %*% descomp$v[, 1:k, drop = FALSE] %*% Dk

  list(
    filas = tibble(etiqueta = rownames(N), x = coord_filas[, 1], y = coord_filas[, 2],
                   peso = rowSums(N), tipo = "Código"),
    columnas = tibble(etiqueta = colnames(N), x = coord_cols[, 1], y = coord_cols[, 2],
                      peso = colSums(N), tipo = "Categoría"),
    inercia = round(100 * descomp$d^2 / sum(descomp$d^2), 1)[1:k]
  )
}

#' Analisis de correspondencias entre codigos y una variable de corte.
#' Devuelve NULL si la tabla es demasiado chica o rala para que valga la pena.
ca_pregunta <- function(d, q, corte, diccionario = NULL,
                        min_codigos = 4, min_grupos = 3, min_total = 60) {

  largo <- codigos_largo(d, q) |> filter(!is.na(.data[[corte]]))
  frec  <- largo |> count(cod) |> filter(n >= 5)
  largo <- largo |> filter(cod %in% frec$cod)

  tab <- table(bonito(largo$cod, diccionario), largo[[corte]])
  tab <- tab[rowSums(tab) > 0, colSums(tab) > 0, drop = FALSE]

  if (nrow(tab) < min_codigos || ncol(tab) < min_grupos || sum(tab) < min_total) return(NULL)

  ca <- ca_svd(tab)
  if (is.null(ca)) return(NULL)

  puntos <- bind_rows(ca$filas, ca$columnas)

  etiquetar <- if (requireNamespace("ggrepel", quietly = TRUE)) {
    function(...) ggrepel::geom_text_repel(..., size = 3.3, min.segment.length = 0.2,
                                           segment.alpha = 0.4, max.overlaps = 20)
  } else {
    function(...) geom_text(..., size = 3.3, vjust = -0.7)
  }

  p <- ggplot(puntos, aes(x = x, y = y, colour = tipo)) +
    geom_hline(yintercept = 0, colour = GRIS_C) +
    geom_vline(xintercept = 0, colour = GRIS_C) +
    geom_point(aes(size = peso), alpha = 0.85, show.legend = FALSE) +
    etiquetar(aes(label = etiqueta, fontface = ifelse(tipo == "Categoría", "bold", "plain")),
              show.legend = FALSE) +
    scale_colour_manual(values = c("Código" = AZUL, "Categoría" = NARANJA)) +
    scale_size_continuous(range = c(1.2, 4.5)) +
    labs(x = paste0("Dimensión 1 (", ca$inercia[1], "%)"),
         y = paste0("Dimensión 2 (", ca$inercia[2], "%)")) +
    theme_minimal(base_size = 11) +
    theme(plot.title       = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title       = element_text(colour = GRIS, size = 9),
          legend.position  = "none")

  attr(p, "inercia") <- ca$inercia
  p
}

# ---- 5. Citas --------------------------------------------------------------

#' Selecciona citas textuales por codigo.
#'
#' Criterios, en este orden:
#'   - el codigo tiene que ser el PRIMERO asignado (el mas relevante)
#'   - largo entre `min_car` y `max_car`, para que entre en la diapositiva
#'   - se diversifica por sexo y region antes de completar con el resto
#' El texto se transcribe tal cual: no se corrige ni se recorta.
citas <- function(d, q, n_por_codigo = 2, n_codigos = 5, min_car = 60, max_car = 400,
                  codigos = NULL, seed = 1234) {

  cc <- paste0("codigo_", q)
  set.seed(seed)

  base <- d |>
    select(texto = all_of(q), codigos_asignados = all_of(cc),
           sexo, edad_num, region) |>
    filter(!is.na(codigos_asignados), codigos_asignados != "ERROR",
           !is.na(texto), nzchar(str_squish(texto))) |>
    mutate(
      primero = str_squish(sub("\\s*;.*$", "", codigos_asignados)),
      largo   = nchar(texto),
      id      = paste0(
        coalesce(sexo, "s/d"),
        ifelse(is.na(edad_num), "", paste0(", ", round(edad_num))),
        ifelse(is.na(region), "", paste0(", ", as.character(region)))
      )
    )

  # por defecto, solo los codigos principales: la idea es ilustrar las posiciones
  # dominantes, no documentar cada categoria del codebook
  objetivo <- codigos %||% head(orden_codigos(codigos_largo(d, q)), n_codigos)

  map_dfr(objetivo, function(k) {

    cand <- base |> filter(primero == k)
    if (!nrow(cand)) return(tibble())

    en_rango <- cand |> filter(largo >= min_car, largo <= max_car)
    if (!nrow(en_rango)) en_rango <- cand |> slice_min(abs(largo - min_car), n = n_por_codigo)

    # una por combinacion de sexo y region primero, despues se completa
    diversas <- en_rango |>
      group_by(sexo, region) |>
      slice_sample(n = 1) |>
      ungroup()

    elegidas <- bind_rows(diversas, en_rango) |>
      distinct(texto, .keep_all = TRUE) |>
      slice_head(n = n_por_codigo)

    elegidas |>
      transmute(codigo = k, etiqueta = bonito(k), cita = texto, quien = id)
  })
}

# ---- 6. Diapositivas -------------------------------------------------------

#' Abre la plantilla y detecta el layout en blanco
abrir_plantilla <- function(ruta = "assets/plantilla_16x9.pptx") {
  if (!file.exists(ruta)) stop("No encuentro la plantilla: ", ruta)
  pres <- read_pptx(ruta)
  ls   <- layout_summary(pres)
  attr(pres, "master") <- ls$master[[1]]
  attr(pres, "layout") <- if ("Blank" %in% ls$layout) "Blank" else ls$layout[[1]]
  pres
}

nueva <- function(pres) {
  add_slide(pres, layout = attr(pres, "layout"), master = attr(pres, "master"))
}

#' Formato de parrafo, tolerante con versiones viejas de officer donde
#' fp_par() todavia no acepta line_spacing.
par_fmt <- function(align = "left", interlinea = 1.2, abajo = 0) {
  tryCatch(
    fp_par(text.align = align, line_spacing = interlinea, padding.bottom = abajo),
    error = function(e) fp_par(text.align = align, padding.bottom = abajo)
  )
}

#' Caja de texto con formato
texto <- function(pres, txt, left, top, width, height,
                  size = 14, color = "grey20", bold = FALSE, italic = FALSE,
                  align = "left", interlinea = 1.2) {

  fp  <- fp_text(font.size = size, bold = bold, italic = italic,
                 color = color, font.family = FUENTE)
  par <- par_fmt(align, interlinea)

  parrafos <- map(as.character(txt), ~ fpar(ftext(.x, fp), fp_p = par))

  # block_list() toma los parrafos como argumentos sueltos: si se le pasa una
  # lista queda anidada y officer escribe la forma sin texto adentro.
  cuerpo <- do.call(block_list, parrafos)

  ph_with(pres, value = cuerpo,
          location = ph_location(left = left, top = top, width = width, height = height))
}

#' Grafico de ggplot como imagen (png a 300 dpi)
grafico <- function(pres, gg, left, top, width, height) {
  if (is.null(gg)) return(pres)
  archivo <- tempfile(fileext = ".png")
  ggsave(archivo, gg, width = width, height = height, dpi = 300, bg = "white")
  ph_with(pres, value = external_img(archivo, width = width, height = height),
          location = ph_location(left = left, top = top, width = width, height = height))
}

#' Franja de titulo comun a las diapositivas de contenido
encabezado <- function(pres, titulo, bajada = NULL) {
  pres <- texto(pres, titulo, left = 0.5, top = 0.3, width = 9, height = 0.5,
                size = 20, color = AZUL, bold = TRUE)
  if (!is.null(bajada) && nzchar(bajada)) {
    pres <- texto(pres, bajada, left = 0.5, top = 0.85, width = 9, height = 0.45,
                  size = 11, color = GRIS, italic = TRUE)
  }
  pres
}

#' Caratula con los logos oficiales
slide_caratula <- function(pres, titulo, subtitulo, fecha,
                           logos = c("assets/logo_ijd_azul.png")) {

  pres <- nueva(pres)

  ancho_logo <- 3.6
  izq <- 0.6
  for (l in logos) {
    if (file.exists(l)) {
      pres <- ph_with(pres, external_img(l, width = ancho_logo, height = ancho_logo * 422 / 1983),
                      location = ph_location(left = izq, top = 0.45,
                                             width = ancho_logo, height = ancho_logo * 422 / 1983))
      izq <- izq + ancho_logo + 0.4
    }
  }

  pres <- texto(pres, titulo,    left = 0.6, top = 2.2,  width = 8.8, height = 0.9, size = 30, color = AZUL, bold = TRUE)
  pres <- texto(pres, subtitulo, left = 0.6, top = 3.15, width = 8.8, height = 0.6, size = 17, color = GRIS)
  pres <- texto(pres, fecha,     left = 0.6, top = 4.6,  width = 8.8, height = 0.4, size = 13, color = GRIS)
  pres
}

slide_seccion <- function(pres, numero, titulo) {
  pres <- nueva(pres)
  pres <- texto(pres, numero, left = 0.7, top = 1.9, width = 1.2, height = 1,
                size = 54, color = GRIS_C, bold = TRUE)
  pres <- texto(pres, titulo, left = 1.9, top = 2.1, width = 7.4, height = 1.4,
                size = 26, color = AZUL, bold = TRUE)
  pres
}

slide_indice <- function(pres, entradas) {
  pres <- nueva(pres)
  pres <- encabezado(pres, "Contenido")
  pres <- texto(pres, paste0(seq_along(entradas), ". ", entradas),
                left = 0.9, top = 1.4, width = 8.4, height = 3.6,
                size = 14, color = "grey20", interlinea = 1.9)
  pres
}

#' Diapositiva con un grafico grande y una nota al pie
slide_grafico <- function(pres, titulo, gg, bajada = NULL, nota = NULL) {
  if (is.null(gg)) return(pres)
  pres <- nueva(pres)
  pres <- encabezado(pres, titulo, bajada)
  pres <- grafico(pres, gg, left = 0.6, top = 1.35, width = 8.8, height = 3.6)
  if (!is.null(nota)) {
    pres <- texto(pres, nota, left = 0.6, top = 5.05, width = 8.8, height = 0.35,
                  size = 9, color = GRIS, italic = TRUE)
  }
  pres
}

#' Enunciado textual de la pregunta, como apertura del modulo
slide_pregunta <- function(pres, id_pregunta, enunciado, n_respuestas) {
  pres <- nueva(pres)
  pres <- texto(pres, id_pregunta, left = 0.6, top = 0.45, width = 8.8, height = 0.4,
                size = 12, color = GRIS, bold = TRUE)
  pres <- texto(pres, paste0("“", str_squish(enunciado), "”"),
                left = 0.6, top = 1.1, width = 8.8, height = 2.8,
                size = 20, color = AZUL, interlinea = 1.35)
  pres <- texto(pres, glue("{n_respuestas} respuestas"),
                left = 0.6, top = 4.5, width = 8.8, height = 0.4,
                size = 12, color = GRIS)
  pres
}

#' Citas textuales agrupadas por codigo. Divide en varias diapositivas.
slide_citas <- function(pres, titulo, tabla_citas, por_slide = 4) {

  if (!nrow(tabla_citas)) return(pres)

  bloques <- split(seq_len(nrow(tabla_citas)),
                   (seq_len(nrow(tabla_citas)) - 1) %/% por_slide)

  fp_cita  <- fp_text(font.size = 11, italic = TRUE, color = "grey15", font.family = FUENTE)
  fp_quien <- fp_text(font.size = 10, color = GRIS, font.family = FUENTE)
  fp_cod   <- fp_text(font.size = 11, bold = TRUE, color = AZUL, font.family = FUENTE)
  par      <- par_fmt("left", 1.15, abajo = 8)

  for (b in bloques) {

    sub <- tabla_citas[b, ]
    pres <- nueva(pres)
    pres <- encabezado(pres, titulo, "Citas textuales, sin editar")

    parrafos <- list()
    codigo_previo <- ""

    for (i in seq_len(nrow(sub))) {
      if (sub$etiqueta[i] != codigo_previo) {
        parrafos <- c(parrafos, list(fpar(ftext(sub$etiqueta[i], fp_cod), fp_p = par)))
        codigo_previo <- sub$etiqueta[i]
      }
      parrafos <- c(parrafos, list(fpar(
        ftext(paste0("“", str_squish(sub$cita[i]), "” "), fp_cita),
        ftext(paste0("(", sub$quien[i], ")"), fp_quien),
        fp_p = par
      )))
    }

    pres <- ph_with(pres, value = do.call(block_list, parrafos),
                    location = ph_location(left = 0.6, top = 1.35, width = 8.8, height = 3.8))
  }

  pres
}
