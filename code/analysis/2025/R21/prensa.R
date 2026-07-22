# librerías
library(DBI)
library(glue)
library(dplyr)
library(stringr)
library(purrr)
library(tidyr)
library(ggplot2)
library(showtext)
library(ggbump)
library(lubridate)
library(lubridate)

# parámetros
meses            <- c("202509", "202510")             # meses seleccionados (formato yyyymm)
keywords_buscar  <- c("morosini", "rural del prado", "uruguay impulsa", "presupuesto", "siniestro", "orsi", "onu", "eutanasia", "el pelón", "homicidio", "mónica ferrero")         # vector de palabras clave a buscar
pal_base <- c("#4E79A7", "#59A14F", "#E15759", "#76B7B2", "#9C755F",
  "#B07AA1", "#F28E2B", "#EDC948", "#FF9DA7", "#BAB0AC",
  "#AEC7E8", "#2CA02C", "#D62728", "#17BECF", "#8C564B",
  "#9467BD", "#BCBD22", "#7F7F7F", "#1F77B4", "#98DF8A")
archivo_menciones <- "plots/menciones_prensa.png"
archivo_ranking   <- "plots/ranking_prensa.png"

# conexión
source("code/utils/pg_connect.R")
con <- pg_connect()

# preparar tsquery
kw_raw  <- trimws(keywords_buscar)
if (length(kw_raw) == 0) stop("Debe proporcionar al menos una palabra clave en 'keywords_buscar'.")

kw_proc <- sapply(kw_raw, function(k) {
  k <- trimws(k)
  if (!grepl(" ", k) && grepl("\\*$", k)) k <- sub("\\*$", ":*", k)
  if (grepl(" ", k)) k <- paste(strsplit(k, "\\s+")[[1]], collapse = " <-> ")
  k
})
tsq     <- paste(kw_proc, collapse = " | ")
clean_kw <- trimws(gsub("\\*$", "", kw_raw))  # para detección en R

# extraer títulos que coincidan en title
titulos <- purrr::map_dfr(meses, function(mm) {
  tabla <- DBI::SQL(paste0("news.m", mm))
  q <- glue_sql("
    SELECT
      m.id_source,
      m.id_uri,
      m.fecha,
      m.hora,
      m.title,
      m.post_url,
      m.body
    FROM {tabla} AS m
    WHERE
      to_tsvector('spanish', lower(coalesce(m.title, '')))
        @@ to_tsquery('spanish', {tolower(tsq)})
  ", .con = con)
  DBI::dbGetQuery(con, q)
})

if (nrow(titulos) == 0) stop("No hay títulos coincidentes en el período seleccionado.")

# join con sources y deptos
sources <- dbGetQuery(con, "
  SELECT id       AS id_source,
         medio    AS medio,
         cod_dpto
    FROM news.sources
")
deptos  <- dbGetQuery(con, "
  SELECT cod_dpto,
         departamento
    FROM news.deptos
")

titulos <- titulos %>%
  mutate(id_source = str_remove(id_source, "^s")) %>% 
  left_join(sources, by = "id_source") %>%
  left_join(deptos,   by = "cod_dpto")

# detectar qué keyword(s) aparecen en cada título
titulos <- titulos %>%
  mutate(
    matched_keywords = map_chr(title, function(txt) {
      found <- clean_kw[str_detect(txt, regex(clean_kw, ignore_case = TRUE))]
      paste(unique(found), collapse = ", ")
    })
  )

titulos <- titulos %>% 
  filter(matched_keywords != "")

titulos <- titulos %>% select(title, fecha, medio, body, post_url, departamento, matched_keywords)

if (nrow(titulos) == 0) stop("No se encontraron menciones para las palabras clave proporcionadas.")

# preparar tipografía
font_add_google("Montserrat", "Montserrat")
showtext_auto()

# preparar datos para la gráfica (agregado semanal)
menciones_semanales <- titulos %>%
  mutate(
    fecha = as.Date(fecha),
    matched_keywords = str_split(matched_keywords, pattern = ",\\s*"),
    semana = floor_date(fecha, unit = "week", week_start = 1)
  ) %>%
  unnest(matched_keywords) %>%
  mutate(matched_keywords = str_trim(matched_keywords)) %>%
  filter(matched_keywords != "", !is.na(matched_keywords), !is.na(semana)) %>%
  group_by(semana, matched_keywords) %>%
  summarise(menciones = n(), .groups = "drop") %>%
  mutate(
    matched_keywords = factor(
      matched_keywords,
      levels = unique(clean_kw[clean_kw %in% matched_keywords])
    )
  ) %>%
  arrange(matched_keywords, semana)

if (nrow(menciones_semanales) == 0) stop("No se encontraron menciones para las palabras clave proporcionadas.")

niveles_kw <- levels(menciones_semanales$matched_keywords)

if (length(niveles_kw) > length(pal_base)) {
  colores_usados <- rep(pal_base, length.out = length(niveles_kw))
} else {
  colores_usados <- pal_base[seq_along(niveles_kw)]
}

names(colores_usados) <- niveles_kw

grafica_menciones <- ggplot(menciones_semanales, aes(x = semana, y = menciones, color = matched_keywords)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = colores_usados, drop = FALSE) +
  labs(
    title = "Menciones por palabra clave",
    subtitle = glue("Semanas de {min(menciones_semanales$semana)} a {max(menciones_semanales$semana)}"),
    x = "Semana",
    y = "Número de menciones",
    color = "Palabra clave"
  ) +
  scale_x_date(date_breaks = "1 week", date_labels = "%d-%b") +
  theme_minimal(base_family = "Montserrat") +
  theme(
    plot.title = element_text(face = "bold", size = 16*3),
    plot.subtitle = element_text(size = 12*3),
    axis.title = element_text(size = 12*3),
    legend.title = element_text(size = 11*3),
    legend.text = element_text(size = 10*3),
    axis.text.x = element_text(size = 11*3, margin = margin(t = 5), color = "#444444"),
    axis.text.y = element_text(size = 11*3, color = "#444444")
  )

# preparar ranking semanal para gráfico tipo bump
ranking_semanal <- menciones_semanales %>%
  group_by(semana) %>%
  mutate(ranking = dense_rank(desc(menciones))) %>%
  ungroup()

max_rank <- max(ranking_semanal$ranking, na.rm = TRUE)

grafica_ranking <- ggplot(ranking_semanal, aes(x = semana, y = ranking, color = matched_keywords)) +
  geom_bump(size = 1.6, smooth = 10) +
  geom_point(aes(fill = matched_keywords), size = 4.5, shape = 21, stroke = 1.2, color = "white") +
  scale_color_manual(
    values = colores_usados,
    drop = FALSE,
    guide = guide_legend(override.aes = list(size = 4, fill = NA))
  ) +
  scale_fill_manual(values = colores_usados, guide = "none") +
  scale_y_reverse(breaks = seq_len(max_rank)) +
  scale_x_date(date_breaks = "1 week", date_labels = "%d-%b") +
  labs(
    title = "Ranking semanal por menciones en la prensa",
    x = "Semana",
    y = "",
    color = "Palabra clave"
  ) +
  theme_minimal(base_family = "Montserrat") +
  theme(
    plot.title = element_text(face = "bold", size = 14*3),
    axis.title = element_text(size = 11*3, color = "#333333"),
    axis.text.x = element_text(size = 11*3, margin = margin(t = 5), color = "#444444"),
    axis.text.y = element_text(size = 11*3, color = "#444444"),
    # panel.background = element_rect(fill = "#FBFBFB", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "right",
    legend.title = element_text(size = 10*3, color = "#333333", face = "bold"),
    legend.text = element_text(size = 9*3, color = "#333333"),
    legend.key = element_rect(fill = "white", color = NA),
    # legend.background = element_rect(fill = "#F7F7F7", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(20, 40, 20, 40)
  )

print(grafica_menciones)
print(grafica_ranking)

if (!dir.exists(dirname(archivo_menciones))) dir.create(dirname(archivo_menciones), recursive = TRUE)
ggsave(
  filename = archivo_menciones,
  plot = grafica_menciones,
  width = 12,
  height = 7,
  dpi = 300
)

if (!dir.exists(dirname(archivo_ranking))) dir.create(dirname(archivo_ranking), recursive = TRUE)
ggsave(
  filename = archivo_ranking,
  plot = grafica_ranking,
  width = 12,
  height = 8,
  dpi = 300
)

message(glue("Gráficas guardadas en {archivo_menciones} y {archivo_ranking}"))
