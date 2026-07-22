library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(quanteda)
library(stringi)
year <- 2025
round_id <- "R11"


norm_txt <- function(x) {
  x <- ifelse(is.na(x), "", x)
  x |>
    stringi::stri_trans_general("Latin-ASCII") |>
    tolower() |>
    stringr::str_squish()
}

dict_q3 <- dictionary(list(
  equidad_apoyo = c(
    "igualdad*", "equidad*", "inclusion*", "apoyo*", "tutori*", "refuerzo*",
    "acompan*", "beca*", "vulnerabl*", "oportunidad*", "contencion*",
    "orientacion*", "familia*", "trayectoria*",
    "leer*", "escrib*", "subsidio*", "ropa", "calzado","ayud*"
  ),

  jornada_tiempo_completo_alimentacion = c(
    "jornada*", "completo*", "tiempo*", "alimentacion*", "comedor*",
    "merienda*", "desayuno*", "almuerzo*", "horario*", "turno*"
  ),

  infraestructura_recursos = c(
    "infraestructur*", "edilicia*", "aula*", "bano*", "banos*", "techo*",
    "calefacci*", "conectividad*", "internet*", "wifi", "biblioteca*",
    "laboratorio*", "material*", "utile*", "uniforme*", "equip*",
    "mantenimiento*", "construccion*", "escuela*", "liceo*", "secundari*",
    "centro*", "edificio*", "insumo*", "silla*", "mesa*", "efectiv*"
  ),

  docentes_formacion_condiciones = c(
    "docente*", "maestr*", "profesor*", "plantel*", "cuerpo*", "formacion*",
    "capacit*", "actualizacion*", "concurs*", "nombramiento*", "hora*",
    "carga*", "laboral*", "salario*", "sueldo*", "compromiso*", "acompan*",
    "seguimiento*", "adscrip*", "taller*", "evaluador*", "paro*"
  ),

  programas_innovacion_tecnologia = c(
    "tecnolog*", "computador*", "computadora*", "pc", "internet*", "tableta*",
    "tablet*", "plataforma*", "virtual*", "digital*", "innovacion*", "innovar*",
    "robotic*", "programacion*", "stem", "ciencia*", "curriculo*", "curricular*",
    "contenido*", "actualizaci*", "laboratorio*", "software*", "app", "datos*",
    "programa*", "reforma*", "real", "enfoque*", "metodolog*", "metodo*",
    "idioma*", "competencia*", "habilidad*", "proyecto*", "extracurricul*", 
    "matematic*", "fisica*", "quimic*", "biologi*", "deporte*", "arte*", "musica*"
  ),

  evaluacion_promocion_disciplina = c(
    "repetir*", "repeti*", "promocion*", "evaluacion*", "prueba*", "examen*",
    "diagnostic*", "convivencia*", "disciplina*", "conducta*", "asistencia*",
    "calificaci*", "nota*", "regimen*", "control*", "exigen*", "exigir*"
  ),

  asistencia_ausentismo_abandono = c(
    "asistenci*", "ausent*", "inasist*", "faltar*", "falta*", "abandon*",
    "desvincul*", "desercion*", "revincul*", "reingres*", "retencion*",
    "riesgo*", "extraedad*", "sobreedad*", "rezago*", "buscar*", "visita*",
    "domiciliari*", "barrido*", "seguir*", "convocar*", "reclamar*",
    "presentismo*", "present*", "deserc*", "materia*", "presencial*",
    "busque*", "seguimient*"
  ),

  primera_infancia_inicial = c(
    "inicial*", "jardin*", "preescolar*", "caif", "ninez", "nino*", "infanci*",
    "sala*", "maternal*", "estimulacion*"
  ),

  acceso_media_terciaria = c(
    "terciari*", "carrera*", "universidad*", "orientacion*", "utec", "utu",
    "fpb", "bachillerato*", "nocturno*", "ingreso*", "egreso*", "pasant*",
    "practic*", "emple*", "trabaj*", "capital", "unibercidad", "centrali*"
  ),

  salud_mental = c(
    "convivencia*", "contencion*", "violencia*", "bullying", "acoso*",
    "mediacion*", "conflicto*", "norma*", "reglamento*", "respeto*",
    "vinculo*", "comunidad*", "salud", "mental", "emocion*", "psico*",
    "estres*", "ansiedad*", "depresion*", "suicidio*", "sentimiento*",
    "bowling"
  ),

  presupuesto_gestion = c(
    "presupuesto*", "recurso*", "inversion*", "invertir*", "gestion*",
    "planificacion*", "priorizar*", "financiamien*", "partida*", "gasto*",
    "porcentaje*"
  )
))


encuesta <- readxl::read_excel(paste0("data/processed/analysis/", year, "/", round_id, "/r11_q2.xlsx")) |>
  mutate(q3_norm = norm_txt(q3))

toks <- tokens(encuesta$q3_norm, remove_punct = TRUE, remove_numbers = TRUE)

mat <- dfm(toks) |>
  dfm_lookup(dict_q3, valuetype = "glob", case_insensitive = TRUE)

cats <- names(dict_q3)

tags <- convert(mat, to = "data.frame") |>
  tibble::as_tibble() |>
  select(any_of(cats))

out <- encuesta |>
  bind_cols(tags) |>
  rowwise() |>
  mutate(
    etiquetas = {
      v <- c_across(all_of(cats))
      paste(cats[!is.na(v) & v > 0], collapse = "; ")
    }
  ) |>
  ungroup()

prioridad <- names(dict_q3)

out <- out |>
  rowwise() |>
  mutate(
    etiqueta_principal = {
      v <- c_across(all_of(prioridad))
      idx <- which(!is.na(v) & v > 0)
      if (length(idx)) prioridad[min(idx)] else NA_character_
    }
  ) |>
  ungroup()

cats_rank <- prioridad
rank <- setNames(seq_along(cats_rank), cats_rank)

count_mat <- out |>
  select(all_of(cats_rank)) |>
  as.data.frame()

ord_list <- apply(count_mat, 1, function(v) {
  pres <- names(v)[!is.na(v) & v > 0]
  if (length(pres) == 0) return(character(0))
  v_pres <- as.numeric(v[pres])
  o <- order(-v_pres, rank[pres], pres)
  pres[o]
})

k <- 5

for (i in seq_len(k)) {
  out[[paste0("q3_", i)]] <- vapply(
    ord_list,
    function(x) if (length(x) >= i) x[[i]] else NA_character_,
    character(1)
  )
}

# Eliminar las columnas de quanteda y la columna normalizada
out <- out |>
  relocate(starts_with("q3_"), .after = q3) |> 
  select(-q3_norm, -all_of(cats))

resumen <- out |>
  # Recalcular el resumen usando solo las columnas finales
  select(starts_with("q3_")) |>
  pivot_longer(everything(), names_to = "posicion", values_to = "categoria") |>
  filter(!is.na(categoria)) |>
  count(categoria, name = "n") |>
  arrange(desc(n))

print(resumen)

openxlsx::write.xlsx(out, paste0("data/processed/analysis/", year, "/", round_id, "/r11_q3.xlsx"))
