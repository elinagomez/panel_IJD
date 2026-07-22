library(readr)
library(dplyr)
library(tibble)
library(writexl)

config <- list(
  analysis_dir = file.path("data", "processed", "analysis", "2026", "R10"),
  input_csv = file.path("data", "processed", "analysis", "2026", "R10", "R10_muestra_verificacion_codificacion.csv"),
  output_csv = file.path("data", "processed", "analysis", "2026", "R10", "R10_verificacion_codificacion_20pct.csv"),
  output_xlsx = file.path("data", "processed", "analysis", "2026", "R10", "R10_verificacion_codificacion_20pct.xlsx")
)

issues <- tribble(
  ~pregunta, ~row_id, ~veredicto, ~observacion, ~codigos_sugeridos,
  "q1", 39, "posible_error", "La respuesta 'No tengo ninguno' no expresa una valoración sustantiva. No debería forzarse a satisfacción.", "",
  "q1", 108, "dudoso", "Los códigos asignados son pertinentes, pero la idea de baja general de nivel/exigencia también podría justificar deterioro_y_decadencia_historica.", "crisis_de_valores_y_respeto; baja_calidad_guarderia; deterioro_y_decadencia_historica",
  "q2", 9, "dudoso", "formacion_integral_y_para_la_vida es plausible; pensamiento_critico_y_ciudadania queda menos explícito en la respuesta.", "formacion_integral_y_para_la_vida",
  "q2", 39, "posible_error", "La respuesta 'Ni idea' no tiene contenido clasificable.", "",
  "q2", 41, "dudoso", "La respuesta solo indica prioridad, sin especificar qué papel debería cumplir la educación.", "",
  "q2", 71, "posible_error", "La respuesta 'No se' no tiene contenido clasificable.", "",
  "q2", 82, "dudoso", "La respuesta es demasiado genérica: 'Uno de los principales del país' indica importancia, pero no desarrolla el rol.", "",
  "q2", 93, "posible_error", "La respuesta 'Lo que ha sido siempre educar' es tautológica y no sustenta contencion_social_y_valores.", "",
  "q2", 103, "dudoso", "La respuesta 'El primero' solo marca prioridad, sin una dimensión sustantiva del codebook.", "",
  "q4", 35, "posible_error", "La respuesta abierta es solo '2'; no contiene información sobre el programa.", "",
  "q4", 45, "dudoso", "La idea de compromiso de la comunidad podría ser participación/relación territorial más que intervención estatal multidisciplinaria.", "diagnostico_y_participacion_censo",
  "q4", 49, "posible_error", "La respuesta abierta es solo '1'; no contiene información sobre el programa.", "",
  "q5", 23, "dudoso", "La respuesta 'En todo sentido' es muy genérica; intervención_integral_multifocal puede ser aceptable, pero queda débilmente justificada.", "intervencion_integral_multifocal",
  "q5", 39, "posible_error", "La respuesta 'No opino' no tiene contenido clasificable.", "",
  "q5", 84, "posible_error", "La respuesta 'No sabría decir' no tiene contenido clasificable.", "",
  "q5", 85, "posible_error", "'Sacar delincuentes' no refiere específicamente a narcotráfico; calza mejor con control territorial estricto o una categoría general de control.", "control_territorial_estricto",
  "q5", 91, "dudoso", "Consumo e inseguridad sugieren narcotráfico/seguridad; prevencion_y_enfoque_social e integral quedan poco explícitos.", "combate_directo_al_narcotrafico; disuasion_y_presencia_activa",
  "q5", 100, "posible_error", "La respuesta 'Si creo que si' no indica qué acciones debería hacer el Ministerio.", "",
  "q5", 114, "posible_error", "La respuesta 'No hay más preguntas?' no tiene contenido clasificable.", "",
  "q6", 69, "dudoso", "Declara que no ve riesgo, pero la mención a muchos ministerios apunta más a coordinación/ineficacia que a desvío de funciones.", "sin_riesgos_identificados",
  "q6", 97, "posible_error", "La respuesta 'No lo sé' no equivale a sin riesgos identificados.", "",
  "q6", 104, "posible_error", "La respuesta dice que sí identifica riesgo y que no sería beneficioso; el código sin_riesgos_identificados contradice el sentido.", "",
  "q7", 34, "posible_error", "'Bien' expresa aprobación mínima; desconocimiento_persistente no corresponde.", "aprobacion_de_la_integralidad",
  "q7", 43, "dudoso", "La evaluación negativa es clara, pero no explicita propaganda/populismo ni una referencia histórica; podría bastar una categoría de ineficacia si existiera.", "utopia_e_ineficacia_historica",
  "q7", 72, "dudoso", "'Bien pero no es fácil' combina aprobación y duda; podría agregarse aprobacion_de_la_integralidad.", "aprobacion_de_la_integralidad; escepticismo_sobre_la_ejecucion",
  "q7", 98, "posible_error", "La respuesta 'No' no permite inferir desconocimiento persistente ni una opinión sustantiva.", "",
  "q7", 104, "dudoso", "La condición 'si se hace de la manera correcta' justifica escepticismo, pero también hay aprobación básica.", "aprobacion_de_la_integralidad; escepticismo_sobre_la_ejecucion",
  "q10", 58, "posible_error", "'En todo manera' es demasiado inespecífico para asignar erosión de confianza.", "",
  "q10", 70, "dudoso", "La respuesta solo dice que afecta negativamente; debilitamiento_operativo_y_del_estado_de_derecho es posible, pero muy inferido.", "debilitamiento_operativo_y_del_estado_de_derecho",
  "q10", 84, "dudoso", "La respuesta es vaga: 'se habla mucho pero no se atiende' no describe claramente el mecanismo de afectación.", "",
  "q12", 34, "posible_error", "La respuesta 'No' indica ausencia de preocupación, pero no necesariamente la lógica 'quien no debe no teme'.", "sin_preocupacion_apoyo_total",
  "q12", 57, "dudoso", "'Ninguna siempre que sean bien usadas' apoya la tecnología pero introduce condición sobre uso; podría agregarse uso_indebido_y_falta_de_control.", "sin_preocupacion_apoyo_total; uso_indebido_y_falta_de_control",
  "q12", 71, "posible_error", "'No lo se' no sustenta ia_como_amenaza_desconocida.", "",
  "q12", 98, "posible_error", "'De acuerdo' parece respuesta a la cerrada, no una preocupación abierta clasificable.", ""
)

muestra <- read_csv(config$input_csv, show_col_types = FALSE, col_types = cols(.default = "c")) |>
  mutate(row_id = as.integer(row_id))

verificada <- muestra |>
  select(-veredicto, -observacion, -codigos_sugeridos) |>
  left_join(issues, by = c("pregunta", "row_id")) |>
  mutate(
    veredicto = coalesce(veredicto, "ok"),
    observacion = coalesce(observacion, "La codificación asignada es consistente con la respuesta y el codebook."),
    codigos_sugeridos = coalesce(codigos_sugeridos, codigos_asignados)
  ) |>
  arrange(pregunta, row_id)

resumen_pregunta <- verificada |>
  count(pregunta, veredicto, name = "n") |>
  group_by(pregunta) |>
  mutate(pct = n / sum(n)) |>
  ungroup()

resumen_total <- verificada |>
  count(veredicto, name = "n") |>
  mutate(pct = n / sum(n))

write_csv(verificada, config$output_csv, na = "")
write_xlsx(
  list(
    verificacion = verificada,
    resumen_pregunta = resumen_pregunta,
    resumen_total = resumen_total
  ),
  path = config$output_xlsx
)

cat("Archivos generados:\n")
cat("- ", config$output_csv, "\n", sep = "")
cat("- ", config$output_xlsx, "\n", sep = "")
cat("\nResumen total:\n")
print(resumen_total)
cat("\nResumen por pregunta:\n")
print(resumen_pregunta, n = Inf)
