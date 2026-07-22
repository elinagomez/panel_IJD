# Revisión integral R8 2026

Fecha de generación: 2026-04-18

## Cobertura
- Decisiones auditadas: 2596
- `MANTENER`: 1112
- `CORREGIR`: 283
- `MISSING_VALIDO`: 533
- `SIN_ENCAJE_CODEBOOK`: 668
- `PENDIENTE`: 0

## Hallazgos de pipeline y prompt
- `q1/dimension_tematica_q1`: Código dominante: 'Crisis de Seguridad' en 92/118 (78.0%). Tras la revisión: corregir=4, subcodificación=3, sesgo_default=4, error_pipeline=1.
- `q1/tono_q1`: Código dominante: 'Neutral / Descriptivo' en 98/118 (83.1%). Tras la revisión: corregir=4, subcodificación=2, sesgo_default=1, error_pipeline=0.
- `q2/percepcion_sistema_q2`: Concentración en NA: 118/118; con texto utilizable en 116/118. Tras la revisión: corregir=38, subcodificación=28, sesgo_default=0, error_pipeline=17.
- `q4/argumentos_justificacion_q4`: Concentración en NA: 118/118; con texto utilizable en 111/118. Tras la revisión: corregir=83, subcodificación=64, sesgo_default=3, error_pipeline=18.
- `q6/influencia_externa_q6`: Código dominante: 'Sesgo Mediático (Veracidad)' en 106/118 (89.8%). Tras la revisión: corregir=3, subcodificación=0, sesgo_default=0, error_pipeline=1.

## Hallazgos semánticos
- `SUBCODIFICACION`: 196
- `SOBRECODIFICACION`: 42
- `SESGO_DE_DEFAULT`: 13
- `ERROR_PIPELINE`: 88

Dimensiones con mayor tasa de cambio:
- `q4 / 3. Argumentos (Justificación)`: tasa_cambio=70.3%, actual_top=`NA`, revisado_top=`NA`.
- `q2 / 2. Percepción del Sistema`: tasa_cambio=32.2%, actual_top=`NA`, revisado_top=`NA`.
- `q4 / 1. Postura Debate (Negro vs Bordaberry)`: tasa_cambio=23.7%, actual_top=`Pro-Bordaberry`, revisado_top=`NS / NC`.
- `q10 / Única`: tasa_cambio=11.0%, actual_top=`Escepticismo / Voluntad Individual`, revisado_top=`Función Institucional (Deber Ser)`.
- `q2 / 1. Postura General`: tasa_cambio=11.0%, actual_top=`Pro-Gobierno / Ministro`, revisado_top=`Pro-Gobierno / Ministro`.
- `q6 / 3. Argumentos de Confianza`: tasa_cambio=11.0%, actual_top=`Gestión / Resultados`, revisado_top=`NA`.
- `q8 / 1. Prioridad de Inversión`: tasa_cambio=11.0%, actual_top=`Ambas (Nuevas + Mejora)`, revisado_top=`Ambas (Nuevas + Mejora)`.
- `q4 / 2. Evaluación de la Oposición`: tasa_cambio=8.5%, actual_top=`Incorrecta`, revisado_top=`Incorrecta`.

## Codebook
- Casos con `requiere_cambio_codebook = TRUE`: 669
- Hay respuestas sustantivas sin encaje limpio en el codebook y conviene revisar esas categorías.

## Coherencia cruzada
- Alertas simples de coherencia detectadas: 99
- Estas alertas no implican error automático; sirven para un segundo control comparando cerrada y abierta.
