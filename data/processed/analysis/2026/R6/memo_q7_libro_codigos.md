# Libro de codigos q7 - R6 2026

## Fuente y alcance

Este memo se basa en la lectura de `q7` en `data/processed/transcriptions/output/2026/transcripcion_R6.csv`.

La pregunta refiere a la opinion sobre una medida del Plan Nacional de Seguridad Publica basada en uso intensivo de tecnologia: videovigilancia masiva, cruce de datos interinstitucionales, historiales de salud y trazabilidad de ciudadanos.

El objetivo del memo es fijar un libro de codigos que capture las posturas principales frente a la medida y, al mismo tiempo, preserve la distincion entre beneficios y riesgos que aparece de forma muy recurrente en las respuestas.

## Lectura analitica

`q7` no replica la logica de `q3`. En esta pregunta muchas respuestas no se ordenan como apoyo o rechazo lineal, sino como balance entre utilidad y amenaza.

Empiricamente aparecen cinco grandes patrones:

- apoyos netos, que ven a la tecnologia como herramienta necesaria para seguridad, control o eficacia;
- apoyos condicionados, favorables solo si hay limites, regulacion y resguardo de datos;
- respuestas ambivalentes, que reconocen beneficios pero advierten riesgos serios, sobre todo en privacidad y libertades;
- criticas donde el riesgo domina y practicamente invalida la medida;
- escepticismo de implementacion, donde la objecion no es tanto normativo-sustantiva sino de humo, incapacidad estatal o repeticion de fracasos previos.

Por eso conviene un esquema de 3 capas:

- `postura_q7`: balance general;
- `beneficio_q7`: beneficio dominante;
- `riesgo_q7`: riesgo dominante.

Luego se deriva un `codigo_q7` plano para mantener compatibilidad con el flujo del repo.

## Capa 1: postura general

Asignar exactamente una postura por respuesta.

### `apoyo_neto`

La respuesta cierra claramente a favor, con beneficios dominantes y sin que los riesgos alteren el juicio general.

Ejemplos abreviados:
- `Solo veo beneficios. La tecnologia es fundamental`
- `Utilizar las herramientas en forma inteligente seria muy eficaz`

### `apoyo_condicionado_resguardos`

La respuesta apoya, pero solo bajo condiciones explicitas de resguardo, regulacion, confidencialidad, uso correcto o limites claros.

Ejemplos abreviados:
- `Podria ser beneficioso si los datos estan realmente resguardados`
- `Es util si hay limites claros y buen control`

### `ambivalente_tradeoff`

La respuesta explicita beneficios y riesgos en tension sin cerrar claramente a favor o en contra.

Ejemplos abreviados:
- `Puede ayudar, pero se pierde privacidad`
- `Es acertada, aunque preocupa que estemos bajo una lupa`

### `critica_riesgo_dominante`

La respuesta entiende que los riesgos pesan mas que los beneficios o directamente niega que haya beneficios.

Ejemplos abreviados:
- `Beneficios ninguno, riesgos todos`
- `No le veo beneficios, solo va a complicar a los buenos ciudadanos`

### `escepticismo_inviabilidad`

La respuesta duda de la implementacion real o la presenta como humo, mas de lo mismo o un fracaso probable.

Ejemplos abreviados:
- `Es todo humo`
- `Ya lo intentaron antes y quedo en la nada`

### `ns_nr_desinformado`

La respuesta expresa desconocimiento, falta de opinion o no tiene densidad suficiente para clasificar postura.

Ejemplos abreviados:
- `No sabria opinar`
- `No lo he analizado`

## Capa 2: beneficio principal

Asignar exactamente un beneficio principal.

### `eficacia_investigacion_prevencion`

El beneficio dominante es investigar mejor, prevenir delitos, mejorar seguridad o controlar mejor el crimen.

Marcadores tipicos:
- `investigaciones mas efectivas`
- `prevencion`
- `mejorar la seguridad`

### `celeridad_respuesta`

El beneficio dominante es actuar mas rapido, responder antes o acortar tiempos operativos.

Marcadores tipicos:
- `respuesta rapida`
- `acortar tiempos`
- `actuar mas rapido`

### `integracion_datos_diagnostico`

El beneficio dominante es el cruce de datos, historiales, antecedentes, diagnostico o seguimiento profundo de casos.

Marcadores tipicos:
- `cruce de datos`
- `historiales`
- `antecedentes`
- `diagnostico y seguimiento`

### `control_trazabilidad_disuasiva`

El beneficio dominante es mayor monitoreo, rastreo, trazabilidad o disuasion por vigilancia.

Marcadores tipicos:
- `mas controlado`
- `estar vigilados`
- `rastreable`
- `barreras para cometer crimen`

### `sin_beneficio_explicito`

No hay beneficio reconocible, se lo niega o la respuesta no permite asignar uno con seguridad.

## Capa 3: riesgo principal

Asignar exactamente un riesgo principal.

### `privacidad_libertades_vigilancia`

El riesgo dominante es la perdida de privacidad, libertades o el avance de una vigilancia excesiva.

Marcadores tipicos:
- `gran hermano`
- `panoptico`
- `control orwelliano`
- `perdida de privacidad`

### `seguridad_datos_filtracion_hackeo`

El riesgo dominante es hackeo, fuga, filtracion o mal resguardo tecnico de datos.

Marcadores tipicos:
- `hackeo`
- `ciberataque`
- `filtracion`
- `confidencialidad`

### `abuso_estatal_espionaje_desvio`

El riesgo dominante es el uso arbitrario o politico del sistema, incluyendo espionaje, abuso de funciones o persecucion.

Marcadores tipicos:
- `espionaje`
- `abuso de funciones`
- `uso inapropiado`
- `persecucion`

### `inoperancia_ineficacia`

El riesgo dominante es que la medida no funcione, no se implemente, repita fracasos previos o quede anulada por inoperancia.

Marcadores tipicos:
- `mas de lo mismo`
- `quedo en la nada`
- `no sabran implementarlo`
- `resultados nulos`

### `sin_riesgo_explicito`

No hay riesgo reconocible, se lo niega o la respuesta no permite asignarlo con seguridad.

## Regla de mapeo al CSV plano

El archivo `codigos_q7.csv` traduce las tres capas a un solo codigo, priorizando la postura general y usando beneficio o riesgo cuando agregan señal sustantiva.

- `apoyo_neto` + `integracion_datos_diagnostico` -> `apoyo_neto_integracion_datos`
- `apoyo_neto` + cualquier otro beneficio o `sin_beneficio_explicito` -> `apoyo_neto_seguridad_control`
- `apoyo_condicionado_resguardos` -> `apoyo_condicionado_resguardos`
- `ambivalente_tradeoff` + `privacidad_libertades_vigilancia` -> `tradeoff_privacidad_libertades`
- `ambivalente_tradeoff` + `seguridad_datos_filtracion_hackeo` o `abuso_estatal_espionaje_desvio` o `inoperancia_ineficacia` -> `tradeoff_gobernanza_datos`
- `critica_riesgo_dominante` + `privacidad_libertades_vigilancia` -> `critica_privacidad_libertades`
- `critica_riesgo_dominante` + `seguridad_datos_filtracion_hackeo` o `abuso_estatal_espionaje_desvio` o `inoperancia_ineficacia` -> `critica_datos_abuso`
- `escepticismo_inviabilidad` -> `escepticismo_inviabilidad`
- `ns_nr_desinformado` -> `ns_nr_desinformado`

## Reglas de decision

Usar esta precedencia para resolver traslapes en `postura_q7`:

1. Si la respuesta expresa desconocimiento o ausencia de opinion, usar `ns_nr_desinformado`.
2. Si el eje es humo, inviabilidad, mas de lo mismo o incapacidad estatal para implementarlo, usar `escepticismo_inviabilidad`.
3. Si el texto presenta el riesgo como dominante o niega beneficios, usar `critica_riesgo_dominante`.
4. Si apoya, pero solo con limites, regulacion, resguardo de datos o uso correcto, usar `apoyo_condicionado_resguardos`.
5. Si explicita beneficios y riesgos sin cerrar claramente a favor o en contra, usar `ambivalente_tradeoff`.
6. Si el balance es claramente favorable, usar `apoyo_neto`.

## Criterios finos para casos dudosos

- Si una respuesta enumera beneficio y riesgo, pero termina con una adhesion clara del tipo `esta bien igual`, clasificar `apoyo_condicionado_resguardos` si el apoyo depende de limites; si no, `apoyo_neto`.
- Si una respuesta trae beneficio y riesgo en espejo, sin cierre normativo claro, priorizar `ambivalente_tradeoff`.
- Si la critica principal es que la tecnologia puede derivar en vigilancia excesiva, aunque tambien se mencione hackeo o mal uso, priorizar `privacidad_libertades_vigilancia`.
- Si la critica principal es que el Estado no sabra implementar la medida o ya fracaso antes, priorizar `inoperancia_ineficacia` y postura `escepticismo_inviabilidad`.
- Si una respuesta solo dice `bien`, `perfecto`, `me parece buena`, usar `apoyo_neto`, `sin_beneficio_explicito` y `sin_riesgo_explicito`.

## Nota operativa

La implementacion runnable usara tres chats separados dentro de `code/analysis/2026/R6/codigos_q7.R`:

- uno para `postura_q7`;
- uno para `beneficio_q7`;
- uno para `riesgo_q7`.

Luego derivara `codigo_q7` en R para mantener compatibilidad con el flujo plano del repo.
