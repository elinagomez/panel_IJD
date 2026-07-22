# Libro de codigos q3 - R6 2026

## Fuente y alcance

Este memo se basa en la lectura de `q3` en `data/processed/transcriptions/output/2026/transcripcion_R6.xlsx`.

La pregunta refiere a la opinion sobre una caracteristica puntual del Plan Nacional de Seguridad Publica: que el plan sea interinstitucional y articule Ministerio del Interior, Fiscalia, Poder Judicial, Mides, INAU, ANEP, gobiernos departamentales y municipios.

El objetivo de este memo es fijar un libro de codigos que capture las posturas principales frente a esa caracteristica, no frente a todo el plan de seguridad en general.

## Lectura analitica

La gran mayoria de las respuestas valora positivamente la coordinacion entre organismos. Dentro de ese bloque favorable aparecen tres lógicas distintas:

- apoyo generico, sin mayor desarrollo;
- apoyo porque la seguridad es un problema integral y no se resuelve solo con policia;
- apoyo porque varios organismos pueden aportar responsabilidades, herramientas y miradas complementarias.

Tambien aparece un subconjunto relevante de apoyos condicionados: personas que estan de acuerdo solo si la coordinacion funciona de verdad, se implementa bien, no queda en el papel o produce resultados concretos.

En la periferia hay dos tipos de critica que conviene separar:

- escepticismo sobre la coordinacion como retorica, burocracia o algo ya sabido;
- rechazo mas principista que sostiene que la seguridad compete centralmente al Ministerio del Interior.

Por ultimo, hay pocos casos de desconocimiento o falta de opinion.

## Capa 1: postura general

Asignar exactamente una postura por respuesta.

### `favorable_claro`

Aprueba la caracteristica del plan sin reparos sustantivos.

Ejemplos abreviados:
- `Perfecto`
- `Me parece muy acertada`
- `Es el camino aunar esfuerzos`

### `favorable_condicionado`

Aprueba, pero solo si la coordinacion se implementa de verdad, se ejecuta bien o muestra resultados.

Ejemplos abreviados:
- `Correcto si es real que se puede articular todo eso`
- `Si se llevara a cabo seria genial`
- `La idea es muy buena, pero su exito depende de que trabajen de verdad en conjunto`

### `critico_esceptico`

Cuestiona la utilidad real; lo ve como humo, burocracia, dilucion, chachara o como algo no novedoso.

Ejemplos abreviados:
- `Una payasada`
- `Puro humo todo lo dicho`
- `No le veo nada de novedoso`

### `rechazo_enfoque`

Rechaza que la seguridad se aborde asi porque entiende que debe quedar centralmente en Interior o en la policia.

Ejemplos abreviados:
- `La seguridad publica le corresponde solo al Ministerio del Interior`
- `El Ministerio de Interior se tiene que encargar de la seguridad y punto`

### `ns_nr_desinformado`

No sabe, no escucho, no tiene opinion o la respuesta es demasiado ambigua para codificar con seguridad.

Ejemplos abreviados:
- `No lo se`
- `No he oido ni visto nada`
- `Ninguna opinion`

## Capa 2: fundamento principal

Asignar exactamente un fundamento principal por respuesta.

### `integral_multicausal`

El fundamento es que la seguridad no se resuelve solo con policia o represion, sino con una mirada integral, social, preventiva o multicausal.

Marcadores tipicos:
- `la seguridad no se arregla solo con policia`
- `plan integral`
- `tema multifactorial`
- `distintos angulos`

### `coordinacion_responsabilidad_compartida`

El fundamento es que distintos organismos deben colaborar, sumar capacidades, compartir responsabilidades o aportar miradas complementarias.

Marcadores tipicos:
- `todos juntos`
- `cada organismo puede aportar`
- `aunar esfuerzos`
- `participacion plural`

### `implementacion_real`

El foco esta en ejecucion, cumplimiento, coordinacion efectiva, independencia entre organismos, control o resultados.

Marcadores tipicos:
- `si se implementa correctamente`
- `si logran coordinar`
- `hay que esperar resultados`
- `no solo en el papel`

### `antiburocracia_no_novedad`

Critica comisiones, dilucion, formalismo, retorica o falta de novedad.

Marcadores tipicos:
- `humo`
- `payasada`
- `mas de lo mismo`
- `solo genera burocracia`

### `interior_central`

Afirma que la competencia principal debe ser del Ministerio del Interior o la policia.

Marcadores tipicos:
- `le corresponde solo al Ministerio del Interior`
- `Interior y punto`

### `sin_fundamento_explicito`

Hay apoyo o rechazo, pero sin una razon desarrollada.

Marcadores tipicos:
- `perfecto`
- `muy bien`
- `bien`

## Regla de mapeo al CSV plano

El archivo `codigos_q3.csv` traduce las dos capas a un solo codigo mutuamente excluyente, para mantener compatibilidad con el flujo actual del repo.

- `favorable_claro` + `sin_fundamento_explicito` -> `favor_generico`
- `favorable_claro` + `integral_multicausal` -> `favor_integral_multicausal`
- `favorable_claro` + `coordinacion_responsabilidad_compartida` -> `favor_coordinacion_compartida`
- `favorable_condicionado` + cualquier fundamento -> `favor_condicionado_implementacion`
- `critico_esceptico` + `antiburocracia_no_novedad` -> `critica_burocracia_no_novedad`
- `rechazo_enfoque` + `interior_central` -> `rechazo_solo_interior`
- `ns_nr_desinformado` -> `ns_nr_desinformado`

## Reglas de decision

Usar esta precedencia para resolver traslapes:

1. Si la respuesta declara desconocimiento o ausencia de opinion, codificar `ns_nr_desinformado`.
2. Si rechaza explicitamente la coordinacion porque la seguridad le compete a Interior, priorizar `rechazo_enfoque`.
3. Si denuncia humo, burocracia, dilucion o no novedad, priorizar `critico_esceptico`.
4. Si el apoyo depende de implementacion, coordinacion efectiva o resultados, priorizar `favorable_condicionado`.
5. Si apoya sin condicion, distinguir entre `integral_multicausal`, `coordinacion_responsabilidad_compartida` y `sin_fundamento_explicito`.

## Criterios finos para casos dudosos

- Si una respuesta dice que la seguridad `no se resuelve solo con policia`, `es un tema multifactorial` o `requiere una mirada integral`, priorizar `integral_multicausal` aunque tambien mencione trabajo conjunto.
- Si la respuesta celebra que participen `varios organismos`, `todos juntos` o `cada uno aporte`, pero no desarrolla la idea de integralidad, usar `coordinacion_responsabilidad_compartida`.
- Si una respuesta combina apoyo con dudas de ejecucion, no usar un codigo favorable simple: pasa a `favor_condicionado_implementacion`.
- Si una respuesta critica que esto ya deberia pasar o que no hay nada nuevo, pero no niega la coordinacion en si, usar `critica_burocracia_no_novedad`.
- Si aparece solo `Ministerio del Interior` o un fragmento igual de breve sin postura interpretable, tratarlo como `ns_nr_desinformado`; para `rechazo_solo_interior` hace falta que la tesis de exclusividad sea entendible.

## Nota operativa

La implementacion runnable del repo usa dos chats separados dentro de `code/analysis/2026/R6/codigos_q3.R`:

- uno para clasificar `postura_q3`;
- otro para clasificar `fundamento_q3`.

Luego deriva `codigo_q3` para mantener compatibilidad con el flujo plano del repo.
