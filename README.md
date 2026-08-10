# Panel WhatsApp — Encuesta Permanente CISCo-IJD

Réplica del flujo del repo `panel_IJD`, arrancando de cero: **R1 / 2026** es la primera
consulta de este panel.

El panel se recluta desde la **encuesta telefónica de LimeSurvey** (línea de base, ola 1):
de ahí salen el celular y los demográficos. Después, cada ronda es una consulta corta por
WhatsApp, con respuestas por **texto y por audio**, así que antes de analizar hay que
transcribir los audios (Whisper en Colab).

## Estado

| Paso | Script | Estado |
|---|---|---|
| 0. Base de contactos (LimeSurvey → contacts) | `code/contacts_lime.R` | hecho — 1.317 contactos |
| 1. Matching encuesta ↔ contactos | `code/matching.R` | hecho — 150 personas, 187 audios |
| 2. Transcripción (Drive ↔ Colab Whisper) | `code/transcripciones.qmd` + `code/whisper_colab.ipynb` | **pendiente** — falta completar `drive:` en `project.yml` |
| 3. Base acumulada del año | — | pendiente (recién con la 2ª ronda) |
| 4. Codificación de abiertas con LLM | `DriveFlow/run.R` + `qualcode.R` | listo para probar — corre local con Ollama, codebook de ejemplo en `data/raw/codebook/` |
| 5. Revisión humana del 20% y acuerdo | `medir_acuerdo()` | pendiente — después de correr el paso 4 |
| Reporte de cobertura de la ronda | `code/reporte_panel.qmd` | hecho |

## Estructura

```
project.yml                       config del proyecto + ronda vigente (R1/2026)
DriveFlow/R1.yml                  config de codificación de esta ronda
code/
  contacts_lime.R                 LimeSurvey (xlsx) -> data/raw/contacts/<AAAAMMDD>.csv
  matching.R                      export de casos + contactos -> data/processed/matched/
  transcripciones.qmd             audios .ogg -> Drive -> Colab Whisper -> texto
  whisper_colab.ipynb             notebook a subir a Colab: A/<ronda> (.ogg) -> B/<ronda> (.txt)
  analisis_panel.R                métricas de cobertura/completitud (funciones)
  reporte_panel.qmd               reporte HTML de la ronda
data/
  raw/limesurvey/                 encuesta telefónica de base + diccionario
  raw/campaigns_wcx/              export crudo de la plataforma, por ronda
  raw/contacts/                   contactos con demográficos, por fecha de corte
  raw/transcriptions/2026/R1/     voice/ (.ogg descargados) y text/ (.txt del Colab)
  raw/acumulada/rounds/2026/      copia de cada ronda ya transcrita
  processed/matched/              encuesta cruzada con contactos, lista para transcribir
  processed/transcriptions/output/2026/   transcripcion_R1.csv / .xlsx
  processed/analysis/2026/R1/     tablas del reporte
plots/2026/R1/
```

## Cómo correrlo

```bash
Rscript code/contacts_lime.R            # 0) contactos desde LimeSurvey
Rscript code/matching.R                 # 1) cruce -> data/processed/matched/
quarto render code/transcripciones.qmd  # 2) audios (ver aviso abajo)
quarto render code/reporte_panel.qmd    # reporte de cobertura de la ronda
Rscript code/analisis_panel.R           # mismas métricas, sin HTML
```

`transcripciones.qmd` conviene correrlo **bloque por bloque** en RStudio, no de un tirón:
en el medio hay que ir a ejecutar el Colab de Whisper y esperar a que deje los `.txt` en Drive.

### El Colab

`code/whisper_colab.ipynb` se sube a Colab (Archivo → Subir notebook) con la misma cuenta
de Drive que figura en `project.yml`. Entorno de ejecución → GPU (T4). Usa `faster-whisper`
`large-v3` en español, saltea los audios que ya tienen `.txt`, y escribe cada transcripción
con **el mismo nombre que el audio** (`q3_fila57.ogg` → `q3_fila57.txt`): R usa ese nombre
para devolver el texto a su fila y su pregunta, así que no hay que renombrar nada.

El notebook del proyecto original nunca se versionó en `panel_IJD` (vive en la Drive de esa
cuenta), así que este es propio de este proyecto.

## Antes del paso 2, completar en `project.yml`

```yaml
drive:
  account_email: ""    # la cuenta de Google que usa el Colab
  folder_audio: "A"    # carpeta raíz de audios (hay que crear A/R1 a mano)
  folder_text:  "B"    # carpeta raíz de textos (hay que crear B/R1 a mano)
  questions_url: ""    # carpeta con el sheet "questions"
```

## Datos de la ronda R1 (extracción 2026-08-04 al 2026-08-10)

- **359 envíos** válidos (se excluyen 4 casos de prueba con respuestas `Q1`, `Q2`, …).
- **9 preguntas efectivas**: `q10` vino vacía en toda la base.
- **278 personas** contactadas, **150 respondieron** (54%), **115 completaron las 9** (41%).
- **1.196 respuestas**: 994 por texto y 202 por audio (17%).
- Base cruzada: 150 personas, **137 con match** en LimeSurvey y 13 sin match
  (quedan marcadas con `match_lime = 0`, no se descartan).
- **187 audios** a transcribir en el paso 2.

## Diferencias con el flujo de `panel_IJD`

| | `panel_IJD` | acá |
|---|---|---|
| Export de la plataforma | vista *analytics* (`Teléfono de Envío` + `Caso: qN`) | vista completa de casos (70 columnas) |
| Cruce | `inner_join` por `Teléfono de Envío` | `left_join` por los últimos 8 dígitos del teléfono, para no perder audios de gente sin match |
| Contactos | CSV mensual de la plataforma | derivados de la encuesta telefónica de LimeSurvey |
| Filtro de completadas | `filter(!is.na(q_ultima))` | se conservan las parciales con `n_resp` y `completa` como columnas |
| Duplicados | — | una persona puede tener varios envíos: se conserva su caso más completo |

## Codificación de preguntas abiertas (paso 4)

Corre con un modelo local: **Ollama + Qwen3**. Las respuestas textuales no salen de
la máquina, que es lo que corresponde con el consentimiento que firmaron los encuestados.

```bash
ollama pull qwen3:8b
ollama serve                 # queda escuchando en localhost:11434
Rscript DriveFlow/run.R      # o abrirlo en RStudio y correr por bloques
```

Insumos, los dos hechos a mano y editables en `data/raw/codebook/`:

- **`questions.xlsx`** — qué pregunta es abierta o cerrada, y de qué cerrada depende cada una.
- **`BookR1.xlsx`** (hoja `R1`) — el codebook: `etiqueta` + `descripcion` por pregunta.
  `type_enum()` restringe la salida a estas etiquetas: el modelo no puede inventar categorías.

Salidas en `data/processed/analysis/2026/R1/`:

- `R1_codificada.csv` — la base con una columna `codigo_<q>` por pregunta abierta.
- `QA_R1_sample20.csv` — muestra del 20% con la columna `codigo_<q>_rev` vacía para
  codificar a mano. Después, `medir_acuerdo(revisado)` devuelve por pregunta el % de
  coincidencia exacta, el % con al menos un código en común y el Jaccard promedio.
  Ese mismo archivo sirve para comparar modelos (`qwen3:8b` vs `14b`) con datos.

Convenciones de la salida: `NA` = la persona no respondió · `"ERROR"` = el modelo falló
en esa fila (no se mezclan) · varios códigos separados por `; `, máximo 3, ordenados
por relevancia.

Volumen de referencia: **1.028 llamadas** para la ronda completa (8 preguntas abiertas).
`run.R` arranca en modo prueba con una sola pregunta y 20 filas — ver `SOLO` y `N_FILAS`.

## Historia del repo y código de referencia

Este repo arrancó como el del panel de FOCUS y desde el commit de reinicio quedó
dedicado solo a este proyecto. **Todo el código anterior sigue en la historia de git**
(commit `66aa5f2`, 8.529 archivos), además del respaldo local aparte.

Para la parte de análisis, lo que más conviene mirar de ahí:

| Qué | Dónde en `66aa5f2` |
|---|---|
| Análisis y gráficos por ronda | `code/analysis/<year>/<round_id>/` |
| Utilidades reusables (INSE, biplot CA, treemap de sentimiento) | `code/utils/` |
| Codificación de abiertas con LLM | `DriveFlow/qualcode.R`, `DriveFlow/run.R` |
| Base acumulada anual | `code/base_acumulada.qmd` |
| Conteo de encuestas por participante (pagos) | `code/n_encuestas.R` |

Recuperar un archivo puntual sin revertir nada:

```bash
git show 66aa5f2:code/utils/sentiment_treemap.R > code/utils/sentiment_treemap.R
git show 66aa5f2 --stat | head -40          # ver qué había
git ls-tree -r 66aa5f2 --name-only | grep utils   # listar por carpeta
```

Ojo al reusarlos: esos scripts asumen las columnas del panel de FOCUS
(`segmento`, `voto2`, `etiqueta`, `q1`…`q16`). Acá los demográficos vienen de
LimeSurvey y tienen otros nombres (`departamento`, `n_educativo`, `ideologia`, `voto`),
así que hay que remapear antes de correrlos.

## Reglas de conteo (editables en `PARAMS`, arriba de `code/analisis_panel.R`)

| Regla | Definición |
|---|---|
| Columnas de preguntas | cualquier columna que termine en `q<número>` |
| Respuesta en **audio** | el valor es un link (`https://…ogg`) |
| Respuesta en **texto** | cualquier otro valor no vacío |
| No cuenta como respuesta | vacío o placeholder (`Q1`, `TEXT`, `undefined`, `null`, `-`) |
| Caso de **prueba** (se excluye) | 5 o más placeholders en la misma fila |
| Preguntas del denominador | se ignoran las 100% vacías en toda la base |
| Identidad de la persona | últimos 8 dígitos del teléfono; ante varios envíos, el más completo |
