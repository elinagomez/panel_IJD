# Panel de encuestas por WhatsApp - CISCO/ IJD, UDELAR

Este repositorio procesa las **rondas** de un panel de encuestas: un mismo grupo de
personas al que se vuelve a consultar periódicamente para poder comparar sus respuestas
a lo largo del tiempo, en vez de mirar una foto puntual.

Las consultas se envían por una plataforma de mensajería que admite respuestas **por
texto y por audio**, y combinan **preguntas cerradas** (escalas, opción múltiple) con
**preguntas abiertas** ("¿por qué?", "¿qué opina de…?"). Eso impone dos pasos que no
existen en una encuesta tradicional:

- **transcribir** los audios antes de poder analizar cualquier cosa;
- **codificar** las respuestas abiertas contra un codebook para poder tabularlas.

El resultado de cada ronda es una base limpia, con demográficos, transcripciones y
respuestas abiertas codificadas, comparable con las rondas anteriores.

## Flujo

```mermaid
flowchart TB
  classDef entrada fill:#FFF7ED,stroke:#FB923C,color:#7C2D12;
  classDef proceso fill:#ECFEFF,stroke:#06B6D4,color:#0E7490;
  classDef salida fill:#ECFCCB,stroke:#84CC16,color:#365314;
  classDef externo fill:#FDF2F8,stroke:#F472B6,color:#831843;
  classDef humano fill:#F5F3FF,stroke:#8B5CF6,color:#4C1D95;

  EXPORT["Export de la plataforma<br/>data/raw/campaigns_wcx"]:::entrada
  BASE["Encuesta de reclutamiento<br/>data/raw/limesurvey"]:::entrada

  CONT["contacts_lime.R<br/>arma la base de contactos"]:::proceso
  MATCH["matching.R<br/>cruza respuestas y demográficos"]:::proceso
  MATCHED["data/processed/matched"]:::salida

  TRANS["transcripciones.qmd<br/>baja audios, sube, reinserta texto"]:::proceso
  COLAB["Colab + Whisper<br/>whisper_colab.ipynb"]:::externo
  TOUT["data/processed/transcriptions<br/>ronda transcrita"]:::salida

  REP["reporte_panel.qmd<br/>cobertura y completitud"]:::proceso
  COD["DriveFlow/run.R<br/>codificación de abiertas"]:::proceso
  QA["Revisión del 20%<br/>y medición de acuerdo"]:::humano
  FIN["data/processed/analysis<br/>ronda codificada"]:::salida
  ACUM["base acumulada del año"]:::salida

  BASE --> CONT --> MATCH
  EXPORT --> MATCH --> MATCHED
  MATCHED --> TRANS
  TRANS <--> COLAB
  TRANS --> TOUT
  TOUT --> REP
  TOUT --> COD --> FIN
  FIN --> QA
  TOUT --> ACUM
```

## Pasos

### 1. Contactos y cruce

El export de la plataforma solo trae el teléfono como identificador. Para poder analizar
por segmento y seguir a la misma persona entre rondas, hay que cruzarlo con los
demográficos, que vienen de la encuesta de reclutamiento del panel.

| | |
|---|---|
| **Scripts** | `code/contacts_lime.R` → `code/matching.R` |
| **Entrada** | export de la ronda en `data/raw/campaigns_wcx/`, encuesta de base en `data/raw/limesurvey/` |
| **Salida** | `data/raw/contacts/<AAAAMMDD>.csv` y `data/processed/matched/<campaign>.csv` |

Criterios del cruce:

- la clave son los **últimos 8 dígitos del teléfono**, para no depender del formato del prefijo;
- se usa `left_join` desde la encuesta: quien todavía no está en la base de reclutamiento
  **no se descarta**, queda con `match_lime = 0`;
- una persona puede tener **varios envíos** en una misma ronda: se conserva el más completo;
- se conservan las respuestas parciales, con `n_resp` y `completa` como columnas.

### 2. Transcripción de los audios

Las respuestas de audio llegan como enlaces `.ogg` dentro de las celdas. El notebook de
Quarto detecta esas celdas, descarga los audios, los sube a Drive, espera a que el Colab
los transcriba y reemplaza cada enlace por su texto.

| | |
|---|---|
| **Scripts** | `code/transcripciones.qmd` + `code/whisper_colab.ipynb` |
| **Entrada** | `data/processed/matched/<campaign>.csv` |
| **Salida** | `data/processed/transcriptions/output/<año>/transcripcion_<ronda>.csv` y `.xlsx` |

```mermaid
flowchart LR
  classDef proceso fill:#ECFEFF,stroke:#06B6D4,color:#0E7490;
  classDef externo fill:#FDF2F8,stroke:#F472B6,color:#831843;

  R1["R: detecta enlaces .ogg<br/>y descarga los audios"]:::proceso
  DA["Drive · carpeta de audio<br/>A/&lt;ronda&gt;"]:::externo
  CL["Colab · Whisper<br/>un .txt por audio"]:::externo
  DB["Drive · carpeta de texto<br/>B/&lt;ronda&gt;"]:::externo
  R2["R: baja los .txt y<br/>reemplaza los enlaces"]:::proceso

  R1 --> DA --> CL --> DB --> R2
```

El circuito depende de una sola convención: **cada `.txt` conserva el nombre del `.ogg`**
(`q3_fila57.ogg` → `q3_fila57.txt`). De ese nombre sale la fila y la columna a la que
vuelve el texto, así que no hay que renombrar nada en el medio.

`transcripciones.qmd` se corre **bloque por bloque**, porque en el medio hay que ir a
ejecutar el Colab. Tiene un modo prueba (`PRUEBA` / `N_PRUEBA`) para validar el circuito
completo con unos pocos audios antes de procesar la ronda entera.

### 3. Cobertura de la ronda

Antes de analizar contenido conviene saber cuánta gente respondió, quién completó y en
qué formato. Es también el control de calidad del envío.

| | |
|---|---|
| **Scripts** | `code/analisis_panel.R` (funciones) y `code/reporte_panel.qmd` (reporte HTML) |
| **Entrada** | el export crudo de la ronda |
| **Salida** | tablas en `data/processed/analysis/<año>/<ronda>/` |

Da respuestas totales, tasa de respuesta y de completitud por persona, reparto entre
texto y audio, embudo de abandono pregunta a pregunta y evolución por día.

Reglas de conteo, editables en `PARAMS` arriba de `code/analisis_panel.R`:

| Regla | Definición |
|---|---|
| Columnas de preguntas | cualquier columna que termine en `q<número>` |
| Respuesta en **audio** | el valor es un enlace `https://….ogg` |
| Respuesta en **texto** | cualquier otro valor no vacío |
| No cuenta como respuesta | vacío o placeholder: `Q1`, `TEXT`, `undefined`, `null`, `-` |
| Caso de **prueba**, se excluye | 5 o más placeholders en la misma fila |
| Preguntas del denominador | se ignoran las que quedaron 100% vacías en la ronda |
| Identidad de la persona | últimos 8 dígitos del teléfono; ante varios envíos, el más completo |

### 4. Codificación de las preguntas abiertas

Una pregunta abierta puede tener cientos de respuestas distintas en texto libre. Para
poder graficarlas y compararlas entre rondas hay que reducirlas a un conjunto fijo de
categorías. Ese trabajo lo hace un modelo de lenguaje **local** (Ollama + Qwen3): las
respuestas textuales no salen de la máquina.

| | |
|---|---|
| **Scripts** | `DriveFlow/exportar_para_colab.R` → `code/codificacion_colab.ipynb` → `DriveFlow/importar_de_colab.R` |
| **Entrada** | la ronda transcrita, la hoja de preguntas y el libro de códigos |
| **Salida** | `<ronda>_codificada.csv` y `QA_<ronda>_sample20.csv` en `data/processed/analysis/<año>/<ronda>/` |

El modelo corre en Colab con GPU, por el mismo esquema de ida y vuelta que la
transcripción: R deja el trabajo en una carpeta de Drive, el notebook lo procesa y
devuelve el resultado a la misma carpeta. **Toda la lógica queda en R** — codebook,
dependencias, prompts, validaciones — y el notebook solo ejecuta el modelo, así que no
hay reglas duplicadas en dos lenguajes.

```mermaid
flowchart LR
  classDef proceso fill:#ECFEFF,stroke:#06B6D4,color:#0E7490;
  classDef externo fill:#FDF2F8,stroke:#F472B6,color:#831843;

  R1["R: arma los prompts<br/>y una fila por respuesta"]:::proceso
  D1["Drive · C/&lt;ronda&gt;<br/>tareas + prompts"]:::externo
  CL["Colab · Qwen3 con GPU<br/>salida restringida al codebook"]:::externo
  D2["Drive · C/&lt;ronda&gt;<br/>codigos"]:::externo
  R2["R: pega los códigos<br/>y arma la muestra del 20%"]:::proceso

  R1 --> D1 --> CL --> D2 --> R2
```

Quien prefiera correrlo en su propia máquina puede usar `DriveFlow/run.R`, que hace lo
mismo contra un [Ollama](https://ollama.com) local. Es el mismo motor y el mismo modelo,
así que los resultados son comparables; la única diferencia es dónde corre.

Los dos insumos humanos viven en la carpeta de Drive del equipo, para que puedan
editarlos sin pasar por GitHub:

- **hoja de preguntas** — una fila por pregunta del proyecto: id, texto, tipo (abierta o
  cerrada), tema, opciones de las cerradas y de qué cerrada depende cada abierta.
- **`LibroCodigos`** — un único libro con **una hoja por ronda** (`R1`, `R2`, …). Cada
  hoja lleva `pregunta`, `etiqueta` y `descripcion`. Para una ronda nueva se duplica la
  hoja anterior y se la renombra: no hace falta crear archivos nuevos.

Los nombres de columna se reconocen ignorando mayúsculas, guiones y tildes, y los ids de
pregunta también (`Q1` y `q1` son lo mismo), así que el equipo puede escribirlos como le
resulte natural.

En cada corrida se guarda una **copia congelada** de los dos insumos junto a los
resultados, en `insumos/`. Sin eso no hay forma de reconstruir con qué versión del
codebook se codificó una ronda ya publicada.

El reparto entre lo humano y lo automático:

```mermaid
flowchart TB
  classDef humano fill:#F5F3FF,stroke:#8B5CF6,color:#4C1D95;
  classDef proceso fill:#ECFEFF,stroke:#06B6D4,color:#0E7490;
  classDef salida fill:#ECFCCB,stroke:#84CC16,color:#365314;

  subgraph H1["Humano · define el universo de categorías"]
    direction LR
    QS["questions<br/>qué pregunta es abierta<br/>y de qué cerrada depende"]:::humano
    CB["codebook de la ronda<br/>etiqueta + descripción"]:::humano
  end

  subgraph AUT["Automático · qualcode.R"]
    direction LR
    CFG["make_cfg<br/>una config por pregunta"]:::proceso
    PR["make_prompt<br/>codebook + reglas + contexto"]:::proceso
    CL["chat_structured<br/>salida restringida al codebook"]:::proceso
  end

  OUT["ronda codificada<br/>una columna codigo_qN"]:::salida
  SA["muestra del 20%<br/>con columna para codificar a mano"]:::salida
  H2["Humano · revisa la muestra<br/>medir_acuerdo decide si el codebook sirve"]:::humano

  QS --> CFG
  CB --> CFG
  CFG --> PR --> CL
  CL --> OUT
  CL --> SA --> H2
  H2 -. corrige el codebook .-> CB
```

**El modelo no puede inventar categorías.** La salida está restringida por un esquema a
las etiquetas del codebook, así que en el peor caso elige mal entre las que definiste.
Todo el trabajo conceptual está en los dos insumos humanos, y el codebook se escribe
leyendo respuestas reales: por eso este paso va después de transcribir, nunca antes.

Cómo trata cada pregunta:

- **preguntas con dependencia**: si una abierta depende de una cerrada previa, la opción
  elegida entra en el prompt como contexto para interpretar respuestas breves o
  elípticas — pero lo que se clasifica es la abierta. Las respuestas se agrupan por esa
  opción, de modo que se arma un prompt por opción y no uno por fila;
- **hasta 3 códigos** por respuesta, ordenados de mayor a menor relevancia y separados
  por `; `;
- **cada codebook necesita una categoría de no clasificable**, para las respuestas
  evasivas o tautológicas. Sin ella el modelo fuerza una categoría sustantiva que la
  persona nunca dio.

Convenciones de la salida, que conviene no confundir:

| Valor | Significa |
|---|---|
| `NA` | la persona no respondió esa pregunta |
| `"ERROR"` | el modelo falló en esa fila |
| `cod_a; cod_b` | uno o más códigos asignados |

Control de calidad: la corrida deja una muestra aleatoria del 20% con una columna
`codigo_<q>_rev` vacía. Se completa a mano y `medir_acuerdo()` devuelve, por pregunta,
el porcentaje de coincidencia exacta, el porcentaje con al menos un código en común y
el Jaccard promedio. Ese mismo archivo sirve para comparar tamaños de modelo con datos
en vez de a ojo.

### Paso a paso de una codificación

**1. Exportar el trabajo a Drive**, desde R en la raíz del proyecto:

```bash
Rscript DriveFlow/exportar_para_colab.R
```

Lee la ronda transcrita, la hoja de preguntas y el libro de códigos; valida los insumos y
corta si hay algo mal; guarda una copia congelada de ambos en `insumos/`; y sube a
`Análisis/C/<ronda>` dos archivos: `prompts_<ronda>.csv` (un prompt por pregunta, o uno
por opción en las preguntas con dependencia) y `tareas_<ronda>.csv` (una fila por
respuesta a clasificar). Para probar con poco, arriba del script están `SOLO` y `N_FILAS`.

**2. Correr el notebook en Colab.** Abrir `codificacion_colab.ipynb` desde la carpeta
Análisis, poner Entorno de ejecución → GPU (T4), y ejecutar las celdas en orden. Instala
Ollama en la máquina de Colab, descarga el modelo, encuentra la carpeta de la ronda y
codifica, escribiendo `codigos_<ronda>.csv` cada 25 respuestas. Si se corta la sesión, se
vuelve a correr la celda 5 y sigue con lo que falta; los `ERROR` se reintentan.

**3. Importar el resultado**, de vuelta en R:

```bash
Rscript DriveFlow/importar_de_colab.R
```

Baja los códigos, los pega a la base de la ronda en columnas `codigo_<q>`, avisa si
alguna etiqueta quedó fuera del codebook, imprime la distribución de cada pregunta y deja
la muestra del 20% para revisar.

**4. Revisar y medir.** Completar a mano la columna `codigo_<q>_rev` de la muestra y
correr `medir_acuerdo()` sobre ese archivo.

La alternativa local es `Rscript DriveFlow/run.R`, que hace los tres pasos de una contra
Ollama en la propia máquina. Arranca en modo prueba (`SOLO`, `N_FILAS`) y antes de
codificar corre `diagnostico_modelo()`, que chequea versiones de paquetes y que el
servidor responda.

## Estructura

```
project.yml                     configuración del proyecto y de la ronda vigente
DriveFlow/<ronda>.yml           configuración de codificación de esa ronda
code/
  contacts_lime.R               encuesta de reclutamiento -> base de contactos
  matching.R                    export + contactos -> base cruzada
  transcripciones.qmd           audios -> Drive -> Colab -> texto
  whisper_colab.ipynb           notebook de transcripción, se sube a Colab
  codificacion_colab.ipynb      notebook de codificación, se sube a Colab
  analisis_panel.R              métricas de cobertura y completitud
  reporte_panel.qmd             reporte HTML de la ronda
DriveFlow/
  qualcode.R                    funciones de codificación con LLM
  exportar_para_colab.R         arma prompts y tareas, y los sube a Drive
  importar_de_colab.R           baja los códigos y los pega a la ronda
  run.R                         alternativa: codifica en la propia máquina
data/
  raw/limesurvey/               encuesta de reclutamiento y diccionario
  raw/campaigns_wcx/            export crudo de la plataforma, por ronda
  raw/contacts/                 contactos con demográficos, por fecha de corte
  raw/codebook/                 copia local de la hoja de preguntas y del libro de códigos
  raw/transcriptions/<año>/<ronda>/   voice/ (.ogg) y text/ (.txt)
  raw/acumulada/rounds/<año>/    cada ronda ya transcrita
  processed/matched/            base cruzada, lista para transcribir
  processed/transcriptions/output/<año>/   ronda transcrita
  processed/analysis/<año>/<ronda>/       tablas, ronda codificada y QA
plots/<año>/<ronda>/
```

Los audios `.ogg` no se versionan: pesan y se vuelven a descargar de los enlaces del
export. Los `.txt` de las transcripciones sí, porque son insumo del análisis y no se
regeneran sin GPU.

## Configuración

Dos niveles, para no tocar los scripts al cambiar de ronda o de proyecto:

**`project.yml`** — lo que vale para todo el proyecto: la cuenta y carpetas de Drive, los
defaults de codificación, las reglas de lectura del export, y el bloque `ronda:` con la
ronda vigente. Cambiar `round_id`, `year` y `campaign` reacomoda todas las rutas.

```yaml
ronda:
  round_id: "R<n>"
  year: <año>
  campaign: "r<n>_<AAAAMMDD>"

drive:
  account_email: ""              # cuenta de drive_auth() y del Colab
  folder_analisis_id: ""         # carpeta de trabajo del equipo
  folder_audio_id: ""            # subcarpeta de audios .ogg
  folder_text_id: ""             # subcarpeta de .txt transcritos
  questions_sheet: ""            # URL de la hoja de preguntas

codificacion:
  provider: "ollama"             # local; las respuestas no salen de la máquina
  model: "qwen3:8b"
  temperature: 0                 # determinista
  seed: 1234                     # ronda reproducible
  think: false                   # Qwen3 razona por defecto; para clasificar no aporta
  max_active: 4                  # llamadas en paralelo
  insumos: "drive"               # o "local" para probar sin conexión
  codebook_name: "LibroCodigos"  # libro único; la hoja se llama igual que la ronda
```

Las carpetas de Drive se identifican por **ID** y no por ruta de nombres: si la carpeta
del equipo es compartida y no cuelga de "Mi unidad", las rutas por nombre no resuelven.
El ID es lo que aparece en la URL de la carpeta, después de `/folders/`. Las subcarpetas
de cada ronda se crean solas.

**`DriveFlow/<ronda>.yml`** — lo que cambia ronda a ronda: el id, el año y los nombres de
salida. Cualquier campo de `codificacion` se puede pisar acá para una ronda puntual. Usar
`_template.yml` como base.

## Ronda nueva: checklist

1. Guardar el export en `data/raw/campaigns_wcx/r<n>_<AAAAMMDD>_casos.csv`.
2. Actualizar el bloque `ronda:` de `project.yml`.
3. `Rscript code/matching.R`.
4. Correr `code/transcripciones.qmd` por bloques, con el Colab en el medio. Validar
   primero con `PRUEBA <- TRUE`.
5. `quarto render code/reporte_panel.qmd` para ver la cobertura.
6. Cargar las preguntas de la ronda en la hoja de preguntas, leer una muestra de
   respuestas abiertas y escribir el codebook en una hoja nueva de `LibroCodigos`,
   nombrada igual que la ronda.
7. Copiar `DriveFlow/_template.yml` a `DriveFlow/<ronda>.yml` y codificar:
   `exportar_para_colab.R` → notebook en Colab → `importar_de_colab.R`.
8. Revisar a mano la muestra del 20% y correr `medir_acuerdo()`.

## Requisitos

- **R** con `tidyverse`, `readr`, `readxl`, `writexl`, `yaml`, `glue`, `ellmer`,
  `googledrive`, `googlesheets4`, `ggplot2`, `knitr`.
- **Quarto**, para los reportes y los notebooks `.qmd`.
- **Cuenta de Google** autenticada con acceso a las carpetas de `project.yml`.
- **Colab con GPU** para la transcripción y la codificación.
- **Ollama** solo si se codifica en la propia máquina: `ollama serve` con el modelo
  descargado. Requiere `curl` >= 6.3.0 en R, y conviene una máquina con GPU dedicada.
