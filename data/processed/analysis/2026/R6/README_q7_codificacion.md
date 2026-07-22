# Codificacion q7 - R6 2026

## Archivos listos

- `data/processed/transcriptions/output/2026/transcripcion_R6.csv`
- `data/processed/analysis/2026/R6/codigos_q7.csv`
- `data/processed/analysis/2026/R6/codigos_q7_postura.csv`
- `data/processed/analysis/2026/R6/codigos_q7_beneficio.csv`
- `data/processed/analysis/2026/R6/codigos_q7_riesgo.csv`
- `code/analysis/2026/R6/codigos_q7.R`
- `data/processed/analysis/2026/R6/memo_q7_libro_codigos.md`

## Comando para correr

```bash
OPENAI_API_KEY=tu_api_key Rscript code/analysis/2026/R6/codigos_q7.R
```

## Salida esperada

El script genera:

- `data/processed/analysis/2026/R6/tecnologia_seguridad_q7.csv`

Ese archivo replica la base de `transcripcion_R6.csv` y agrega estas columnas:

- `postura_q7`
- `beneficio_q7`
- `riesgo_q7`
- `codigo_q7`

La clasificacion se hace en tres llamadas separadas al modelo:

- un chat para `postura_q7`
- un chat para `beneficio_q7`
- otro chat para `riesgo_q7`

Despues el script deriva `codigo_q7` a partir de esas tres capas.

## Nota

El entorno actual no tiene `OPENAI_API_KEY` cargada, por lo que el script no puede ejecutarse aca contra la API. La estructura, insumos y archivo de entrada ya quedaron prontos para que lo corras localmente.
