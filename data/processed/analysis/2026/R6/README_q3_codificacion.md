# Codificacion q3 - R6 2026

## Archivos listos

- `data/processed/transcriptions/output/2026/transcripcion_R6.csv`
- `data/processed/analysis/2026/R6/codigos_q3.csv`
- `data/processed/analysis/2026/R6/codigos_q3_postura.csv`
- `data/processed/analysis/2026/R6/codigos_q3_fundamento.csv`
- `code/analysis/2026/R6/codigos_q3.R`
- `data/processed/analysis/2026/R6/memo_q3_libro_codigos.md`

## Comando para correr

```bash
OPENAI_API_KEY=tu_api_key Rscript code/analysis/2026/R6/codigos_q3.R
```

## Salida esperada

El script genera:

- `data/processed/analysis/2026/R6/seguridad_interinstitucional_q3.csv`

Ese archivo replica la base de `transcripcion_R6.csv` y agrega estas columnas:

- `postura_q3`
- `fundamento_q3`
- `codigo_q3`

La clasificacion se hace en dos llamadas separadas al modelo:

- un chat para `postura_q3`
- otro chat para `fundamento_q3`

Despues el script deriva `codigo_q3` a partir de ambas capas.

## Nota

El entorno actual no tiene `OPENAI_API_KEY` cargada, por lo que el script no puede ejecutarse aca contra la API. La estructura, insumos y archivo de entrada ya quedaron prontos para que lo corras localmente.
