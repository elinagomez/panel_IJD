import { FileBlob, SpreadsheetFile } from "@oai/artifact-tool";
import path from "node:path";
import { fileURLToPath } from "node:url";

const scriptDir = path.dirname(fileURLToPath(import.meta.url));
const repoRoot = path.resolve(scriptDir, "../../../..");
const workbookPath = path.join(repoRoot, "data/processed/analysis/2026/R11/R11_codificada.xlsx");
const input = await FileBlob.load(workbookPath);
const workbook = await SpreadsheetFile.importXlsx(input);

const sample = await workbook.inspect({
  kind: "table",
  range: "resultados!A1:AE12",
  include: "values,formulas",
  tableMaxRows: 12,
  tableMaxCols: 31,
});
console.log(sample.ndjson);

const errors = await workbook.inspect({
  kind: "match",
  searchTerm: "#REF!|#DIV/0!|#VALUE!|#NAME\\?|#N/A",
  options: { useRegex: true, maxResults: 300 },
  summary: "final formula error scan",
});
console.log(errors.ndjson);

await workbook.render({ sheetName: "resultados", range: "A1:AE25", scale: 1 });
console.log("rendered=resultados!A1:AE25");
