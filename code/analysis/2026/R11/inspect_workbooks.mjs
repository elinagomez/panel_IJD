import { FileBlob, SpreadsheetFile } from "@oai/artifact-tool";

const files = [
  "/Users/simonherrera/Downloads/clasificacion_R11_2026.xlsx",
  "/Users/simonherrera/Downloads/R11 Base y análisis.xlsx",
];

for (const file of files) {
  console.log(`FILE\t${file}`);
  const input = await FileBlob.load(file);
  const workbook = await SpreadsheetFile.importXlsx(input);
  const sheets = workbook.worksheets.items ?? workbook.worksheets;
  console.log("WORKBOOK_KEYS", Object.keys(workbook).join(","));
  console.log("WORKSHEETS_KEYS", Object.keys(workbook.worksheets).join(","));
  for (const sheet of sheets) {
    console.log(`SHEET\t${sheet.name}\t${sheet.id ?? ""}`);
    const sample = await workbook.inspect({
      kind: "table",
      range: `${sheet.name}!A1:Z40`,
      include: "values,formulas",
      tableMaxRows: 40,
      tableMaxCols: 26,
    });
    console.log(sample.ndjson);
  }
}
