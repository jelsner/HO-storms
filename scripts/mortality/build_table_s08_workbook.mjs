#!/usr/bin/env node

import fs from "node:fs/promises";
import { createRequire } from "node:module";

const require = createRequire(import.meta.url);
const { SpreadsheetFile, Workbook } = require("@oai/artifact-tool");

const artifactDir =
  "outputs/mortality/environmental_research_figure_s05_table_s08_v1_2026-11-20";
const tableDir = `${artifactDir}/tables`;
const previewDir = `${artifactDir}/workbook_previews`;
const estimatesPath = `${tableDir}/table_s08_alternative_specification_estimates.csv`;
const summaryPath = `${tableDir}/table_s08_specification_summary.csv`;
const dictionaryPath = `${tableDir}/table_s08_data_dictionary.csv`;
const workbookPath = `${tableDir}/table_s08_alternative_specification_estimates.xlsx`;

await fs.mkdir(previewDir, { recursive: true });

async function csvValues(path, sheetName) {
  const text = await fs.readFile(path, "utf8");
  const imported = await Workbook.fromCSV(text, { sheetName });
  return imported.worksheets.getItem(sheetName).getUsedRange().values;
}

function normalizeMatrix(matrix, numericHeaders, booleanHeaders = new Set()) {
  const headers = matrix[0].map(String);
  return matrix.map((row, rowIndex) =>
    row.map((value, columnIndex) => {
      if (rowIndex === 0) return String(value);
      if (value === "" || value === null || value === undefined) return null;
      const header = headers[columnIndex];
      if (booleanHeaders.has(header)) {
        if (String(value).toUpperCase() === "TRUE") return true;
        if (String(value).toUpperCase() === "FALSE") return false;
      }
      if (numericHeaders.has(header)) {
        const numeric = Number(value);
        return Number.isFinite(numeric) ? numeric : null;
      }
      return String(value);
    })
  );
}

function columnName(index) {
  let value = index + 1;
  let name = "";
  while (value > 0) {
    const remainder = (value - 1) % 26;
    name = String.fromCharCode(65 + remainder) + name;
    value = Math.floor((value - 1) / 26);
  }
  return name;
}

const numericHeaders = new Set([
  "analysis_order",
  "specification_order",
  "cause_order",
  "age_order",
  "period_order",
  "alternative_log_rate_ratio",
  "alternative_standard_error",
  "alternative_p_value",
  "alternative_rate_ratio",
  "alternative_ci_lower",
  "alternative_ci_upper",
  "primary_log_rate_ratio",
  "primary_standard_error",
  "primary_p_value",
  "primary_rate_ratio",
  "primary_ci_lower",
  "primary_ci_upper",
  "delta_log_rate_ratio",
  "empirical_p_upper",
  "empirical_p_two_sided_centered",
  "empirical_p_upper_max_t",
  "global_empirical_p_max_t",
  "observations",
  "wind_threshold_kt",
]);
const booleanHeaders = new Set([
  "direction_agrees_with_primary",
  "interval_overlaps_primary",
  "primary_ci_excludes_null",
  "alternative_interval_excludes_null",
  "ci_exclusion_changed",
]);
const summaryNumericHeaders = new Set([
  "analysis_order",
  "specification_order",
  "rows",
  "causes",
  "age_groups",
  "periods",
  "direction_evaluable",
  "direction_agreement",
  "interval_overlap_evaluable",
  "interval_overlap",
  "ci_exclusion_changes",
]);

const estimateValues = normalizeMatrix(
  await csvValues(estimatesPath, "EstimatesImport"),
  numericHeaders,
  booleanHeaders
);
const summaryValues = normalizeMatrix(
  await csvValues(summaryPath, "SummaryImport"),
  summaryNumericHeaders
);
const dictionaryValues = await csvValues(dictionaryPath, "DictionaryImport");

const workbook = Workbook.create();
const summary = workbook.worksheets.add("Summary");
const estimates = workbook.worksheets.add("Estimates");
const dictionary = workbook.worksheets.add("Data dictionary");
const fontFamily = "Arial";

summary.showGridLines = false;
estimates.showGridLines = false;
dictionary.showGridLines = false;
summary.tabColor = "#1F4E78";
estimates.tabColor = "#5B9BD5";
dictionary.tabColor = "#A5A5A5";

summary.getRange("A2").values = [["Table S8. Alternative-specification estimates"]];
summary.getRange("A3").values = [[
  "Sensitivity analyses for 34-kt tropical-cyclone exposure and ZIP-level mortality"
]];
summary.getRange("A2:M2").format.borders = {
  bottom: { style: "thin", color: "#1F4E78" },
};
summary.getRange("A2").format.font = {
  name: fontFamily,
  size: 14,
  bold: true,
  color: "#1F1F1F",
};
summary.getRange("A3").format.font = {
  name: fontFamily,
  size: 10,
  italic: true,
  color: "#595959",
};

const compactSummary = [
  [
    "Analysis",
    "Specification",
    "Rows",
    "Causes",
    "Age groups",
    "Periods",
    "Direction agreement",
    "CI overlap",
    "CI-exclusion changes",
  ],
];
for (const row of summaryValues.slice(1)) {
  const directionEvaluable = Number(row[8] ?? 0);
  const intervalEvaluable = Number(row[10] ?? 0);
  compactSummary.push([
    row[1],
    row[3],
    Number(row[4]),
    Number(row[5]),
    Number(row[6]),
    Number(row[7]),
    directionEvaluable > 0 ? Number(row[9]) / directionEvaluable : null,
    intervalEvaluable > 0 ? Number(row[11]) / intervalEvaluable : null,
    Number(row[12]),
  ]);
}
summary.getRange("A6").write(compactSummary);
const summaryLastRow = 5 + compactSummary.length;
const summaryTable = summary.tables.add(
  `A6:I${summaryLastRow}`,
  true,
  "SensitivitySummaryTable"
);
summaryTable.style = "TableStyleMedium2";
summary.getRange(`A6:I${summaryLastRow}`).format.font = {
  name: fontFamily,
  size: 10,
};
summary.getRange("A6:I6").format = {
  fill: "#1F4E78",
  font: { name: fontFamily, size: 10, bold: true, color: "#FFFFFF" },
  horizontalAlignment: "center",
  verticalAlignment: "center",
  wrapText: true,
};
summary.getRange(`C7:F${summaryLastRow}`).format.numberFormat = "#,##0";
summary.getRange(`G7:H${summaryLastRow}`).format.numberFormat = "0.0%";
summary.getRange(`I7:I${summaryLastRow}`).format.numberFormat = "#,##0";
summary.getRange(`A7:B${summaryLastRow}`).format.horizontalAlignment = "left";
summary.getRange(`C7:I${summaryLastRow}`).format.horizontalAlignment = "right";
summary.getRange("A6:A14").format.columnWidth = 26;
summary.getRange("B6:B14").format.columnWidth = 39;
summary.getRange("C6:I14").format.columnWidth = 14;
summary.getRange("A6:I14").format.verticalAlignment = "center";
summary.getRange("A6:I14").format.rowHeight = 22;
summary.getRange("A6:I6").format.rowHeight = 34;

summary.getRange("A17").values = [["Notes"]];
summary.getRange("A17").format.font = {
  name: fontFamily,
  size: 10,
  bold: true,
  color: "#1F4E78",
};
summary.getRange("A18").values = [[
  "The workbook contains 452 alternative-specification rows. Primary comparators are repeated on each Estimates row."
]];
summary.getRange("A19").values = [[
  "Strict temporal permutation bounds are the 2.5th and 97.5th percentiles of the 199-calendar null distribution, not model confidence intervals."
]];
summary.getRange("A20").values = [[
  "No sensitivity-specific multiplicity adjustment was prespecified."
]];
summary.getRange("A18:A20").format.font = {
  name: fontFamily,
  size: 10,
  color: "#595959",
};
summary.getRange("A18:A20").format.columnWidth = 110;

const estimatesRows = estimateValues.length;
const estimatesCols = estimateValues[0].length;
estimates.getRange("A1").write(estimateValues);
const estimatesLastColumn = columnName(estimatesCols - 1);
const estimatesTable = estimates.tables.add(
  `A1:${estimatesLastColumn}${estimatesRows}`,
  true,
  "AlternativeSpecificationEstimates"
);
estimatesTable.style = "TableStyleMedium2";
estimates.getRange(`A1:${estimatesLastColumn}${estimatesRows}`).format.font = {
  name: fontFamily,
  size: 9,
};
estimates.getRange(`A1:${estimatesLastColumn}1`).format = {
  fill: "#1F4E78",
  font: { name: fontFamily, size: 9, bold: true, color: "#FFFFFF" },
  horizontalAlignment: "center",
  verticalAlignment: "center",
  wrapText: true,
};
estimates.getRange(`A1:${estimatesLastColumn}1`).format.rowHeight = 48;

const estimateHeaders = estimateValues[0];
for (let index = 0; index < estimateHeaders.length; index += 1) {
  const header = String(estimateHeaders[index]);
  const column = columnName(index);
  let width = 13;
  if (["analysis", "cause", "alternative_interval_type"].includes(header)) width = 26;
  if (header === "specification") width = 38;
  if (header === "age_group") width = 13;
  if (header === "period") width = 18;
  if (["analysis_version", "source_commit"].includes(header)) width = 31;
  if (header === "build_date") width = 12;
  if (header === "analysis_note") width = 55;
  if (header.endsWith("_order")) width = 10;
  estimates.getRange(`${column}1:${column}${estimatesRows}`).format.columnWidth = width;
  if (
    header.includes("rate_ratio") ||
    header.includes("standard_error") ||
    header.includes("ci_lower") ||
    header.includes("ci_upper") ||
    header === "delta_log_rate_ratio"
  ) {
    estimates.getRange(`${column}2:${column}${estimatesRows}`).format.numberFormat =
      "0.0000";
  }
  if (header.includes("p_value") || header.startsWith("empirical_p")) {
    estimates.getRange(`${column}2:${column}${estimatesRows}`).format.numberFormat =
      "0.0000";
  }
  if (header === "observations" || header.endsWith("_order") || header === "wind_threshold_kt") {
    estimates.getRange(`${column}2:${column}${estimatesRows}`).format.numberFormat =
      "#,##0";
  }
}
estimates.getRange(`A2:${estimatesLastColumn}${estimatesRows}`).format.verticalAlignment =
  "center";
estimates.getRange(`AM2:AM${estimatesRows}`).format.wrapText = true;
estimates.freezePanes.freezeRows(1);
estimates.freezePanes.freezeColumns(4);

dictionary.getRange("A2").values = [["Table S8 data dictionary"]];
dictionary.getRange("A2").format.font = {
  name: fontFamily,
  size: 14,
  bold: true,
  color: "#1F1F1F",
};
dictionary.getRange("A2:B2").format.borders = {
  bottom: { style: "thin", color: "#1F4E78" },
};
dictionary.getRange("A4").write(dictionaryValues);
const dictionaryLastRow = 3 + dictionaryValues.length;
const dictionaryTable = dictionary.tables.add(
  `A4:B${dictionaryLastRow}`,
  true,
  "TableS08DataDictionary"
);
dictionaryTable.style = "TableStyleMedium2";
dictionary.getRange(`A4:B${dictionaryLastRow}`).format.font = {
  name: fontFamily,
  size: 10,
};
dictionary.getRange("A4:B4").format = {
  fill: "#1F4E78",
  font: { name: fontFamily, size: 10, bold: true, color: "#FFFFFF" },
  horizontalAlignment: "center",
  verticalAlignment: "center",
};
dictionary.getRange(`A4:A${dictionaryLastRow}`).format.columnWidth = 42;
dictionary.getRange(`B4:B${dictionaryLastRow}`).format.columnWidth = 105;
dictionary.getRange(`A5:B${dictionaryLastRow}`).format.verticalAlignment = "top";
dictionary.getRange(`B5:B${dictionaryLastRow}`).format.wrapText = true;
dictionary.getRange(`A5:B${dictionaryLastRow}`).format.rowHeight = 30;
dictionary.freezePanes.freezeRows(4);

workbook.recalculate();

const summaryInspection = await workbook.inspect({
  kind: "table",
  range: "Summary!A2:I20",
  include: "values,formulas",
  tableMaxRows: 20,
  tableMaxCols: 12,
  maxChars: 6000,
});
const estimateInspection = await workbook.inspect({
  kind: "table",
  range: "Estimates!A1:M12",
  include: "values,formulas",
  tableMaxRows: 12,
  tableMaxCols: 13,
  maxChars: 6000,
});
const errorInspection = await workbook.inspect({
  kind: "match",
  searchTerm: "#REF!|#DIV/0!|#VALUE!|#NAME\\?|#N/A|#NUM!|#NULL!|#SPILL!|#CALC!",
  options: { useRegex: true, maxResults: 300 },
  summary: "final formula error scan",
  maxChars: 3000,
});

const summaryPreview = await workbook.render({
  sheetName: "Summary",
  range: "A1:I21",
  scale: 1.5,
  format: "png",
});
await fs.writeFile(
  `${previewDir}/table_s08_summary.png`,
  new Uint8Array(await summaryPreview.arrayBuffer())
);
const estimatesPreview = await workbook.render({
  sheetName: "Estimates",
  range: "A1:M18",
  scale: 1.2,
  format: "png",
});
await fs.writeFile(
  `${previewDir}/table_s08_estimates.png`,
  new Uint8Array(await estimatesPreview.arrayBuffer())
);
const dictionaryPreview = await workbook.render({
  sheetName: "Data dictionary",
  range: "A1:B20",
  scale: 1.2,
  format: "png",
});
await fs.writeFile(
  `${previewDir}/table_s08_data_dictionary.png`,
  new Uint8Array(await dictionaryPreview.arrayBuffer())
);

const output = await SpreadsheetFile.exportXlsx(workbook);
await output.save(workbookPath);

console.log(summaryInspection.ndjson);
console.log(estimateInspection.ndjson);
console.log(errorInspection.ndjson);
console.log(`Workbook written: ${workbookPath}`);
