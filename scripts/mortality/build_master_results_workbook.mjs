import fs from "node:fs/promises";
import path from "node:path";
import {
  SpreadsheetFile,
  Workbook,
} from "/Users/jelsner/.cache/codex-runtimes/codex-primary-runtime/dependencies/node/node_modules/@oai/artifact-tool/dist/artifact_tool.mjs";

const freezeDir = path.resolve(
  "outputs/mortality/paper_master_34kt/frozen_v1_2026-11-17",
);
const outputPath = path.join(freezeDir, "master_results_34kt_frozen_v1.xlsx");
const previewDir = path.join(freezeDir, "workbook_previews");

const colors = {
  navy: "#17365D",
  blue: "#D9EAF7",
  paleBlue: "#EEF5FA",
  green: "#E2F0D9",
  darkGreen: "#375623",
  gold: "#FFF2CC",
  lightGray: "#F2F2F2",
  border: "#D9E1F2",
  white: "#FFFFFF",
  text: "#1F2937",
};
const fontFamily = "Arial";

function parseCsv(text) {
  const rows = [];
  let row = [];
  let field = "";
  let quoted = false;
  for (let i = 0; i < text.length; i += 1) {
    const char = text[i];
    if (quoted) {
      if (char === '"' && text[i + 1] === '"') {
        field += '"';
        i += 1;
      } else if (char === '"') {
        quoted = false;
      } else {
        field += char;
      }
    } else if (char === '"') {
      quoted = true;
    } else if (char === ",") {
      row.push(field);
      field = "";
    } else if (char === "\n") {
      row.push(field.replace(/\r$/, ""));
      rows.push(row);
      row = [];
      field = "";
    } else {
      field += char;
    }
  }
  if (field.length || row.length) {
    row.push(field.replace(/\r$/, ""));
    rows.push(row);
  }
  return rows;
}

function typedValue(value, header) {
  if (value === "") return null;
  if (value === "TRUE") return true;
  if (value === "FALSE") return false;
  if (header === "freeze_date") return new Date(`${value}T12:00:00Z`);
  if (
    header !== "source_commit" &&
    header !== "result_id" &&
    header !== "analysis_version" &&
    header !== "sha256" &&
    /^-?(?:\d+\.?\d*|\.\d+)(?:[eE][+-]?\d+)?$/.test(value)
  ) {
    return Number(value);
  }
  return value;
}

async function readCsv(fileName) {
  const raw = await fs.readFile(path.join(freezeDir, fileName), "utf8");
  const rows = parseCsv(raw);
  const headers = rows[0];
  return rows.map((row, rowIndex) =>
    row.map((value, columnIndex) =>
      rowIndex === 0 ? value : typedValue(value, headers[columnIndex]),
    ),
  );
}

function columnName(index) {
  let result = "";
  let value = index + 1;
  while (value > 0) {
    const remainder = (value - 1) % 26;
    result = String.fromCharCode(65 + remainder) + result;
    value = Math.floor((value - 1) / 26);
  }
  return result;
}

function styleDataSheet(sheet, rows, options = {}) {
  const rowCount = rows.length;
  const columnCount = rows[0].length;
  const lastColumn = columnName(columnCount - 1);
  const usedRange = sheet.getRange(`A1:${lastColumn}${rowCount}`);
  const headerRange = sheet.getRange(`A1:${lastColumn}1`);

  sheet.showGridLines = false;
  sheet.freezePanes.freezeRows(1);
  if (options.freezeColumns) {
    sheet.freezePanes.freezeColumns(options.freezeColumns);
  }
  usedRange.format.font = { name: fontFamily, size: 9, color: colors.text };
  usedRange.format.verticalAlignment = "center";
  headerRange.format = {
    fill: colors.navy,
    font: { name: fontFamily, size: 9, bold: true, color: colors.white },
    wrapText: true,
    verticalAlignment: "center",
    borders: { preset: "all", style: "thin", color: colors.border },
  };
  headerRange.format.rowHeight = 42;
  usedRange.format.borders = {
    insideHorizontal: { style: "thin", color: "#E7EDF5" },
  };

  rows[0].forEach((header, index) => {
    const letter = columnName(index);
    const dataRange = sheet.getRange(`${letter}2:${letter}${rowCount}`);
    let width = 13;
    if (["cause", "analysis_tier", "period", "age_group"].includes(header)) width = 24;
    if (["model_formula_summary", "cluster_summary", "reference_period", "description", "details", "value"].includes(header)) width = 42;
    if (["source_commit", "sha256", "path"].includes(header)) width = 44;
    if (["result_id", "analysis_version", "check_name", "item"].includes(header)) width = 28;
    if (header.includes("p_value") || header.includes("fdr_q")) {
      dataRange.format.numberFormat = "0.0000";
      width = Math.max(width, 15);
    } else if (
      header.includes("rate_ratio") ||
      header.includes("ci_lower") ||
      header.includes("ci_upper") ||
      header.includes("standard_error") ||
      header.includes("log_likelihood") ||
      header.includes("pseudo_r2") ||
      header.includes("correlation") ||
      header.includes("chisq") ||
      header.includes("z_value") ||
      header === "log_rate_ratio" ||
      header === "percent_change"
    ) {
      dataRange.format.numberFormat = "0.000";
      width = Math.max(width, 14);
    } else if (header === "freeze_date") {
      dataRange.format.numberFormat = "yyyy-mm-dd";
      width = 13;
    } else if (
      header.includes("observations") ||
      header.includes("deaths") ||
      header.includes("zip_days") ||
      header.includes("clusters") ||
      header.includes("rows") ||
      header.includes("count") ||
      header === "bytes"
    ) {
      dataRange.format.numberFormat = "#,##0";
      width = Math.max(width, 15);
    }
    sheet.getRange(`${letter}:${letter}`).format.columnWidth = width;
  });

  if (options.tableName) {
    const table = sheet.tables.add(`A1:${lastColumn}${rowCount}`, true, options.tableName);
    table.style = "TableStyleMedium2";
    table.showBandedRows = true;
    table.showFilterButton = true;
  }
}

function addDataSheet(workbook, name, rows, options = {}) {
  const sheet = workbook.worksheets.add(name);
  sheet.getRange("A1").write(rows);
  styleDataSheet(sheet, rows, options);
  sheet.tabColor = options.tabColor || colors.navy;
  return sheet;
}

const masterRows = await readCsv("master_results_34kt_frozen_v1.csv");
const interactionRows = await readCsv("age_interaction_contrasts_34kt_frozen_v1.csv");
const diagnosticRows = await readCsv("model_diagnostics_34kt_frozen_v1.csv");
const dictionaryRows = await readCsv("data_dictionary_34kt_frozen_v1.csv");
const qaRows = await readCsv("qa_checks_34kt_frozen_v1.csv");
const auditRows = await readCsv("analysis_audit_34kt_frozen_v1.csv");
const manifestRows = await readCsv("freeze_manifest_34kt_frozen_v1.csv");

const workbook = Workbook.create();

const summary = workbook.worksheets.add("Summary");
summary.showGridLines = false;
summary.tabColor = colors.navy;
summary.getRange("A1:H2").merge();
summary.getRange("A1").values = [["Florida Tropical Cyclone Mortality — Frozen Master Results"]];
summary.getRange("A1:H2").format = {
  fill: colors.navy,
  font: { name: fontFamily, size: 20, bold: true, color: colors.white },
  verticalAlignment: "center",
};
summary.getRange("A4:B12").values = [
  ["Analysis version", "frozen_v1_2026-11-17"],
  ["Freeze date", new Date("2026-11-17T12:00:00Z")],
  ["Exposure threshold", ">=34 kt"],
  ["Geography", "Florida ZIP codes"],
  ["Master estimates", 176],
  ["Cause categories", 11],
  ["Age groups", 4],
  ["Event-relative periods", 4],
  ["QA checks passed", "16 of 16"],
];
summary.getRange("D4:H12").values = [
  ["Analysis structure", null, null, null, null],
  ["Primary", "All diseases; all ages and three age strata", null, null, null],
  ["Prioritized secondary", "Cardiovascular; Respiratory; Injuries; Neuropsychiatric disorders", null, null, null],
  ["Exploratory", "Six remaining cause-specific categories", null, null, null],
  ["Model", "Fixed-effects Poisson", null, null, null],
  ["Fixed effects", "ZIP; ISO year-week; day-of-week; ZIP x COVID", null, null, null],
  ["Uncertainty", "Two-way clustered by ZIP and ISO year-week", null, null, null],
  ["Interaction tests", "Pairwise age contrasts plus 2-df global Wald test", null, null, null],
  ["Multiplicity", "Benjamini-Hochberg FDR families documented in Audit", null, null, null],
];
summary.getRange("A14:H16").merge();
summary.getRange("A14").values = [[
  "Frozen artifact: use the Master Results sheet as the canonical row-level source. " +
  "Interaction Tests contains covariance-aware age comparisons; Diagnostics, Dictionary, QA Checks, Audit, and Manifest document reproducibility.",
]];
summary.getRange("A4:A12").format = {
  fill: colors.paleBlue,
  font: { name: fontFamily, size: 10, bold: true, color: colors.navy },
  borders: { preset: "all", style: "thin", color: colors.border },
};
summary.getRange("B4:B12").format = {
  font: { name: fontFamily, size: 10, color: colors.text },
  borders: { preset: "all", style: "thin", color: colors.border },
};
summary.getRange("D4:H4").merge();
summary.getRange("D4:H4").format = {
  fill: colors.navy,
  font: { name: fontFamily, size: 11, bold: true, color: colors.white },
};
for (let row = 5; row <= 12; row += 1) {
  summary.getRange(`E${row}:H${row}`).merge();
}
summary.getRange("D5:D12").format = {
  fill: colors.paleBlue,
  font: { name: fontFamily, size: 10, bold: true, color: colors.navy },
  borders: { preset: "all", style: "thin", color: colors.border },
};
summary.getRange("E5:H12").format = {
  font: { name: fontFamily, size: 10, color: colors.text },
  wrapText: true,
  borders: { preset: "all", style: "thin", color: colors.border },
};
summary.getRange("A14:H16").format = {
  fill: colors.gold,
  font: { name: fontFamily, size: 10, italic: true, color: colors.text },
  wrapText: true,
  verticalAlignment: "center",
  borders: { preset: "outside", style: "thin", color: "#D6B656" },
};
summary.getRange("B5").format.numberFormat = "yyyy-mm-dd";
summary.getRange("A:H").format.columnWidth = 15;
summary.getRange("A:A").format.columnWidth = 24;
summary.getRange("B:B").format.columnWidth = 30;
summary.getRange("D:D").format.columnWidth = 24;
summary.getRange("E:H").format.columnWidth = 18;

addDataSheet(workbook, "Master Results", masterRows, {
  freezeColumns: 7,
  tableName: "MasterResultsTable",
  tabColor: "#4472C4",
});
addDataSheet(workbook, "Interaction Tests", interactionRows, {
  freezeColumns: 5,
  tableName: "InteractionTestsTable",
  tabColor: "#70AD47",
});
addDataSheet(workbook, "Diagnostics", diagnosticRows, {
  freezeColumns: 4,
  tableName: "DiagnosticsTable",
  tabColor: "#5B9BD5",
});
addDataSheet(workbook, "Dictionary", dictionaryRows, {
  freezeColumns: 2,
  tableName: "DictionaryTable",
  tabColor: "#A5A5A5",
});
const qaSheet = addDataSheet(workbook, "QA Checks", qaRows, {
  freezeColumns: 2,
  tableName: "QAChecksTable",
  tabColor: "#70AD47",
});
qaSheet.getRange(`B2:B${qaRows.length}`).conditionalFormats.add("cellIs", {
  operator: "equal",
  formula: '"PASS"',
  format: { fill: colors.green, font: { bold: true, color: colors.darkGreen } },
});
addDataSheet(workbook, "Audit", auditRows, {
  freezeColumns: 1,
  tableName: "AuditTable",
  tabColor: "#FFC000",
});
addDataSheet(workbook, "Manifest", manifestRows, {
  freezeColumns: 2,
  tableName: "ManifestTable",
  tabColor: "#A5A5A5",
});

workbook.recalculate();

const workbookInspection = await workbook.inspect({
  kind: "workbook,sheet,table",
  maxChars: 12000,
  tableMaxRows: 4,
  tableMaxCols: 8,
  tableMaxCellChars: 80,
});
console.log(workbookInspection.ndjson);

const errorInspection = await workbook.inspect({
  kind: "match",
  searchTerm: "#REF!|#DIV/0!|#VALUE!|#NAME\\?|#N/A",
  options: { useRegex: true, maxResults: 100 },
  summary: "final formula error scan",
  maxChars: 4000,
});
console.log(errorInspection.ndjson);

await fs.mkdir(previewDir, { recursive: true });
const sheetNames = [
  "Summary",
  "Master Results",
  "Interaction Tests",
  "Diagnostics",
  "Dictionary",
  "QA Checks",
  "Audit",
  "Manifest",
];
for (const sheetName of sheetNames) {
  const preview = await workbook.render({
    sheetName,
    autoCrop: "all",
    scale: 0.4,
    format: "png",
  });
  const previewName = `${sheetName.toLowerCase().replaceAll(" ", "_")}.png`;
  await fs.writeFile(
    path.join(previewDir, previewName),
    new Uint8Array(await preview.arrayBuffer()),
  );
}

const xlsx = await SpreadsheetFile.exportXlsx(workbook);
await xlsx.save(outputPath);
console.log(`WORKBOOK_PATH=${outputPath}`);
console.log(`PREVIEW_DIR=${previewDir}`);
