library(data.table)
library(fixest)

model_specs <- data.table(
  label = c(
    "Hurricanes >=33 m/s",
    "Storms >=28 m/s",
    "Storms >=17 m/s",
    "Age >=60 years",
    "Injury deaths"
  ),
  path = c(
    "models/m_event_64kt.rds",
    "models/m_event_55kt.rds",
    "models/m_event_34kt.rds",
    "models/m_event_64kt_age60.rds",
    "models/m_event_64kt_injury.rds"
  )
)

days <- c(-8, -7, -3, -1, 0, 1, 2)
day_labels <- c("-8", "-7", "-3", "-1", "0 (Date0)", "+1", "+2")

extract_daily_ci <- function(label, path) {
  model <- readRDS(path)
  coef_table <- coeftable(model)
  terms <- paste0("rel_day_f::", days)

  data.table(
    model = label,
    day = days,
    day_label = day_labels,
    term = terms,
    beta = coef_table[terms, "Estimate"],
    se = coef_table[terms, "Std. Error"],
    p = coef_table[terms, "Pr(>|z|)"]
  )[, `:=`(
    RR = exp(beta),
    LCI = exp(beta - 1.96 * se),
    UCI = exp(beta + 1.96 * se),
    RR_CI = sprintf("%.2f (%.2f-%.2f)",
                    exp(beta),
                    exp(beta - 1.96 * se),
                    exp(beta + 1.96 * se)),
    RR_CI_word = sprintf("%.2f (95%% CI: %.2f-%.2f)",
                         exp(beta),
                         exp(beta - 1.96 * se),
                         exp(beta + 1.96 * se))
  )]
}

raw <- rbindlist(Map(extract_daily_ci, model_specs$label, model_specs$path))
raw[, model := factor(model, levels = model_specs$label)]
raw[, day_label := factor(day_label, levels = day_labels)]

wide <- dcast(raw, model ~ day_label, value.var = "RR_CI")
setnames(wide, "model", "Model / threshold")

wide_word <- dcast(raw, model ~ day_label, value.var = "RR_CI_word")
setnames(wide_word, "model", "Model / threshold")

caption <- paste(
  "Table S1. Selected Daily Rate Ratios (RRs) for Key Models.",
  "RR = exp(beta); reference = non-storm days (None).",
  "Values are rate ratios with 95% confidence intervals."
)

notes <- data.table(
  item = c("Caption", "Inference", "Source models", "Cardiovascular model"),
  note = c(
    caption,
    "Confidence intervals use exp(beta +/- 1.96 * cluster-robust SE) from fixest::coeftable().",
    paste(model_specs$path, collapse = "; "),
    "Cardiovascular subset data were present, but Table S1 in the revised PDF did not include a cardiovascular row."
  )
)

dir.create("data/outputs/table_s1", recursive = TRUE, showWarnings = FALSE)
fwrite(raw, "data/outputs/table_s1/TableS1_RR_CI_raw.csv")
fwrite(wide, "data/outputs/table_s1/TableS1_RR_CI_word_ready.csv")
fwrite(wide_word, "data/outputs/table_s1/TableS1_RR_CI_word_ready_verbose.csv")
fwrite(notes, "data/outputs/table_s1/TableS1_notes.csv")
fwrite(wide, "data/outputs/table_s1/TableS1_RR_CI_paste_into_Word.tsv", sep = "\t")

html <- paste0(
  "<!doctype html><html><head><meta charset=\"utf-8\"><style>",
  "body{font-family:Arial,sans-serif;} table{border-collapse:collapse;font-size:11pt;} ",
  "caption{caption-side:top;text-align:left;font-weight:bold;margin-bottom:8px;} ",
  "th,td{border:1px solid #777;padding:5px 7px;vertical-align:top;} th{background:#eaeaea;} ",
  "td:not(:first-child),th:not(:first-child){text-align:center;white-space:nowrap;} ",
  "p.note{font-size:10pt;margin-top:8px;} </style></head><body><table><caption>",
  caption,
  "</caption><thead><tr>",
  paste(sprintf("<th>%s</th>", names(wide)), collapse = ""),
  "</tr></thead><tbody>",
  paste(apply(wide, 1, function(row) {
    paste0("<tr>", paste(sprintf("<td>%s</td>", row), collapse = ""), "</tr>")
  }), collapse = "\n"),
  "</tbody></table><p class=\"note\">Confidence intervals use exp(beta +/- 1.96 x cluster-robust SE) from fixest::coeftable().</p></body></html>"
)
writeLines(html, "data/outputs/table_s1/TableS1_RR_CI_for_Word.html")

print(wide)
