# Locked Analysis and Reporting Plan

## Tropical-cyclone mortality relative to ZIP-specific storm impact in Florida

**Target journal:** *Environmental Research*, Environmental Epidemiology and Human Health section  
**Document version:** 1.0  
**Lock date:** 2026-11-17  
**Status:** Locked after PI approval of the three central research questions; canonical tracked protocol  
**Primary results freeze:** `frozen_v1_2026-11-17`  
**Primary wind threshold:** sustained winds greater than or equal to 34 kt  
**Study period:** 1985-2022  

This document fixes the inferential hierarchy, estimands, multiplicity rules,
spatial-selection rule, sensitivity analyses, and publication inventory for the
new cause-, age-, and phase-specific mortality paper. It is intentionally more
specific than a manuscript outline. Decisions below are not to be changed in
response to the direction or statistical significance of subsequent results.

The target journal describes an Environmental Epidemiology and Human Health
section, but cautions against work presented only as a region-specific case
study. The manuscript will therefore present Florida as a multi-decade,
multi-storm setting for estimating a general climate-health phenomenon: when
tropical-cyclone mortality begins, which causes contribute, whether older
adults experience stronger associations, and whether older-adult effects are
spatially concentrated.

## 1. Central research questions

The following wording is locked and will be used consistently in the abstract,
Introduction, Methods, Results, Discussion, figures, tables, and cover letter.

1. **When does mortality increase relative to the ZIP-specific storm-impact
   date?**
2. **Which causes of death account for these increases, and are associations
   stronger among older adults?**
3. **Are the strongest older-adult associations spatially concentrated, or are
   they geographically diffuse?**

## 2. Study design and analytic population

- Unit of analysis: ZIP-day.
- Geography: Florida ZIP codes represented in the mortality panel.
- Observation period: 1985-2022.
- Seasonal restriction: May-November, plus dates required to complete the
  event-relative windows through day +13.
- Exposure: modeled ZIP-specific tropical-cyclone winds greater than or equal
  to 34 kt.
- Impact date: the ZIP-specific date assigned as relative day 0.
- Overlapping event windows: a ZIP-date is assigned to the closest impact date;
  an exact-distance tie is assigned to the pre-impact event.
- Reference observations: all retained ZIP-days not assigned to one of the four
  event-relative periods.
- Outcomes: daily death counts by ZIP, underlying cause, and age group.
- Age range: valid recorded ages 0-120 years.
- Age groups: all ages, younger than 65, 65-74, and 75 or older. The three
  age-specific groups are mutually exclusive.

## 3. Event-relative periods

The four seven-day periods are fixed:

| Order | Label | Relative days | Interpretation |
|---:|---|---:|---|
| 1 | Days -14 to -8 | -14 through -8 | Early threat period |
| 2 | Days -7 to -1 | -7 through -1 | Imminent threat/preparation period |
| 3 | Days 0 to +6 | 0 through +6 | Impact and immediate aftermath |
| 4 | Days +7 to +13 | +7 through +13 | Early recovery period |

The paper will use these labels rather than calendar weeks. No analysis will
assign exposure using conventional Monday-Sunday weeks.

## 4. Cause-of-death categories

The analysis includes all diseases plus ten cause-specific categories:

1. Cardiovascular
2. Respiratory
3. Infectious diseases
4. Injuries
5. Neuropsychiatric disorders
6. Renal diseases
7. Digestive diseases
8. Diabetes
9. Endocrine, nutritional, and metabolic disorders
10. Neoplasms

ICD-9 and ICD-10 definitions are those implemented in
`scripts/mortality/cause_age_count_helpers.R` and recorded in Supplementary
Table S1. Cause-specific categories are not mutually exclusive across all
categories. In particular, diabetes is nested within the broader endocrine,
nutritional, and metabolic category. Results will not be summed across cause
categories.

## 5. Primary statistical model

For each outcome-age stratum, the primary model is a fixed-effects Poisson model
for daily death counts:

`deaths ~ event_period | ZIP + ISO year-week + day-of-week + ZIP x COVID-period`

where the COVID period is January 1, 2020 through December 31, 2022. Variance is
estimated with two-way cluster-robust standard errors by ZIP and ISO year-week.
The model is interpreted as a conditional comparison of mortality counts within
ZIPs and common calendar-time strata. Results are reported as rate ratios,
percent changes, 95% confidence intervals, unadjusted two-sided p-values, and
the prespecified false-discovery-rate q-values.

The manuscript will use association language. It will not describe estimates
as causal effects or deaths as caused by tropical cyclones.

## 6. Inferential hierarchy and estimands

### 6.1 Research question 1: timing of all-disease mortality

The estimands are the 16 all-disease rate ratios defined by four age groups and
four event-relative periods. This is the primary family.

- Primary reporting family: 16 tests.
- Multiplicity: Benjamini-Hochberg correction across the 16 tests.
- The all-ages estimates are the main population-level answer.
- Age-specific all-disease estimates connect research questions 1 and 2 and
  remain in the same locked primary family.
- The temporal pattern will be interpreted from effect sizes, confidence
  intervals, and coherence across adjacent periods, not from isolated p-values.

### 6.2 Research question 2: causes and older-adult heterogeneity

The ten cause-specific outcomes are divided into two families fixed before
manuscript preparation:

- **Prioritized secondary family (64 tests):** cardiovascular, respiratory,
  injuries, and neuropsychiatric disorders; four age groups by four periods.
- **Exploratory family (96 tests):** infectious, renal, digestive, diabetes,
  endocrine/nutritional/metabolic, and neoplasms; four age groups by four
  periods.

Multiplicity rules:

1. Benjamini-Hochberg correction within the 64 prioritized secondary tests.
2. Benjamini-Hochberg correction within the 96 exploratory tests.
3. A second, more conservative Benjamini-Hochberg correction across all 160
   cause-specific tests is reported for transparency and is the eligibility
   criterion for spatial analysis.

Age heterogeneity is assessed in a jointly fitted stacked-age Poisson model with
age-specific ZIP, ISO year-week, day-of-week, and ZIP-by-COVID fixed effects.
This model retains covariance among the three mutually exclusive age-specific
coefficients.

- Primary age-heterogeneity test: two-degree-of-freedom Wald test of equality
  among the under-65, 65-74, and 75-or-older coefficients within each
  cause-period combination.
- Multiplicity for cause-specific global age tests: Benjamini-Hochberg across
  40 tests (ten causes by four periods).
- Focused pairwise contrasts: 65-74 versus under 65 and 75 or older versus
  under 65, with Benjamini-Hochberg correction across 80 contrasts.
- The 75-or-older versus 65-74 contrast is reported in the complete supplement
  but is not part of the focused older-versus-younger family.

**Locked interpretation rule:** A positive or statistically detectable
association within an older age stratum is an older-adult association. It will
be described as *stronger among older adults* only when the global
age-heterogeneity test survives its prespecified FDR correction and the relevant
older-versus-under-65 contrast supports that direction. Differences between
point estimates alone will not be interpreted as effect modification.

### 6.3 Research question 3: spatial concentration

Spatial analysis is restricted to cause-age-period combinations satisfying all
of the following rules in the frozen 34-kt results:

1. Cause-specific outcome rather than all diseases.
2. Age group 65-74 or 75 or older.
3. Positive rate ratio.
4. The 95% confidence interval excludes 1.
5. The Benjamini-Hochberg q-value across all 160 cause-specific tests is less
   than 0.05.
6. At least 500 deaths occur statewide in the corresponding event-period cell.

Applying this rule to the frozen results produces exactly five spatial targets:

| Cause | Age group | Period | Frozen statewide RR | Event-period deaths |
|---|---|---|---:|---:|
| Cardiovascular | 65-74 | Days 0 to +6 | 1.113 | 2,305 |
| Cardiovascular | 75 or older | Days 0 to +6 | 1.053 | 8,779 |
| Respiratory | 75 or older | Days +7 to +13 | 1.086 | 2,041 |
| Neuropsychiatric disorders | 75 or older | Days 0 to +6 | 1.092 | 2,744 |
| Neoplasms | 75 or older | Days -7 to -1 | 1.073 | 4,174 |

This eligibility rule is fixed even if subsequent ZIP-level spatial statistics
are null. The five maps will be produced and reported as a complete set.

For each target:

1. Fit the same pooled cause-age-period Poisson model used for the statewide
   estimate.
2. Set the target event indicator to the reference level to obtain the daily
   counterfactual expected count.
3. Aggregate observed and expected deaths within ZIP.
4. Model the ZIP count as Poisson(expected deaths x ZIP rate ratio).
5. Stabilize ZIP rate ratios with the existing gamma-Poisson empirical-Bayes
   procedure; retain the posterior mean, 95% interval, posterior probability
   that the rate ratio exceeds 1, and reliability.
6. Calculate global and local Moran statistics on centered log EB-smoothed rate
   ratios using row-standardized queen contiguity.
7. Use 9,999 permutations and a fixed random seed recorded in the output.
8. Adjust the five global Moran p-values with Benjamini-Hochberg correction.
9. Adjust local Moran p-values across ZIPs within each map using
   Benjamini-Hochberg correction.
10. Classify FDR-supported local results as high-high, low-low, high-low, or
    low-high; retain islands and non-significant ZIPs explicitly.

Evidence of global spatial concentration requires a positive global Moran's I
with q < 0.05 across the five primary maps. If that criterion is not met, the
result will be described as no evidence of global spatial concentration, not as
proof of perfectly uniform or diffuse effects. Local clusters are descriptive
unless their within-map FDR q-value is less than 0.05.

## 7. Results-freeze and reproducibility rules

The canonical input for primary and cause/age results is:

`outputs/mortality/paper_master_34kt/frozen_v1_2026-11-17/master_results_34kt_frozen_v1.csv`

The primary freeze contains 176 unique cause-age-period rows and passed all 16
recorded QA checks. Publication scripts must read the frozen files rather than
reconstructing primary estimates from saved model objects or notebooks.

- Primary figures and tables will not silently refit models.
- Spatial results will receive a separate versioned freeze because they are not
  yet complete for all five targets.
- Sensitivity analyses will be stored separately and will never overwrite the
  primary freeze.
- Display rounding occurs only in publication artifacts; computations use
  full-precision values.
- Every artifact will include the analysis version and generating-script path
  in its metadata or caption notes.

## 8. Sensitivity and robustness analyses

The following analyses are required. They will be interpreted by consistency
of magnitude, direction, and uncertainty rather than by whether each p-value
crosses 0.05.

### 8.1 Exposure and event-time sensitivities

1. Repeat the four-period models at wind thresholds of 55 kt and 64 kt for all
   11 outcomes and four age groups.
2. Fit daily event-time models from day -14 through day +13 for all diseases and
   for the five frozen spatial targets; compare the daily pattern with the
   corresponding seven-day estimate.
3. Repeat the 34-kt models after excluding every ZIP-date involved in an
   overlapping event window before the closest-impact assignment.
4. Repeat the principal all-disease model using the established temporal and
   spatial placebo calendars from the earlier threat-days analysis.

### 8.2 Population and period sensitivities

5. Repeat the primary and prioritized secondary models with the annual ZIP
   population as a log offset using
   `data/mortality/population/florida_zip_population_1985_2022.rds`.
6. Repeat the primary and prioritized secondary models excluding 2020-2022.
7. Repeat all cause-specific models in the ICD-10 era (1999-2022).

### 8.3 Event-influence sensitivity

8. Perform leave-one-impact-date-out analyses for the 16 all-disease estimates
   and the five frozen older-adult spatial-target estimates. There are 77
   distinct impact dates in the 34-kt exposure calendar. Report the full range
   and identify any impact date whose removal reverses the direction of an
   estimate.

### 8.4 Spatial-weight sensitivity

9. Recalculate global Moran's I using rook contiguity and symmetric six-nearest-
   neighbor weights. Queen contiguity remains primary. Report whether the sign
   and inferential conclusion agree across the three weight definitions.

No additional sensitivity analysis will be promoted to the primary analysis
after results are inspected. Material reviewer-requested analyses will be
identified as post-plan additions.

## 9. Missingness, sparse data, and model diagnostics

- Deaths lacking a valid age are excluded from all age and all-age models so
  that the all-age count equals the sum of the three age strata.
- Cause-specific models exclude deaths lacking a usable underlying-cause code.
- ZIP-day cells with no deaths are retained as zeros.
- Fixed-effect strata removed by the Poisson estimator because they contain
  only zero outcomes or singletons are reported in model diagnostics.
- A cause-age-period estimate is not suppressed solely because its confidence
  interval is wide. Sparse estimates remain in the complete supplement.
- Spatial models must report ZIPs without an estimable counterfactual, polygon
  islands, observed and expected counts, EB reliability, and local-test
  multiplicity adjustment.

## 10. Reporting conventions

- Rate ratios are displayed to two decimal places in prose and generally two or
  three decimals in figures and tables.
- Confidence intervals are always displayed with the point estimate.
- P-values below 0.001 are displayed as `<0.001`; q-values are labeled `q`.
- The phrase `statistically significant` is reserved for the relevant
  prespecified FDR q < 0.05 criterion. Nominal p < 0.05 findings are described
  as nominal or unadjusted.
- Null interaction tests will be reported directly; they will not be replaced
  with comparisons of which age-specific confidence intervals include 1.
- Spatial color scales will be identical across the five EB maps when a common
  range is defensible; otherwise limits will be clearly labeled and a common
  secondary standardized scale will be supplied.
- Color will not be the only carrier of inferential meaning. LISA classes will
  use both color and categorical labels in legends.
- Main-text conclusions will distinguish association, heterogeneity, and
  spatial concentration as separate claims.

## 11. Exact main-text figure and table inventory

### Main figures

| ID | Working title | Research question | Locked content |
|---|---|---:|---|
| Figure 1 | Study design and event-relative periods | 1-3 | Florida exposure setting, ZIP-day workflow, four seven-day periods, reference days, outcomes, age groups, and model hierarchy. No effect estimates. |
| Figure 2 | Timing of all-disease mortality relative to ZIP-specific impact | 1 | Forest plot of all 16 all-disease estimates: four periods for all ages, under 65, 65-74, and 75 or older. Show RR, 95% CI, and primary-family q marker. |
| Figure 3 | Cause-specific mortality across event-relative periods | 2 | Forest plot of all-age estimates for all ten disease categories and all four periods. Distinguish prioritized and exploratory families and display the conservative 160-test q marker. |
| Figure 4 | Age-specific mortality for prioritized causes | 2 | Faceted forest plot for cardiovascular, respiratory, injuries, and neuropsychiatric disorders: three mutually exclusive age groups by four periods. Annotate the global age-heterogeneity q-value for each cause-period. |
| Figure 5 | EB-smoothed ZIP rate ratios for eligible older-adult associations | 3 | Five small-multiple choropleths, one for each locked spatial target, using EB-smoothed ZIP rate ratios and showing ZIPs without estimable results. |
| Figure 6 | Spatial clusters in eligible older-adult associations | 3 | Five corresponding local Moran cluster maps. Each panel reports global Moran's I and its five-map FDR q-value; local classes use within-map FDR. |

### Main tables

| ID | Working title | Research question | Locked content |
|---|---|---:|---|
| Table 1 | Study population, exposure, and mortality counts | 1-3 | Study years, ZIPs, distinct 34-kt impact dates, modeled ZIP-days, event ZIP-days, and deaths by cause and age; footnote category overlap and valid-age restriction. |
| Table 2 | All-disease mortality by age and event-relative period | 1 | Sixteen rows with event deaths, RR, 95% CI, percent change, p-value, and 16-test primary-family q-value. |
| Table 3 | Prioritized cause-specific mortality and age heterogeneity | 2 | Sixteen cause-period rows. Columns give all-age, under-65, 65-74, and 75-or-older RR (95% CI), conservative cause-specific q-value, global age-heterogeneity statistic, p-value, and q-value. |

Figures and tables will not duplicate every numeric field. Figures emphasize
patterns; tables provide exact principal estimates. Complete numerical results
remain in the supplement and machine-readable files.

## 12. Exact supplementary figure and table inventory

### Supplementary figures

| ID | Working title | Locked content |
|---|---|---|
| Figure S1 | Daily all-disease event-time profiles | Daily day -14 through +13 RRs for all ages and each age stratum, with the four seven-day estimates overlaid or shown alongside. |
| Figure S2 | Wind-threshold sensitivity | All-disease four-period estimates at 34, 55, and 64 kt, faceted by age group. |
| Figure S3 | Daily profiles for spatial-target associations | Daily day -14 through +13 estimates for the five cause-age combinations eligible for spatial analysis. |
| Figure S4 | Leave-one-impact-date-out influence | Distribution/range of jackknife estimates for the 16 all-disease results and five spatial targets; flag direction reversals. |
| Figure S5 | Alternative-specification forest plot | Primary and prioritized estimates under the main model, population offset, exclusion of 2020-2022, temporal and spatial placebo negative controls including 199 matched strict temporal permutations, ICD-10 era where applicable, and exclusion of overlapping windows. |
| Figure S6 | Age-specific results for exploratory causes | Age-specific four-period forest plots for infectious, renal, digestive, diabetes, endocrine/nutritional/metabolic, and neoplasm mortality. |
| Figure S7 | Older-versus-younger interaction contrasts | Ratios of rate ratios for 65-74 versus under 65 and 75 or older versus under 65 for all ten causes and four periods, with focused-family q markers. |
| Figure S8 | Spatial reliability and weight sensitivity | EB reliability maps for the five spatial targets plus a comparison of global Moran's I under queen, rook, and six-nearest-neighbor weights. |

### Supplementary tables and machine-readable supplements

| ID | Working title | Locked content |
|---|---|---|
| Table S1 | Cause-of-death definitions | ICD-9 and ICD-10 definitions, inclusions, nesting, and non-mutual-exclusivity notes for all ten categories. |
| Table S2 | Complete feasibility counts | Statewide and four-period ZIP-days, deaths, and nonzero ZIP-days for all 11 outcomes and four age groups. |
| Table S3 | Complete 34-kt model estimates | All 176 frozen cause-age-period estimates with raw p-values and both applicable FDR q-values. |
| Table S4 | Complete age-interaction tests | All 132 pairwise contrasts and 44 global age-heterogeneity tests with raw and adjusted values. |
| Table S5 | Primary and joint-model diagnostics | Observations, fixed-effect removals, clusters, convergence, likelihood, pseudo-R-squared, and coefficient reconciliation. |
| Table S6 | Wind-threshold sensitivity estimates | Complete 55- and 64-kt four-period estimates aligned with the 34-kt freeze. |
| Table S7 | Leave-one-impact-date-out estimates | Full jackknife results for the 16 all-disease estimates and five spatial targets. |
| Table S8 | Alternative-specification estimates | Population-offset, COVID-excluded, ICD-10-era, overlap-excluded, and placebo results with differences from the primary estimate. |
| Table S9 | Spatial model and global Moran results | For each of five targets: ZIPs estimated, observed and expected deaths, EB prior parameters, reliability summary, queen global Moran's I, p, five-map q, and rook/k-nearest-neighbor sensitivity. |
| Data S1 | ZIP-level spatial estimates | Machine-readable ZIP observed/expected counts, raw RR, EB RR and interval, posterior probability, reliability, neighbor count, local Moran's I, local p/q, and LISA class for all five targets. |

The STROBE checklist and RECORD extension will be prepared as separate
submission checklists rather than numbered supplementary tables.

## 13. Manuscript results order

The Results section will follow the research questions rather than the order in
which analyses were run:

1. Study population, exposure, and counts.
2. Timing of all-disease mortality.
3. Cause-specific mortality.
4. Age-specific associations and formal interaction tests.
5. Spatial concentration of the five eligible older-adult associations.
6. Sensitivity, falsification, and influence analyses.

## 14. Interpretation guardrails

The manuscript will not:

- equate a within-age association with statistically supported age
  heterogeneity;
- select spatial maps based on attractive patterns;
- describe nominal p < 0.05 results as surviving multiplicity correction;
- sum overlapping cause categories;
- infer individual mechanisms from ecological ZIP-day associations;
- claim that absence of global spatial autocorrelation proves geographic
  uniformity;
- imply that a Florida-only design directly estimates effects in other states;
  or
- overwrite or silently revise the frozen primary results.

The Discussion may propose plausible pathways such as anticipatory stress,
care disruption, evacuation, power dependency, exposure during impact, and
recovery conditions, but these will be labeled hypotheses rather than measured
mediators.

## 15. Amendment policy

After the lock date, any substantive change requires an amendment appended
below with:

1. date;
2. requested change;
3. reason;
4. whether results were known when requested;
5. affected research question, multiplicity family, figure, or table; and
6. approving investigators.

Editorial changes, corrections of clear programming errors, and additional
reviewer-requested sensitivity analyses do not alter the locked primary
estimands. They must still be documented in the amendment log.

### Amendment log

#### Amendment A01 — retained cause-of-death scope

1. **Date:** November 18, 2026.
2. **Requested change:** Remove digestive diseases, diabetes, and endocrine,
   nutritional, and metabolic disorders from the publication analysis and
   recompute all affected Benjamini-Hochberg families using the seven retained
   cause-specific outcomes.
3. **Reason:** The principal investigator narrowed the scientific scope of the
   paper and determined that these three outcomes did not need to be considered.
4. **Results-known status:** The principal investigator had not seen the results
   when making the request. The analyst had access to the frozen-v1 results.
   Frozen v1 is preserved unchanged, and this decision is implemented as
   `amended_v2_2026-11-18` rather than overwriting it.
5. **Affected analyses:** Research question 2; the exploratory family changes
   from 96 to 48 tests, the conservative cause-specific family from 160 to 112
   tests, global age-heterogeneity families from 44/40 to 32/28 tests, and
   pairwise age-contrast families from 132/120/80 to 96/84/56 tests. The
   16-test all-disease family and 64-test prioritized family are unchanged.
   Under the unchanged spatial-eligibility rule, the target set expands from
   five to eight: the original five targets plus cardiovascular mortality at
   ages 75+ during days +7 to +13, injury mortality at ages 75+ during days 0
   to +6, and neuropsychiatric mortality at ages 75+ during days +7 to +13.
6. **Approval:** Requested and approved by the principal investigator; recorded
   by the analyst.

#### Amendment A02 — event-influence scope

1. **Date:** November 19, 2026.
2. **Requested change:** Restrict the leave-one-impact-date-out sensitivity
   analysis to the 16 all-disease age-period estimands and omit the eight
   older-adult spatial targets from this analysis.
3. **Reason:** Focus the computationally intensive exact deletion analysis on
   the primary all-disease family. Spatial robustness remains addressed by the
   prespecified alternative-weight analysis.
4. **Results-known status:** No leave-one-impact-date-out results had been
   produced or inspected when the scope change was requested. An initial model
   fit had been interrupted before producing a checkpoint.
5. **Affected analyses:** Sensitivity analysis 8, Figure S4, and Table S7. The
   primary estimands, multiplicity families, and spatial analyses are unchanged.
6. **Approval:** Requested by the analyst and recorded in the project workflow
   before inspecting event-influence results.

#### Amendment A03 — COVID-period exclusion scope

1. **Date:** November 19, 2026.
2. **Requested change:** Restrict the sensitivity analysis excluding 2020-2022
   to the 16 primary all-disease age-period estimands rather than repeating the
   64 prioritized cause-specific estimands.
3. **Reason:** Focus this period sensitivity on the main results while limiting
   expansion of the secondary sensitivity analyses.
4. **Results-known status:** No COVID-excluded model results had been produced
   or inspected when the scope change was requested.
5. **Affected analyses:** Sensitivity analysis 6 and the corresponding portions
   of Figure S5 and Table S8. Primary estimands and multiplicity families are
   unchanged.
6. **Approval:** Requested by the analyst and recorded in the project workflow
   before fitting the COVID-excluded models.

