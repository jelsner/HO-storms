# First-trimester landmark analysis of PTB and vPTB

## Design

Pregnancies were classified as exposed when at least one corrected,
residence-specific 34-kt local impact date occurred during trimester 1
(`acute_direct_T1_34 = 1`). The landmark was the actual start date of
trimester 2, which was gestational day 91 for every retained pregnancy.
Only pregnancies still ongoing at the landmark were included.

Gestational age in days was the Cox analysis time. For PTB, follow-up ended at
delivery or 37 completed weeks; delivery before 37 weeks was the event. For
vPTB, follow-up ended at delivery or 32 completed weeks; delivery before 32
weeks was the event.

Models included maternal and paternal race, ethnicity, and education;
maternal age; prepregnancy BMI category; previous preterm birth; WIC; and
payment source. County and conception year-month were controlled through
joint Cox strata, and robust standard errors were clustered by county. The
analysis used complete covariate cases and singleton births.

## Cohort

- Source birth records: 4,662,195
- Complete-case singleton pregnancies ongoing at the landmark: 2,988,783
- Exposed during trimester 1: 440,197 (14.7%)
- Unexposed during trimester 1: 2,548,586 (85.3%)
- Counties: 67
- County/conception-month strata: 15,317

## Results

| Outcome | Events | Adjusted HR (95% CI) | P value |
|---|---:|---:|---:|
| PTB | 235,163 | 1.005 (0.980–1.030) | 0.717 |
| vPTB | 34,032 | 0.998 (0.924–1.077) | 0.956 |

For PTB, 35,402 events occurred among 440,197 exposed pregnancies (8.04%) and
199,761 among 2,548,586 unexposed pregnancies (7.84%). After adjustment, the
estimated hazard ratio was 1.005, indicating essentially no difference in the
subsequent hazard of delivery before 37 weeks.

For vPTB, 4,971 events occurred among exposed pregnancies (1.13%) and 29,061
among unexposed pregnancies (1.14%). The adjusted hazard ratio was 0.998,
again indicating essentially no difference in the subsequent hazard of
delivery before 32 weeks.

## Interpretation

There was no evidence that corrected 34-kt exposure during trimester 1 was
associated with the subsequent hazard of PTB or vPTB. Both point estimates
were nearly exactly 1.00, both confidence intervals included 1.00, and neither
test approached conventional statistical significance. These results agree
with the broader corrected analyses in finding no consistent adverse
association between tropical-cyclone exposure and preterm delivery.

The estimand is conditional on a pregnancy remaining ongoing at the end of
trimester 1 and subsequently appearing in the live-birth records. The analysis
cannot evaluate miscarriage, fetal death absent from the birth file, or other
early pregnancy losses. Exposure is also based on residence at delivery rather
than confirmed maternal location during the storm.

