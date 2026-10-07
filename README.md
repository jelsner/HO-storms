# HO-storms

This repository contains two related but distinct Florida tropical-cyclone
health studies:

1. **Mortality** — daily mortality before, during, and after tropical-cyclone
   impacts. The manuscript is currently under review.
2. **Birth outcomes** — the newer pregnancy and preterm-birth analyses using
   corrected, residence-specific storm exposure and time-at-risk models.

Run scripts and render notebooks from the repository root so that relative
paths resolve consistently.

## Repository layout

```text
HO-storms/
├── analysis/
│   ├── mortality/       # Mortality R Markdown analyses
│   └── births/          # Birth analyses; historical code is under legacy/
├── apps/
│   └── mortality/       # Mortality Shiny applications and deployment files
├── data/                # Local data; ignored by Git
│   ├── mortality/
│   │   ├── raw/
│   │   ├── derived/
│   │   ├── population/
│   │   └── processed/
│   ├── births/
│   │   ├── raw/
│   │   ├── derived/
│   │   └── processed/
│   └── shared/
│       ├── storm/       # IBTrACS and warning/advisory inputs
│       └── geography/   # Boundaries used across studies
├── figs/
│   ├── mortality/
│   └── births/
├── literature/          # References relevant to either study
├── manuscript/
│   ├── mortality/       # Submitted manuscript and reviewer material
│   └── births/          # Birth-outcomes manuscript drafts
├── models/
│   ├── mortality/
│   └── births/
├── outputs/
│   ├── mortality/       # Tables, summaries, and spatial products
│   └── births/
└── scripts/
    ├── mortality/
    └── births/
```

## Main entry points

### Mortality study

- `analysis/mortality/ThreatDays.Rmd`: principal threat-day analysis.
- `analysis/mortality/All-Cause_Mortality.Rmd`: all-cause mortality analysis.
- `analysis/mortality/Figures.Rmd`: manuscript figures.
- `scripts/mortality/`: reproducible count, model, table, and spatial scripts.
- `manuscript/mortality/`: submitted paper and review documents.

### Birth-outcomes study

- `analysis/births/HO-storms-exposure.Rmd`: corrected storm-exposure build.
- `analysis/births/HO-storms-models.Rmd`: conventional trimester models.
- `analysis/births/HO-storms-time-to-event.Rmd`: gestational time-at-risk
  models.
- `analysis/births/legacy/`: original exploratory and collaborator notebooks;
  retained for provenance but not treated as the current workflow.
- `scripts/births/draft_figure1_gestational_timeline.R`: revised manuscript
  Figure 1.
- `models/births/conventional-window7/`: corrected symmetric seven-day model
  results.
- `models/births/time-to-event/`: time-to-event model results.
- `manuscript/births/`: current birth-outcomes manuscript drafts.

## Naming convention

New work should be placed under the appropriate study subfolder. Inputs shared
by both studies belong in `data/shared`; study-specific intermediate datasets
belong in `data/<study>/derived`; model objects and model-estimate files belong
in `models/<study>`; publication-ready figures and tables belong in
`figs/<study>` and `outputs/<study>`.
