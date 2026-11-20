# Strict temporal permutation summary



|Period         |Observed RR (95% CI) |Null median RR |Null 95% interval |Upper-tail p |Centered two-sided p |Max-T p |
|:--------------|:--------------------|:--------------|:-----------------|:------------|:--------------------|:-------|
|Days -14 to -8 |1.037 (1.016-1.058)  |1.033          |1.021-1.044       |0.285        |0.600                |0.745   |
|Days -7 to -1  |1.048 (1.026-1.071)  |1.038          |1.027-1.050       |0.050        |0.115                |0.195   |
|Days 0 to +6   |1.057 (1.033-1.081)  |1.036          |1.026-1.047       |0.005        |0.005                |0.005   |
|Days +7 to +13 |1.047 (1.022-1.072)  |1.033          |1.022-1.047       |0.035        |0.055                |0.085   |

Notes: Each of 199 calendars preserves the observed event year, weekday, and ZIP footprint; keeps the complete days -14 through +13 window within May-November; and places every placebo impact at least 28 days from every observed impact affecting that footprint. Thus no placebo ZIP-day overlaps an observed event window. The same all-age fixed-effects Poisson specification is fit to every calendar. The global standardized max-T p-value is 0.005.

Interpretation: The placebo distributions remain positively shifted, with median RRs of 1.033-1.038, demonstrating residual temporal structure unrelated to exact storm timing. The observed days 0 to +6 RR of 1.057 exceeds all 199 permutations and remains significant after four-period max-T adjustment (p=0.005). Days +7 to +13 has upper-tail p=0.035 but max-T p=0.085. Neither pre-impact period survives max-T adjustment (days -14 to -8 p=0.745; days -7 to -1 p=0.195). Accordingly, the strict temporal falsification strongly supports the impact-week association but weakens a causal interpretation of the pre-impact elevations.
