# Table S9. Spatial model and global Moran results



|Target                                              | ZIPs estimated | Observed / expected deaths | Statewide RR | EB prior mean (SD) | Reliability median [IQR] |     Queen I (p; q)      |      Rook I (p; q)      |      6NN I (p; q)       | Local FDR clusters |
|:---------------------------------------------------|:--------------:|:--------------------------:|:------------:|:------------------:|:------------------------:|:-----------------------:|:-----------------------:|:-----------------------:|:------------------:|
|Cardiovascular, age 65-74, Days 0 to +6             |    964/1046    |       2,305 / 2071.7       |    1.113     |   1.113 (0.010)    | 0.0002 [0.0001, 0.0003]  | 0.032 (0.1175; 0.2350)  | 0.035 (0.0951; 0.2252)  | 0.013 (0.4103; 0.8206)  |         0          |
|Cardiovascular, age 75+, Days 0 to +6               |    982/1046    |       8,779 / 8334.2       |    1.053     |   1.053 (0.031)    | 0.0058 [0.0021, 0.0117]  | 0.010 (0.5936; 0.5936)  | 0.007 (0.7100; 0.7100)  | 0.002 (0.8618; 0.9178)  |         0          |
|Cardiovascular, age 75+, Days +7 to +13             |    982/1046    |       8,474 / 8111.5       |    1.045     |   1.044 (0.082)    | 0.0379 [0.0138, 0.0744]  | 0.035 (0.0872; 0.2350)  | 0.033 (0.1063; 0.2252)  | 0.028 (0.0871; 0.3484)  |         2          |
|Respiratory, age 75+, Days +7 to +13                |    954/1046    |       2,041 / 1879.7       |    1.086     |   1.086 (0.010)    | 0.0001 [0.0001, 0.0003]  | -0.018 (0.4201; 0.5601) | -0.016 (0.4846; 0.6461) | 0.001 (0.9178; 0.9178)  |         0          |
|Injuries, age 75+, Days 0 to +6                     |    928/1046    |        617 / 501.4         |    1.231     |   1.229 (0.227)    | 0.0171 [0.0075, 0.0299]  | -0.012 (0.5909; 0.5936) | -0.011 (0.6329; 0.7100) | -0.009 (0.6265; 0.9178) |         0          |
|Neuropsychiatric disorders, age 75+, Days 0 to +6   |    959/1046    |       2,744 / 2513.3       |    1.092     |   1.092 (0.010)    | 0.0002 [0.0001, 0.0003]  | -0.018 (0.4131; 0.5601) | -0.017 (0.4671; 0.6461) | 0.003 (0.8038; 0.9178)  |         0          |
|Neuropsychiatric disorders, age 75+, Days +7 to +13 |    959/1046    |       2,610 / 2470.9       |    1.056     |   1.056 (0.010)    | 0.0002 [0.0001, 0.0004]  | 0.033 (0.1093; 0.2350)  | 0.033 (0.1126; 0.2252)  | 0.021 (0.1934; 0.5157)  |         0          |
|Neoplasms, age 75+, Days -7 to -1                   |    972/1046    |       4,174 / 3891.6       |    1.073     |   1.073 (0.010)    | 0.0003 [0.0001, 0.0005]  | 0.041 (0.0454; 0.2350)  | 0.046 (0.0274; 0.2192)  | 0.041 (0.0134; 0.1072)  |         0          |

Notes: ZIP rate ratios were stabilized with gamma-Poisson empirical-Bayes smoothing. Global Moran tests used centered log EB rate ratios, row-standardized weights, and 9,999 permutations. q-values are Benjamini-Hochberg adjustments across the eight targets within each weight definition. Queen contiguity is primary; rook and symmetric six-nearest-neighbor weights are sensitivity analyses. Local FDR clusters use within-map Benjamini-Hochberg correction.
