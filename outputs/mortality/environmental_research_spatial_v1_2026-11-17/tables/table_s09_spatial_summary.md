# Table S9. Spatial model and global Moran results



|Target                                            | ZIPs estimated | Observed / expected deaths | Statewide RR | EB prior mean (SD) | Reliability median [IQR] |     Queen I (p; q)      |      Rook I (p; q)      |      6NN I (p; q)      | Local FDR clusters |
|:-------------------------------------------------|:--------------:|:--------------------------:|:------------:|:------------------:|:------------------------:|:-----------------------:|:-----------------------:|:----------------------:|:------------------:|
|Cardiovascular, age 65-74, Days 0 to +6           |    964/1046    |       2,305 / 2071.7       |    1.113     |   1.113 (0.010)    | 0.0002 [0.0001, 0.0003]  | 0.032 (0.1175; 0.2938)  | 0.035 (0.0951; 0.2377)  | 0.013 (0.4103; 0.9178) |         0          |
|Cardiovascular, age 75+, Days 0 to +6             |    982/1046    |       8,779 / 8334.2       |    1.053     |   1.053 (0.031)    | 0.0058 [0.0021, 0.0117]  | 0.010 (0.5936; 0.5936)  | 0.007 (0.7100; 0.7100)  | 0.002 (0.8618; 0.9178) |         0          |
|Respiratory, age 75+, Days +7 to +13              |    954/1046    |       2,041 / 1879.7       |    1.086     |   1.086 (0.010)    | 0.0001 [0.0001, 0.0003]  | -0.018 (0.4201; 0.5251) | -0.016 (0.4846; 0.6058) | 0.001 (0.9178; 0.9178) |         0          |
|Neuropsychiatric disorders, age 75+, Days 0 to +6 |    959/1046    |       2,744 / 2513.3       |    1.092     |   1.092 (0.010)    | 0.0002 [0.0001, 0.0003]  | -0.018 (0.4131; 0.5251) | -0.017 (0.4671; 0.6058) | 0.003 (0.8038; 0.9178) |         0          |
|Neoplasms, age 75+, Days -7 to -1                 |    972/1046    |       4,174 / 3891.6       |    1.073     |   1.073 (0.010)    | 0.0003 [0.0001, 0.0005]  | 0.041 (0.0454; 0.2270)  | 0.046 (0.0274; 0.1370)  | 0.041 (0.0134; 0.0670) |         0          |

Notes: ZIP rate ratios were stabilized with gamma-Poisson empirical-Bayes smoothing. Global Moran tests used centered log EB rate ratios, row-standardized weights, and 9,999 permutations. q-values are Benjamini-Hochberg adjustments across the five targets within each weight definition. Queen contiguity is primary; rook and symmetric six-nearest-neighbor weights are sensitivity analyses. Local FDR clusters use within-map Benjamini-Hochberg correction.
