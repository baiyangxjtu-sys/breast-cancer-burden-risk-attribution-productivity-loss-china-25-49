# Breast cancer burden, risk-attribution patterns, and mortality-related productivity loss in Chinese women aged 25–49 years

Reproducible analysis code and author-derived outputs for the revised iScience manuscript.

## Repository status

- **Frozen analysis release:** v1.1.10
- **Release role:** final reporting-and-packaging release used for the revised manuscript and Data S1
- **Lead Contact:** Jian Zhang
- **Repository owner:** Yang Bai (`baiyangxjtu-sys`)

Version v1.1.10 preserves the validated analytical results and incorporates the final reporting, terminology, documentation, output-index, and release-hygiene corrections used for resubmission.

### Second-round presentation sync — 2026-08-30

Following the second-round iScience review, presentation and terminology were synchronized without changing the analytical release. The frozen analytical version remains v1.1.10.

The R2 update includes:
- a separate R2 figure-presentation patch script;
- revised publication-facing main figures;
- clarification that the arithmetic mean of the five age-specific PAF estimates is an unweighted descriptive mean rather than an overall or population-weighted PAF;
- corresponding presentation and documentation updates.

No input data, model specifications, statistical formulas, random seeds, or numerical analytical results were changed.

## Study scope

The analysis integrates aggregated population-level information from:

- Global Burden of Disease Study 2023
- GLOBOCAN 2022
- United Nations World Population Prospects 2024
- International Labour Organization
- World Bank

The main analytic population is women aged 25–49 years. No individual-level clinical, exposure, or outcome records were used.

## Repository structure

```text
.
├── Code/
│   ├── GBD_EOBC_full_pipeline_FINAL_STANDALONE_v1.1.10.R
│   ├── GBD_EOBC_full_pipeline_FINAL_STANDALONE_v1.1.10_R2_FIGURE_PATCH_FINAL.R
│   └── README.md
├── Results/
│   ├── Manuscript_Ready/
│   ├── Sensitivity_Results/
│   ├── Analysis_Tables/
│   ├── Analysis_Figures/
│   └── Logs/
├── Documentation/
├── Source_Data_Access/
├── README.md
├── CHANGELOG.md
├── CITATION.cff
├── LICENSE
├── LICENSE-DATA.md
├── FILE_MANIFEST_v1.1.10_FROZEN.csv
└── SHA256SUMS_v1.1.10_FROZEN.txt
```

`Results/Manuscript_Ready/` is the publication-facing output set. It contains the 6 main figures, 2 main tables, 6 supplementary figures, 17 supplementary tables, consolidated supplementary workbook, figure source data, and output-quality documentation used for the revised manuscript.

## Main analytical components

1. Burden estimation and restricted-age weighted rate calculation
2. Sequential decomposition of changing incident-case counts
3. Cohort-specific descriptive incidence analysis
4. Descriptive GBD PAF analysis
5. Exploratory XGBoost–SHAP feature ranking
6. Forward temporal validation and grouped calendar-block bootstrap
7. Structural and target sensitivity analyses
8. Mortality-to-incidence ratio calculation
9. Human Capital Approach estimation of mortality-related productivity loss
10. Bayesian age–period–cohort status-quo projection to 2050
11. GBD–GLOBOCAN external-baseline comparison and scaling sensitivity analysis

## Software

The frozen analysis was run in R 4.5.1 on macOS. Key packages include tidyverse, dplyr, tidyr, ggplot2, INLA, xgboost, shapviz, forecast, patchwork, cowplot, and openxlsx. See `Results/Logs/sessionInfo_final.txt` for the complete software environment.

## Input data and redistribution policy

Original third-party source files are **not redistributed** in this public repository. Obtain the required source files from the official providers and place them in a local `data/` directory.

Expected filenames, file patterns, source roles, and GBD export specifications are documented in:

- `Source_Data_Access/DATA_SOURCES.md`
- `Source_Data_Access/EXPECTED_INPUT_FILES.txt`
- `Source_Data_Access/GBD_QUERY_SPECIFICATIONS.md`

The repository contains analysis code and author-derived outputs. Original third-party datasets remain subject to the terms of their respective providers.

## How to run

### 1. Clone or download the repository

```bash
git clone https://github.com/baiyangxjtu-sys/breast-cancer-burden-risk-attribution-productivity-loss-china-25-49.git
```

### 2. Create the local data folder

```text
<PROJECT_ROOT>/data/
```

Place the required third-party input files there. Do not commit the `data/` directory.

### 3. Set the project root

The author's default project root is `/Users/baiyang/Desktop/GBD`. On another system, set:

```r
Sys.setenv(
  GBD_EOBC_PROJECT_ROOT = "/absolute/path/to/project"
)
```

### 4. Run the standalone analysis

```text
Code/GBD_EOBC_full_pipeline_FINAL_STANDALONE_v1.1.10.R
```

The pipeline writes results to the v1.1.10 output directory and performs automated output checks.

## Expected validation state

The frozen v1.1.10 release passed the supplied publication-output checks:

- `Results/Manuscript_Ready/Documentation/V1_1_10_OUTPUT_QUALITY_CHECKS.csv`
- `Results/Manuscript_Ready/Documentation/MANUSCRIPT_OUTPUT_QUALITY_CHECKS.csv`

All required checks in these files are reported as `PASS` in the frozen release.

## Interpretation boundaries

- XGBoost–SHAP is used for **exploratory model-based feature ranking**, not causal inference or individual-level risk prediction.
- The China high-BMI PAF series is an observed constant-zero series in the analytic panel and is excluded from model-based feature ranking; this does not imply that obesity exposure is absent or biologically unimportant.
- The residual component of case-growth decomposition is descriptive and should not be interpreted as a causal or metabolic component.
- GLOBOCAN is used for external-baseline comparison/scaling sensitivity, not external validation or calibration.
- The BAPC analysis is a status-quo projection scenario; long-horizon uncertainty should not be interpreted as a deterministic expected outcome.
- Mortality-related productivity loss does not represent total economic or societal burden.

## Package integrity

`FILE_MANIFEST_v1.1.10_FROZEN.csv` and `SHA256SUMS_v1.1.10_FROZEN.txt` are retained as integrity records for the frozen v1.1.10 analytical release.

The second-round iScience revision added presentation-only files and documentation updates on 2026-08-30. These R2 presentation-sync additions do not alter the frozen analytical calculations or numerical results. A fully refreshed manifest and SHA-256 checksum set for the R2-synchronized reproducibility package is provided in Data S1.

## Licenses

- Analysis code: MIT License
- Author-derived tables, figure source data, figures, and documentation: CC BY 4.0, as described in `LICENSE-DATA.md`
- Original third-party data: governed by the terms of the respective data providers

## Citation

Citation metadata are provided in `CITATION.cff`.

- Yang Bai: ORCID 0009-0001-4478-4383
- Jian Zhang: ORCID 0000-0003-1144-3627

The article DOI and preferred journal citation can be added after publication.

## Contact

**Lead Contact**  
Jian Zhang  
Department of Breast Surgery  
The First Affiliated Hospital of Xi'an Jiaotong University  
Xi'an, Shaanxi, China  
Email: `zjxjtu14@163.com`  
ORCID: `0000-0003-1144-3627`
