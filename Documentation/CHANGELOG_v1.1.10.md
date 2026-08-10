# CHANGELOG — v1.1.10

## Scope
Final reporting-and-packaging release based on the analytically validated v1.1.8 results and the v1.1.9 audit run.

## Corrections and refinements
- Shortened the Supplementary Figure S1 in-figure high-BMI subtitle to prevent clipping; retained the complete interpretation in the figure caption.
- Standardized GLOBOCAN baseline-scaling terminology across publication-facing outputs, raw audit CSV files, and workbook sheets.
- Corrected Supplementary Table S12 from “relative increase” to “relative change,” because some scaling factors are below 1.
- Added the statement that GLOBOCAN-to-GBD scaling-factor uncertainty is not propagated to Figure S5 and its caption.
- Clarified HCA multipliers as illustrative scenario assumptions rather than empirical age-specific female wage estimates.
- Added the exact calendar-block sampling description to SHAP sensitivity figures/captions.
- Replaced residual projection “calibration” wording with “anchoring” where it referred to the 2023 projection anchor.
- Completed `MANUSCRIPT_OUTPUT_INDEX.csv` to include 6 main figures, 2 main tables, 6 supplementary figures, 17 supplementary tables, and the consolidated workbook (32 entries).
- Corrected all release documentation and quality-check filenames to v1.1.10.
- Added release-hygiene instructions and a clean-ZIP creation utility that excludes macOS hidden files.
- Renamed the evidence audit output to `Computational_Evidence_Audit_Summary_v1.1.10.csv` to reflect its scope.

## Analytical invariance
No intentional changes were made to input data, model specifications, statistical formulas, XGBoost parameters, random seeds, BAPC methods, HCA calculations, or numerical results.
