# Changelog

## R2 presentation sync — 2026-08-30

This second-round iScience revision is a presentation and terminology synchronization of the frozen v1.1.10 analytical release.

### R2 presentation updates

- Added the R2 figure-presentation patch while retaining the original v1.1.10 standalone analytical pipeline.
- Revised Figure 2A from a waterfall plot to a standard bar chart.
- Revised Figure 2C to a grouped bar chart.
- Reordered Figure 3 specific-risk attribution panels to Global first and China second.
- Clarified Global/China wording in relevant figure titles and labels.
- Renamed the five-age-group PAF arithmetic summary as a descriptive mean rather than an overall PAF.
- Synchronized the consolidated supplementary workbook and publication-facing documentation.

### Analytical invariance

No input data, model specifications, statistical formulas, XGBoost parameters, random seeds, decomposition calculations, BAPC calculations, HCA calculations, or numerical analytical results were changed. The frozen analytical release remains v1.1.10.

## v1.1.10 — final reporting-and-packaging release

This is the canonical frozen release accompanying the revised manuscript and Data S1. It is based on the analytically validated v1.1.8 results and the v1.1.9 audit run.

### Reporting and packaging refinements

- Standardized publication-facing terminology and captions.
- Corrected Supplementary Table S12 terminology from relative increase to relative change.
- Clarified that GLOBOCAN-to-GBD scaling-factor uncertainty is not propagated into the scaled projection uncertainty.
- Clarified HCA multipliers as illustrative sensitivity-scenario assumptions.
- Added exact calendar-block sampling descriptions for SHAP sensitivity analyses.
- Replaced residual projection “calibration” wording with “anchoring” where applicable.
- Completed the manuscript output index to include all 6 main figures, 2 main tables, 6 supplementary figures, 17 supplementary tables, and the consolidated workbook.
- Corrected release metadata and output-quality documentation to v1.1.10.
- Added release-hygiene checks and clean-archive guidance.

### Analytical invariance

No intentional changes were made to input data, model specifications, statistical formulas, XGBoost parameters, random seeds, BAPC methods, HCA calculations, or numerical results relative to the validated analytical foundation.
