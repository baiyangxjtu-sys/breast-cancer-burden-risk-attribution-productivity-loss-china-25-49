# v1.1.10 post-run audit

Audit date: 2026-08-02

## Result

The v1.1.10 pipeline outputs passed analytical and reporting checks.

- Projection anchoring checks passed.
- The 32-entry manuscript output index is complete and every indexed file exists.
- All rows in `V1_1_10_OUTPUT_QUALITY_CHECKS.csv` and `MANUSCRIPT_OUTPUT_QUALITY_CHECKS.csv` are PASS.
- Supplementary Figure S1 no longer clips the high-BMI subtitle.
- Supplementary Figure S5 uses external-baseline scaling terminology and states that scaling-factor uncertainty was not propagated.
- Supplementary Table S12 uses “relative change,” and public audit exports use baseline-scaling field names.
- HCA multipliers are described as illustrative assumptions rather than empirical age-specific wage estimates.
- Relative to the preceding output set, unchanged analytical CSVs retain identical values; differences are limited to intended terminology and reporting labels.

## Release correction

The author-created archive named `GBD_EOBC_final_code_package_v1.1.10.zip` contained only `Results/` and included macOS metadata. This clean release restores the standalone R code and release documentation and excludes `__MACOSX`, `.DS_Store`, and AppleDouble `._*` files.

## Scope

This audit does not establish that third-party input data may be redistributed. Raw GBD, GLOBOCAN, UN, ILO, and World Bank files are not included in this archive.
