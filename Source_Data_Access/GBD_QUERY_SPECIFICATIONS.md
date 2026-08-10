# GBD export specifications required by the v1.1.10 pipeline

## Purpose

This document records the GBD filters required by the standalone analysis code. Local filenames such as `data1a.csv` and `data3a.csv` are author aliases and are not official GHDx dataset identifiers.

## Common settings

- **Cause:** Breast cancer
- **Sex:** Female
- **Years:** 1990–2023
- **Main analytic age groups:** 25–29, 30–34, 35–39, 40–44, and 45–49 years
- **Restricted-age standardization:** recalculated by the authors from the five age-specific groups using fixed GBD standard-population weights

## A. Burden exports: `data1*.csv`

### Required locations

China, Global, High SDI, High-middle SDI, Middle SDI, Low-middle SDI, and Low SDI.

### Required measures

Incidence, prevalence, deaths, DALYs, YLLs, and YLDs.

### Required metrics

Number and Rate.

### Uses

Table 1, historical burden trends, weighted rates, sequential decomposition, cohort-specific descriptive analysis, MIR, mortality-related productivity loss, and historical case/population reconstruction for BAPC projection.

## B. Risk-attribution exports: `data3*.csv`

### Required locations

China and Global for the principal risk-attribution analyses.

### Required metric

Percent.

### Required measures

- DALYs for descriptive PAF analyses and exploratory XGBoost–SHAP feature ranking
- YLLs for risk-attributable mortality-related productivity loss

### Required broad risk categories

Behavioral risks, dietary risks, and metabolic risks.

### Required specific risks

High alcohol use, high body-mass index, high fasting plasma glucose, low physical activity, and tobacco.

## C. World-map exports: `data6*.csv`

- All available countries/territories required for mapping
- Female
- Breast cancer
- Year 2023
- Age-standardized
- Metric: Rate
- Measures: incidence, prevalence, deaths, DALYs

These files are used only for the descriptive world maps in Figure 1.

## Validation expectations

The pipeline checks/enforces the required female breast-cancer rows, age groups and period, location groups, completeness of the DALY target and PAF feature panel, the China high-BMI zero-variance series, YLL-specific PAF coverage for HCA, interval ordering, and manuscript-output consistency.

Exact original GBD files are not redistributed. Re-users should document their own GHDx download date and retain their query/export metadata.
