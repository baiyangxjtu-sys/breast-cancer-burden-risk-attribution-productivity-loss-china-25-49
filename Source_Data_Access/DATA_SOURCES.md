# Source data access and provenance

## Overview

This repository contains the standalone analysis code and author-derived outputs for the study:

**Breast cancer burden, risk-attribution patterns, and mortality-related productivity loss in Chinese women aged 25–49 years**

The frozen analysis release is **v1.1.10**.

Original third-party source files are **not redistributed** in this repository. Users should obtain those files directly from the official providers and place them in a local `data/` directory before running the analysis.

## 1. Global Burden of Disease Study 2023

- **Provider:** Institute for Health Metrics and Evaluation (IHME)
- **Platform:** Global Health Data Exchange (GHDx)
- **Downloaded:** May 12, 2026
- **Population:** Female
- **Cause:** Breast cancer
- **Historical period:** 1990–2023
- **Main analytic ages:** 25–29, 30–34, 35–39, 40–44, and 45–49 years
- **Comparison locations:** China, Global, High SDI, High-middle SDI, Middle SDI, Low-middle SDI, and Low SDI

### Local input groups

- `data1*.csv`: breast-cancer burden estimates used for incidence, prevalence, deaths, DALYs, YLLs, YLDs, weighted rates, decomposition, MIR, HCA, and BAPC.
- `data3*.csv`: GBD Comparative Risk Assessment PAF estimates used for descriptive risk-attribution analyses, exploratory XGBoost–SHAP ranking, and YLL-specific risk-attributable productivity loss.
- `data6*.csv`: all-location, age-standardized rate estimates used for the descriptive world maps in Figure 1.

The independent GBD 2021 SDI file was not used by the frozen v1.1.10 pipeline. SDI reference groups were read directly from the GBD burden exports.

## 2. GLOBOCAN 2022

- **Provider:** International Agency for Research on Cancer
- **Platform:** Global Cancer Observatory, Cancer Today
- **Use in this study:** external comparison of the 2022 incidence baseline and an external-baseline scaling sensitivity analysis

The analysis uses age-specific 2022 incident-case estimates for China and the global population at ages 25–29, 30–34, 35–39, 40–44, and 45–49 years. The ten case estimates are embedded in the standalone R script for transparent reproduction.

Comparison rates are derived using matched 2022 population denominators reconstructed from the GBD analytic panel. They are therefore rates derived from GLOBOCAN case estimates, not official GLOBOCAN incidence rates.

GLOBOCAN projection exports for 2022–2050 were checked for provenance but were not used as inputs to the Bayesian age–period–cohort model.

## 3. United Nations World Population Prospects 2024

- **Provider:** United Nations, Department of Economic and Social Affairs, Population Division
- **Accessed:** April 7, 2026
- **Local file:** `UN_Pop.csv.csv`
- **Variant:** Medium
- **Sex:** Female
- **Age grouping:** 5-year age groups
- **Population unit in source file:** Thousands of persons
- **Use in this study:** China female population projections for 2024–2050

The pipeline multiplies `PopFemale` by 1,000 to convert the source values from thousands of persons to persons.

## 4. International Labour Organization

- **Provider:** International Labour Organization
- **Platform:** ILOSTAT
- **Accessed:** April 7, 2026
- **Indicator:** Labour force participation rate by sex and age (%)
- **Location:** China
- **Sex:** Female
- **Local analysis file:** `data5a.xlsx`

The local extract contains population-census observations. For the five analytic age groups, the pipeline uses observed 1990, 2000, and 2010 values, linearly interpolates intermediate years, and extends the nearest observed value to boundary years through 2023. Percentages are divided by 100 before analysis.

## 5. World Bank

- **Provider:** World Bank
- **Database:** World Development Indicators
- **Indicator:** GDP per capita (constant 2015 US$)
- **Indicator code:** `NY.GDP.PCAP.KD`
- **Downloaded:** May 12, 2026
- **Local file pattern:** `API_NY.GDP.PCAP.KD_*.csv`
- **Use in this study:** annual China GDP per capita for 1990–2023 in the mortality-related productivity-loss analysis

## Raw-data redistribution policy

The following source files remain outside the public repository:

- GBD export files (`data1*.csv`, `data3*.csv`, and `data6*.csv`)
- GLOBOCAN original exports
- `UN_Pop.csv.csv`
- `data5a.xlsx` and other ILO original exports
- World Bank original CSV exports
- unused GBD, SDI, and intermediate `.RData` files

The repository shares the standalone code, author-derived tables, figure source data, final figures, software-environment records, and quality-control outputs.
