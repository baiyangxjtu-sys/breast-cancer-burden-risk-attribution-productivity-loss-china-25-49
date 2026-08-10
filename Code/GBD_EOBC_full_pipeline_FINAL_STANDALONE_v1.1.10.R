# ==============================================================================
# GBD EOBC Full Pipeline
# Breast cancer burden, decomposition, exploratory risk-feature ranking,
# mortality-related productivity loss, external comparison, and BAPC projection
# Final standalone analysis version v1.1.10 (reporting-only patch)
# Fully standalone single-file release; no modules folder is required
# ==============================================================================

rm(list = ls())
gc()

analysis_version <- "v1.1.10"
analysis_package_name <- paste0(
  "GBD_EOBC_final_code_package_",
  analysis_version
)

# ------------------------------------------------------------------------------
# 0. Project and path settings
# ------------------------------------------------------------------------------

# Detect the folder containing this script so paths remain portable across systems.
detect_script_dir <- function() {
  command_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", command_args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(
      sub("^--file=", "", file_arg[1]),
      winslash = "/", mustWork = FALSE
    )))
  }

  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    active_path <- rstudioapi::getActiveDocumentContext()$path
    if (nzchar(active_path)) {
      return(dirname(normalizePath(
        active_path, winslash = "/", mustWork = FALSE
      )))
    }
  }

  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

# The default project folder is retained for the author's Mac. It can be
# overridden without editing this script by setting GBD_EOBC_PROJECT_ROOT.
project_root <- Sys.getenv(
  "GBD_EOBC_PROJECT_ROOT",
  unset = "/Users/baiyang/Desktop/GBD"
)

# The script can be stored either directly under the project folder or inside
# a versioned code-package folder. Results are always written to the versioned
# package folder so that outputs from earlier runs are not overwritten.
detected_script_dir <- detect_script_dir()
expected_code_root <- file.path(project_root, analysis_package_name)

if (basename(detected_script_dir) == analysis_package_name) {
  code_root <- detected_script_dir
} else {
  code_root <- expected_code_root
}

dir.create(code_root, showWarnings = FALSE, recursive = TRUE)

input_path <- file.path(project_root, "data")
output_path <- file.path(code_root, "Results")

primary_results_dir <- file.path(
  output_path,
  "Primary_Results"
)

sensitivity_results_dir <- file.path(
  output_path,
  "Sensitivity_Results"
)

analysis_tables_dir <- file.path(
  output_path,
  "Analysis_Tables"
)

analysis_figures_dir <- file.path(
  output_path,
  "Analysis_Figures"
)

logs_dir <- file.path(
  output_path,
  "Logs"
)

# Store primary manuscript figures and tables separately from sensitivity outputs.
fig_pdf_dir <- file.path(
  primary_results_dir,
  "Figures_PDF"
)

fig_png_dir <- file.path(
  primary_results_dir,
  "Figures_PNG"
)

tables_dir <- file.path(
  primary_results_dir,
  "Tables"
)

# Compatibility alias used by several export sections.
tab_dir <- tables_dir

# Validate the fixed project structure before creating result folders.
if (!dir.exists(project_root)) {
  stop(
    "Project folder not found: ",
    project_root
  )
}

if (!dir.exists(input_path)) {
  stop(
    "Input folder not found: ",
    input_path,
    "\nExpected location: /Users/baiyang/Desktop/GBD/data"
  )
}

# code_root is created automatically for this standalone release.

output_dirs <- c(
  output_path,
  primary_results_dir,
  sensitivity_results_dir,
  analysis_tables_dir,
  analysis_figures_dir,
  logs_dir,
  fig_pdf_dir,
  fig_png_dir,
  tables_dir
)

invisible(
  lapply(
    output_dirs,
    dir.create,
    showWarnings = FALSE,
    recursive = TRUE
  )
)

message("Project root: ", project_root)
message("Code root:    ", code_root)
message("Input data:   ", input_path)
message("Output:       ", output_path)

# ------------------------------------------------------------------------------
# 1. Packages
# ------------------------------------------------------------------------------

cran_packages <- c(
  "tidyverse", "scales", "patchwork", "cowplot", "ggrepel", "forecast",
  "maps", "openxlsx", "xgboost", "shapviz", "viridis"
)

for (pkg in cran_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# INLA is installed only when absent. Avoid reinstalling it on every run.
options(timeout = 1200)
if (!requireNamespace("INLA", quietly = TRUE)) {
  install.packages(
    "INLA",
    repos = c(
      CRAN = "https://cloud.r-project.org",
      INLA = "https://inla.r-inla-download.org/R/stable"
    ),
    dependencies = TRUE
  )
}
library(tidyverse)
library(scales)
library(patchwork)
library(cowplot)
library(ggrepel)
library(forecast)
library(maps)
library(openxlsx)
library(INLA)
library(xgboost)
library(shapviz)
library(viridis)

capture.output(
  sessionInfo(),
  file = file.path(logs_dir, "sessionInfo_final.txt")
)

# ------------------------------------------------------------------------------
# 2. Global settings
# ------------------------------------------------------------------------------

target_locations <- c(
  "China", "Global", "High SDI", "High-middle SDI",
  "Middle SDI", "Low-middle SDI", "Low SDI"
)

age_specific <- c(
  "25-29 years", "30-34 years", "35-39 years",
  "40-44 years", "45-49 years"
)

all_ages <- c("Age-standardized", age_specific)

color_china  <- "#BC3C29FF"
color_global <- "#0072B5FF"

sdi_colors <- c(
  "High SDI"        = "#20854EFF",
  "High-middle SDI" = "#00A087FF",
  "Middle SDI"      = "#4DBBD5FF",
  "Low-middle SDI"  = "#8491B4FF",
  "Low SDI"         = "#B09C85FF"
)

std_weights_25_49 <- tibble(
  age_name = age_specific,
  std_weight = c(0.0807, 0.0754, 0.0699, 0.0645, 0.0585)
)

save_plot_both <- function(plot_obj, file_stub, width, height, dpi = 600) {
  ggsave(
    filename = file.path(fig_pdf_dir, paste0(file_stub, ".pdf")),
    plot = plot_obj,
    width = width,
    height = height,
    device = "pdf"
  )
  ggsave(
    filename = file.path(fig_png_dir, paste0(file_stub, ".png")),
    plot = plot_obj,
    width = width,
    height = height,
    dpi = dpi,
    bg = "white"
  )
}

theme_pub <- function(base_size = 14) {
  theme_classic(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", color = "black"),
      axis.title = element_text(color = "black"),
      axis.text = element_text(color = "black"),
      legend.title = element_text(face = "bold", color = "black"),
      legend.text = element_text(color = "black"),
      strip.text = element_text(face = "bold", color = "black"),
      strip.background = element_rect(fill = "#F2F2F2", color = "black", linewidth = 0.7)
    )
}

# ------------------------------------------------------------------------------
# 3. Data reading
# ------------------------------------------------------------------------------

message("▶ Reading GBD and auxiliary data...")

data1_files <- list.files(input_path, pattern = "^data1.*\\.csv$", full.names = TRUE)
if (length(data1_files) == 0) stop("No data1*.csv files found.")

df_full <- data1_files %>%
  map_dfr(~ read_csv(.x, show_col_types = FALSE)) %>%
  filter(
    sex_name == "Female",
    cause_name == "Breast cancer",
    location_name %in% target_locations,
    age_name %in% all_ages,
    year <= 2023
  )

data3_files <- list.files(input_path, pattern = "^data3.*\\.csv$", full.names = TRUE)
if (length(data3_files) == 0) stop("No data3*.csv files found.")

df_data3 <- data3_files %>%
  map_dfr(~ read_csv(.x, show_col_types = FALSE))

data6_files <- list.files(input_path, pattern = "^data6.*\\.csv$", full.names = TRUE)
if (length(data6_files) == 0) warning("No data6*.csv found. Figure 1 maps will fail if required.")

gdp_files <- list.files(
  input_path,
  pattern = "^API_NY\\.GDP\\.PCAP\\.KD_.*\\.csv$",
  full.names = TRUE
)

if (length(gdp_files) != 1L) {
  stop(
    "Expected exactly one World Bank file for indicator ",
    "NY.GDP.PCAP.KD, but found ",
    length(gdp_files),
    "."
  )
}

file_gdp <- gdp_files[[1]]

df_gdp_raw <- read_csv(
  file_gdp,
  skip = 4,
  show_col_types = FALSE
)

indicator_codes <- unique(
  stats::na.omit(df_gdp_raw$`Indicator Code`)
)

if (
  length(indicator_codes) != 1L ||
  indicator_codes != "NY.GDP.PCAP.KD"
) {
  stop(
    "Unexpected World Bank indicator code: ",
    paste(indicator_codes, collapse = ", "),
    ". Expected NY.GDP.PCAP.KD."
  )
}

df_gdp <- df_gdp_raw %>%
  filter(`Country Name` == "China") %>%
  select(starts_with("19"), starts_with("20")) %>%
  pivot_longer(
    cols = everything(),
    names_to = "year",
    values_to = "GDP_per_capita"
  ) %>%
  mutate(
    year = as.numeric(year),
    GDP_per_capita = as.numeric(GDP_per_capita)
  ) %>%
  filter(year >= 1990, year <= 2023) %>%
  select(year, GDP_per_capita)

file_ilo <- file.path(input_path, "data5a.xlsx")
if (!file.exists(file_ilo)) stop("No data5a.xlsx ILO file found.")

df_ilo_raw <- openxlsx::read.xlsx(file_ilo)

df_ilo <- df_ilo_raw %>%
  select(year = time, age_raw = classif1.label, LFPR = obs_value) %>%
  mutate(
    year = as.numeric(year),
    LFPR = as.numeric(LFPR) / 100,
    age_name = paste0(str_extract(age_raw, "\\d{2}-\\d{2}"), " years")
  ) %>%
  filter(age_name %in% age_specific) %>%
  select(year, age_name, LFPR) %>%
  complete(year = 1990:2023, age_name) %>%
  group_by(age_name) %>%
  arrange(year) %>%
  mutate(
    LFPR = approx(
      x = year[!is.na(LFPR)],
      y = LFPR[!is.na(LFPR)],
      xout = year,
      rule = 2
    )$y
  ) %>%
  ungroup()

# ------------------------------------------------------------------------------
# 4. Weighted ASR functions and Table 1
# ------------------------------------------------------------------------------

calc_weighted_asr_25_49 <- function(data, measure_pattern, out_name) {
  data %>%
    filter(
      location_name %in% target_locations,
      age_name %in% age_specific,
      metric_name == "Rate",
      str_detect(measure_name, measure_pattern),
      year >= 1990,
      year <= 2023
    ) %>%
    left_join(std_weights_25_49, by = "age_name") %>%
    group_by(location_name, year) %>%
    summarise(
      "{out_name}" := sum(val * std_weight, na.rm = TRUE) /
        sum(std_weight[!is.na(val)], na.rm = TRUE),
      .groups = "drop"
    )
}

df_asir_25_49 <- calc_weighted_asr_25_49(df_full, "Incidence", "ASIR_25_49")
df_asdr_25_49 <- calc_weighted_asr_25_49(df_full, "Deaths|Death", "ASDR_25_49")
df_asdaly_25_49 <- calc_weighted_asr_25_49(df_full, "DALY", "ASDALY_25_49")

calc_eapc_from_asr <- function(data, rate_col, measure_label) {
  data %>%
    filter(.data[[rate_col]] > 0) %>%
    group_by(location_name) %>%
    group_modify(~ {
      fit <- lm(log(.x[[rate_col]]) ~ .x$year)
      beta <- coef(fit)[2]
      se <- summary(fit)$coefficients[2, 2]
      tibble(
        Measure = measure_label,
        EAPC = 100 * (exp(beta) - 1),
        Lower_CI = 100 * (exp(beta - 1.96 * se) - 1),
        Upper_CI = 100 * (exp(beta + 1.96 * se) - 1)
      )
    }) %>%
    ungroup() %>%
    mutate(
      across(c(EAPC, Lower_CI, Upper_CI), ~ round(.x, 2)),
      `EAPC (95% CI)` = paste0(EAPC, " (", Lower_CI, " to ", Upper_CI, ")")
    )
}

df_eapc_final <- bind_rows(
  calc_eapc_from_asr(df_asir_25_49, "ASIR_25_49", "ASIR"),
  calc_eapc_from_asr(df_asdr_25_49, "ASDR_25_49", "ASDR"),
  calc_eapc_from_asr(df_asdaly_25_49, "ASDALY_25_49", "AS-DALY rate")
)

write_csv(df_eapc_final, file.path(tables_dir, "Table_EAPC_weighted_ASR_25_49.csv"))

df_number_25_49 <- df_full %>%
  filter(
    location_name %in% target_locations,
    age_name %in% age_specific,
    metric_name == "Number",
    str_detect(measure_name, "Incidence|Deaths|Death"),
    year %in% c(1990, 2023)
  ) %>%
  mutate(
    measure_clean = case_when(
      str_detect(measure_name, "Incidence") ~ "Incidence",
      str_detect(measure_name, "Deaths|Death") ~ "Deaths",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(measure_clean)) %>%
  group_by(location_name, year, measure_clean) %>%
  summarise(Number = sum(val, na.rm = TRUE), .groups = "drop")

df_inc_table <- df_number_25_49 %>%
  filter(measure_clean == "Incidence") %>%
  left_join(df_asir_25_49, by = c("location_name", "year")) %>%
  mutate(
    Incidence = paste0(scales::comma(round(Number)), " (", round(ASIR_25_49, 2), ")")
  ) %>%
  select(location_name, year, Incidence) %>%
  pivot_wider(names_from = year, values_from = Incidence, names_prefix = "Incidence_")

df_death_table <- df_number_25_49 %>%
  filter(measure_clean == "Deaths") %>%
  left_join(df_asdr_25_49, by = c("location_name", "year")) %>%
  mutate(
    Deaths = paste0(scales::comma(round(Number)), " (", round(ASDR_25_49, 2), ")")
  ) %>%
  select(location_name, year, Deaths) %>%
  pivot_wider(names_from = year, values_from = Deaths, names_prefix = "Deaths_")

df_table1_final <- df_inc_table %>%
  left_join(df_death_table, by = "location_name") %>%
  rename(
    Location = location_name,
    `Incidence 1990 (ASIR)` = Incidence_1990,
    `Incidence 2023 (ASIR)` = Incidence_2023,
    `Deaths 1990 (ASDR)` = Deaths_1990,
    `Deaths 2023 (ASDR)` = Deaths_2023
  ) %>%
  left_join(
    df_eapc_final %>% filter(Measure == "ASIR") %>%
      select(Location = location_name, `Incidence EAPC` = `EAPC (95% CI)`),
    by = "Location"
  ) %>%
  left_join(
    df_eapc_final %>% filter(Measure == "ASDR") %>%
      select(Location = location_name, `Deaths EAPC` = `EAPC (95% CI)`),
    by = "Location"
  ) %>%
  mutate(Location = factor(Location, levels = target_locations)) %>%
  arrange(Location)

write_csv(df_table1_final, file.path(tables_dir, "Table1_Burden_EAPC_weighted_ASIR_25_49.csv"))

# ------------------------------------------------------------------------------
# 5. Figure 1: maps and temporal trends
# ------------------------------------------------------------------------------

message("▶ Building Figure 1...")

df_world_raw <- data6_files %>%
  map_dfr(~ read_csv(.x, show_col_types = FALSE)) %>%
  filter(
    year == 2023,
    age_name == "Age-standardized",
    metric_name == "Rate",
    str_detect(measure_name, "Incidence|Prevalence|Death|DALY")
  ) %>%
  mutate(
    measure_clean = case_when(
      str_detect(measure_name, "Incidence") ~ "Incidence",
      str_detect(measure_name, "Prevalence") ~ "Prevalence",
      str_detect(measure_name, "Death") ~ "Deaths",
      str_detect(measure_name, "DALY") ~ "DALYs",
      TRUE ~ NA_character_
    ),
    location_name = case_when(
      location_name == "United States of America" ~ "USA",
      location_name == "Russian Federation" ~ "Russia",
      location_name == "United Kingdom" ~ "UK",
      location_name == "Republic of Korea" ~ "South Korea",
      location_name == "Democratic People's Republic of Korea" ~ "North Korea",
      location_name == "Taiwan (Province of China)" ~ "Taiwan",
      location_name == "Iran (Islamic Republic of)" ~ "Iran",
      location_name == "Syrian Arab Republic" ~ "Syria",
      location_name == "Viet Nam" ~ "Vietnam",
      location_name == "Lao People's Democratic Republic" ~ "Laos",
      location_name == "United Republic of Tanzania" ~ "Tanzania",
      location_name == "Cote d'Ivoire" ~ "Ivory Coast",
      TRUE ~ location_name
    )
  )

world_map <- map_data("world") %>% filter(region != "Antarctica")
map_data_joined <- world_map %>% left_join(df_world_raw, by = c("region" = "location_name"))

plot_single_map <- function(target_measure, plot_title, legend_title) {
  ggplot(map_data_joined %>% filter(measure_clean == target_measure),
         aes(x = long, y = lat, group = group, fill = val)) +
    geom_polygon(color = "black", linewidth = 0.08) +
    scale_fill_distiller(
      palette = "YlOrRd",
      direction = 1,
      na.value = "grey90",
      name = legend_title,
      labels = scales::comma
    ) +
    coord_fixed(ratio = 1.3) +
    labs(title = plot_title) +
    theme_void(base_size = 12) +
    theme(
      legend.position = "right",
      legend.title = element_text(face = "bold", size = 9),
      legend.text = element_text(size = 8),
      plot.title = element_text(face = "bold", hjust = 0.5, size = 13)
    )
}

p_map_inc  <- plot_single_map("Incidence", "A1. Age-standardized incidence rate", "ASIR\nper 100,000")
p_map_prev <- plot_single_map("Prevalence", "A2. Age-standardized prevalence rate", "ASPR\nper 100,000")
p_map_dea  <- plot_single_map("Deaths", "A3. Age-standardized death rate", "ASDR\nper 100,000")
p_map_daly <- plot_single_map("DALYs", "A4. Age-standardized DALY rate", "AS-DALY\nper 100,000")

df_trend_weighted <- bind_rows(
  df_asir_25_49 %>% rename(Rate = ASIR_25_49) %>% mutate(measure_clean = "Incidence"),
  df_asdr_25_49 %>% rename(Rate = ASDR_25_49) %>% mutate(measure_clean = "Deaths"),
  df_asdaly_25_49 %>% rename(Rate = ASDALY_25_49) %>% mutate(measure_clean = "DALYs")
) %>%
  mutate(location_name = factor(location_name, levels = target_locations))

plot_trend <- function(m_name, title_txt, y_lab) {
  ggplot(df_trend_weighted %>% filter(measure_clean == m_name),
         aes(x = year, y = Rate, color = location_name)) +
    geom_line(aes(
      linewidth = location_name %in% c("China", "Global"),
      linetype = location_name == "Global"
    )) +
    scale_linewidth_manual(values = c("TRUE" = 1.2, "FALSE" = 0.8), guide = "none") +
    scale_linetype_manual(values = c("TRUE" = "dashed", "FALSE" = "solid"), guide = "none") +
    scale_color_manual(values = c("China" = color_china, "Global" = color_global, sdi_colors)) +
    scale_x_continuous(breaks = seq(1990, 2023, by = 10)) +
    labs(title = title_txt, x = "Year", y = y_lab) +
    theme_pub(12) +
    theme(legend.position = "none")
}

p1 <- plot_trend("Incidence", "B1. ASIR trends", "Age-standardized rate per 100,000")
p2 <- plot_trend("Deaths", "B2. ASDR trends", "Age-standardized rate per 100,000")
p3 <- plot_trend("DALYs", "B3. AS-DALY rate trends", "Age-standardized rate per 100,000")

df_comp <- df_full %>%
  filter(
    location_name == "China",
    metric_name == "Number",
    age_name %in% age_specific,
    str_detect(measure_name, "YLL|YLD")
  ) %>%
  mutate(
    component = ifelse(str_detect(measure_name, "YLL"), "YLLs (Premature death)", "YLDs (Disability)"),
    component = factor(component, levels = c("YLDs (Disability)", "YLLs (Premature death)"))
  ) %>%
  group_by(year, component) %>%
  summarise(val = sum(val, na.rm = TRUE), .groups = "drop")

p4 <- ggplot(df_comp, aes(x = year, y = val, fill = component)) +
  geom_area(alpha = 0.85, color = "white", linewidth = 0.2) +
  scale_fill_manual(values = c("YLDs (Disability)" = color_global, "YLLs (Premature death)" = color_china)) +
  scale_y_continuous(labels = scales::label_number(scale_cut = cut_short_scale())) +
  labs(title = "B4. DALY composition in China, ages 25–49", x = "Year", y = "Absolute DALYs", fill = "") +
  theme_pub(12) +
  theme(
    legend.position = c(0.35, 0.86),
    legend.background = element_rect(fill = alpha("white", 0.7), color = NA),
    legend.text = element_text(size = 8, face = "bold")
  )

dummy_legend <- ggplot(df_trend_weighted, aes(x = year, y = Rate, color = location_name)) +
  geom_line() +
  scale_color_manual(values = c("China" = color_china, "Global" = color_global, sdi_colors)) +
  theme_pub(12) +
  theme(legend.position = "bottom", legend.title = element_blank())

shared_legend_f1 <- cowplot::get_legend(dummy_legend)

layout_A_f1 <- (p_map_inc | p_map_prev) / (p_map_dea | p_map_daly)
layout_B_f1 <- ((p1 | p2) / (p3 | p4)) / shared_legend_f1 + plot_layout(heights = c(10, 10, 1))
fig1_final <- wrap_elements(layout_A_f1) / wrap_elements(layout_B_f1) + plot_layout(heights = c(1.25, 1))
save_plot_both(fig1_final, "Figure 1", width = 16, height = 18)

# ------------------------------------------------------------------------------
# 6. Figure 2: decomposition and cohort-specific pattern
# ------------------------------------------------------------------------------

message("▶ Building Figure 2...")

calc_decomposition <- function(loc) {
  df_loc <- df_full %>%
    filter(
      location_name == loc,
      str_detect(measure_name, "Incidence"),
      age_name %in% age_specific,
      year %in% c(1990, 2023),
      metric_name %in% c("Number", "Rate")
    ) %>%
    select(year, age_name, metric_name, val) %>%
    group_by(year, age_name, metric_name) %>%
    summarise(val = mean(val, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = metric_name, values_from = val) %>%
    mutate(Population = Number / (Rate / 100000))

  df_90 <- df_loc %>% filter(year == 1990)
  df_23 <- df_loc %>% filter(year == 2023)

  C_1990 <- sum(df_90$Number, na.rm = TRUE)
  C_2023 <- sum(df_23$Number, na.rm = TRUE)
  P_1990 <- sum(df_90$Population, na.rm = TRUE)
  P_2023 <- sum(df_23$Population, na.rm = TRUE)

  C_pop_only <- C_1990 * (P_2023 / P_1990)
  C_demog <- sum(df_23$Population * (df_90$Rate / 100000), na.rm = TRUE)

  tibble(
    Location = loc,
    Cases_1990 = C_1990,
    Population_Growth = C_pop_only - C_1990,
    Age_Structure = C_demog - C_pop_only,
    Epidemiological_Risk = C_2023 - C_demog,
    Cases_2023 = C_2023
  )
}

all_decomp_results <- map_dfr(target_locations, calc_decomposition)

df_table2 <- all_decomp_results %>%
  mutate(
    Total_Increase = Cases_2023 - Cases_1990,
    Pop_pct = Population_Growth / Total_Increase,
    Age_pct = Age_Structure / Total_Increase,
    Residual_pct = Epidemiological_Risk / Total_Increase
  ) %>%
  mutate(
    across(
      c(
        Cases_1990,
        Cases_2023,
        Total_Increase,
        Population_Growth,
        Age_Structure,
        Epidemiological_Risk
      ),
      ~ round(.x, 0)
    )
  ) %>%
  mutate(
    across(
      c(Pop_pct, Age_pct, Residual_pct),
      ~ percent(.x, accuracy = 0.1)
    )
  ) %>%
  rename(
    Residual_Epidemiological_Component = Epidemiological_Risk
  )

write_csv(df_table2, file.path(tables_dir, "Supplementary_Table_S2_Decomposition_results.csv"))

china_res <- calc_decomposition("China")

wf_data <- tibble(
  Category = factor(
    c(
      "1990\ncases",
      "Population\ngrowth",
      "Age\nstructure",
      "Residual\nepidemiological\ncomponent",
      "2023\ncases"
    ),
    levels = c(
      "1990\ncases",
      "Population\ngrowth",
      "Age\nstructure",
      "Residual\nepidemiological\ncomponent",
      "2023\ncases"
    )
  ),
  Value = c(
    china_res$Cases_1990,
    china_res$Population_Growth,
    china_res$Age_Structure,
    china_res$Epidemiological_Risk,
    china_res$Cases_2023
  ),
  Type = factor(c("Base", "Driver", "Driver", "Driver", "Total"), levels = c("Base", "Driver", "Total"))
) %>%
  mutate(
    End = cumsum(ifelse(Type == "Base", Value, ifelse(Type == "Total", 0, Value))),
    Start = End - Value
  )

wf_data$Start[5] <- 0
wf_data$End[5] <- china_res$Cases_2023
wf_data$Value[5] <- china_res$Cases_2023

p_waterfall <- ggplot(wf_data, aes(x = Category, fill = Type)) +
  geom_rect(aes(
    xmin = as.numeric(Category) - 0.4,
    xmax = as.numeric(Category) + 0.4,
    ymin = Start,
    ymax = End
  ), color = "black", linewidth = 0.4) +
  geom_text(aes(y = End + max(china_res$Cases_2023) * 0.05, label = comma(round(Value))),
            size = 4, fontface = "bold") +
  scale_fill_manual(values = c("Base" = color_global, "Driver" = color_china, "Total" = color_global)) +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()), expand = expansion(mult = c(0, 0.15))) +
  labs(title = "A. Sequential decomposition of increased cases in China", x = "", y = "Incident cases") +
  theme_pub(13) +
  theme(legend.position = "none")

# Explicitly divide each signed component by the same total increase before
# plotting. position = "fill" is not valid here because High SDI contains a
# negative residual component and ggplot normalizes positive and negative stacks
# separately.
df_bar <- all_decomp_results %>%
  mutate(Total_Increase = Cases_2023 - Cases_1990) %>%
  select(
    Location,
    Total_Increase,
    Population_Growth,
    Age_Structure,
    Epidemiological_Risk
  ) %>%
  pivot_longer(
    cols = c(
      Population_Growth,
      Age_Structure,
      Epidemiological_Risk
    ),
    names_to = "Component",
    values_to = "Absolute_Contribution"
  ) %>%
  mutate(
    Relative_Contribution = Absolute_Contribution / Total_Increase,
    Component = case_when(
      Component == "Population_Growth" ~ "Population growth",
      Component == "Age_Structure" ~ "Age structure",
      Component == "Epidemiological_Risk" ~
        "Residual epidemiological component",
      TRUE ~ Component
    ),
    Location = factor(Location, levels = rev(target_locations))
  )

p_bar <- ggplot(
  df_bar,
  aes(
    x = Location,
    y = Relative_Contribution,
    fill = Component
  )
) +
  geom_col(
    position = "stack",
    color = "black",
    width = 0.6,
    linewidth = 0.3
  ) +
  geom_hline(yintercept = 0, linewidth = 0.45, color = "black") +
  coord_flip() +
  scale_y_continuous(
    labels = percent_format(),
    breaks = c(-0.2, 0, 0.4, 0.8, 1.2),
    limits = c(-0.2, 1.2),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_fill_manual(values = c(
    "Population growth" = "#4DBBD5FF",
    "Age structure" = "#00A087FF",
    "Residual epidemiological component" = "#BC3C29FF"
  )) +
  labs(
    title = "B. Relative contributions of components",
    x = "",
    y = "Share of total increase",
    fill = ""
  ) +
  theme_pub(13) +
  theme(legend.position = "bottom")

df_cohort <- df_full %>%
  filter(
    location_name == "China",
    metric_name == "Rate",
    str_detect(measure_name, "Incidence"),
    age_name %in% age_specific
  ) %>%
  mutate(
    age_start = as.numeric(str_extract(age_name, "^[0-9]+")),
    age_mid = age_start + 2,
    birth_year = year - age_mid,
    cohort_group = case_when(
      birth_year >= 1960 & birth_year < 1970 ~ "1960s",
      birth_year >= 1970 & birth_year < 1980 ~ "1970s",
      birth_year >= 1980 & birth_year < 1990 ~ "1980s",
      birth_year >= 1990 ~ "1990 or later",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(cohort_group)) %>%
  group_by(cohort_group, age_name, age_mid) %>%
  summarise(mean_incidence_rate = mean(val, na.rm = TRUE), .groups = "drop") %>%
  arrange(cohort_group, age_mid)

write_csv(df_cohort, file.path(tables_dir, "Supplementary_Table_S7_Cohort_specific_rates.csv"))

df_label <- df_cohort %>%
  group_by(cohort_group) %>%
  filter(age_mid == max(age_mid)) %>%
  ungroup()

p_cohort <- ggplot(df_cohort, aes(x = age_name, y = mean_incidence_rate, color = cohort_group, group = cohort_group)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3.2, shape = 21, fill = "white", stroke = 1.1) +
  geom_text_repel(data = df_label, aes(label = cohort_group), nudge_x = 0.4, direction = "y", hjust = 0, fontface = "bold", size = 4.5) +
  scale_color_manual(values = c("1960s" = "#0072B5FF", "1970s" = "#4DBBD5FF", "1980s" = "#E18727FF", "1990 or later" = "#BC3C29FF")) +
  scale_x_discrete(expand = expansion(mult = c(0.05, 0.15))) +
  labs(title = "C. Descriptive cohort-specific incidence patterns in China", x = "Age group", y = "Incidence rate per 100,000") +
  theme_pub(13) +
  theme(legend.position = "none")

fig2_final <- wrap_elements(p_waterfall + p_bar + plot_layout(widths = c(1.15, 1))) /
  wrap_elements(p_cohort) +
  plot_layout(heights = c(1, 1.05))

save_plot_both(fig2_final, "Figure 2", width = 14, height = 12)

# ------------------------------------------------------------------------------
# 7. Figure 3: risk attribution and XGBoost-SHAP
# ------------------------------------------------------------------------------

message("▶ Building Figure 3...")

level2_risks <- c("Behavioral risks", "Dietary risks", "Metabolic risks")
level3_risks <- c("High alcohol use", "High body-mass index", "High fasting plasma glucose", "Low physical activity", "Tobacco")

df_macro <- df_data3 %>%
  filter(
    location_name %in% c("China", "Global"),
    metric_name == "Percent",
    str_detect(measure_name, "DALY"),
    age_name %in% age_specific,
    rei_name %in% level2_risks
  ) %>%
  group_by(year, location_name, rei_name) %>%
  summarise(val = mean(val, na.rm = TRUE), .groups = "drop")

if (nrow(df_macro) == 0L) {
  stop("No DALY-specific broad PAF rows were found for Figure 3A.")
}

p_macro <- ggplot(df_macro, aes(x = year, y = val, color = rei_name, linetype = location_name)) +
  geom_line(linewidth = 1.1) +
  scale_color_manual(values = c("Behavioral risks" = "#00A087FF", "Dietary risks" = "#4DBBD5FF", "Metabolic risks" = color_china)) +
  scale_linetype_manual(values = c("China" = "solid", "Global" = "dashed")) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "A. Broad risk-attribution trends", x = "", y = "Average PAF", color = "Risk category", linetype = "Location") +
  theme_pub(12) +
  theme(legend.position = "bottom")

risk_name_clean <- function(x) {
  case_when(
    str_detect(x, "glucose") ~ "High fasting plasma glucose",
    str_detect(x, "body-mass") ~ "High BMI",
    str_detect(x, "alcohol") ~ "High alcohol use",
    str_detect(x, "Tobacco|smoking") ~ "Tobacco use",
    str_detect(x, "physical activity") ~ "Low physical activity",
    TRUE ~ x
  )
}

df_micro <- df_data3 %>%
  filter(
    location_name %in% c("China", "Global"),
    metric_name == "Percent",
    str_detect(measure_name, "DALY"),
    age_name %in% age_specific,
    rei_name %in% level3_risks
  ) %>%
  mutate(
    Risk_Factor = risk_name_clean(rei_name),
    # Preserve signed GBD PAF estimates. Negative values are not missing and
    # must not be silently truncated to zero in descriptive displays.
    age_name = factor(age_name, levels = age_specific)
  ) %>%
  group_by(location_name, year, age_name, Risk_Factor) %>%
  summarise(val = mean(val, na.rm = TRUE), .groups = "drop")

if (nrow(df_micro) == 0L) {
  stop("No DALY-specific risk PAF rows were found for Figure 3C-D.")
}

heat_limit <- max(abs(df_micro$val), na.rm = TRUE)

plot_heatmap <- function(loc, title_txt, show_x = FALSE) {
  p <- ggplot(
    df_micro %>% filter(location_name == loc),
    aes(x = year, y = age_name, fill = val)
  ) +
    geom_tile(color = NA) +
    facet_wrap(~ Risk_Factor, ncol = 5) +
    scale_fill_gradient2(
      low = "#3B4CC0", mid = "white", high = "#B40426", midpoint = 0,
      name = "Signed PAF", labels = percent_format(accuracy = 1),
      limits = c(-heat_limit, heat_limit), oob = scales::squish
    ) +
    scale_x_continuous(
      breaks = seq(1990, 2020, by = 10),
      expand = c(0, 0)
    ) +
    labs(
      title = title_txt,
      x = ifelse(show_x, "Year", ""),
      y = "Age group"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      strip.text = element_text(face = "bold", size = 9),
      panel.border = element_rect(
        color = "black",
        fill = NA,
        linewidth = 0.5
      ),
      plot.title = element_text(face = "bold", size = 12),
      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        color = "black"
      ),
      axis.text.y = element_text(color = "black")
    )

  if (identical(loc, "China")) {
    bmi_annotation <- tibble::tibble(
      Risk_Factor = "High BMI",
      year = 2006.5,
      age_name = factor("35-39 years", levels = age_specific),
      label = "PAF constant at 0"
    )

    p <- p +
      geom_text(
        data = bmi_annotation,
        aes(x = year, y = age_name, label = label),
        inherit.aes = FALSE,
        color = "grey35",
        fontface = "italic",
        size = 3.2
      )
  }

  p
}

p_micro_china <- plot_heatmap("China", "C. Specific risk-attribution heatmap: China", FALSE)
p_micro_global <- plot_heatmap("Global", "D. Specific risk-attribution heatmap: Global", TRUE)

df_target_y <- df_full %>%
  filter(location_name == "China", metric_name == "Number", str_detect(measure_name, "DALY"), age_name %in% age_specific) %>%
  group_by(year, age_name) %>%
  summarise(Burden_number = sum(val, na.rm = TRUE), .groups = "drop")

feature_cols <- c("High_alcohol_use", "High_BMI", "High_fasting_plasma_glucose", "Low_physical_activity", "Tobacco_use")

df_features_x <- df_data3 %>%
  filter(
    location_name == "China",
    metric_name == "Percent",
    str_detect(measure_name, "DALY"),
    age_name %in% age_specific,
    rei_name %in% level3_risks
  ) %>%
  mutate(
    Feature = case_when(
      str_detect(rei_name, "glucose") ~ "High_fasting_plasma_glucose",
      str_detect(rei_name, "body-mass") ~ "High_BMI",
      str_detect(rei_name, "alcohol") ~ "High_alcohol_use",
      str_detect(rei_name, "Tobacco|smoking") ~ "Tobacco_use",
      str_detect(rei_name, "physical activity") ~ "Low_physical_activity",
      TRUE ~ rei_name
    )
  ) %>%
  group_by(year, age_name, Feature) %>%
  summarise(val = mean(val, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Feature, values_from = val)

# Preserve the joined analytical matrix before any missing-data handling.
df_ml_raw <- df_features_x %>%
  left_join(df_target_y, by = c("year", "age_name"))

# ==============================================================================
# BEGIN EMBEDDED MODULE: 01_data_integrity_audit.R
# ==============================================================================

# ==============================================================================
# Data integrity audit for the XGBoost analytical matrix
# ==============================================================================

message("▶ Data integrity audit for the XGBoost analytical matrix...")

required_objects <- c(
  "df_data3", "df_features_x", "df_target_y", "df_ml_raw",
  "feature_cols", "level3_risks", "age_specific", "analysis_tables_dir"
)
missing_objects <- required_objects[!vapply(required_objects, exists, logical(1), inherits = TRUE)]
if (length(missing_objects) > 0) {
  stop("Module 01 cannot run. Missing objects: ", paste(missing_objects, collapse = ", "))
}

dir.create(analysis_tables_dir, showWarnings = FALSE, recursive = TRUE)

risk_label_for_audit <- function(x) {
  dplyr::case_when(
    stringr::str_detect(x, "glucose") ~ "High fasting plasma glucose",
    stringr::str_detect(x, "body-mass") ~ "High BMI",
    stringr::str_detect(x, "alcohol") ~ "High alcohol use",
    stringr::str_detect(x, "Tobacco|smoking") ~ "Tobacco use",
    stringr::str_detect(x, "physical activity") ~ "Low physical activity",
    TRUE ~ x
  )
}

# 1. Audit raw long-format GBD PAF values before any negative-value truncation,
#    averaging, pivoting, joining, or imputation.
risk_long_raw <- df_data3 %>%
  dplyr::filter(
    location_name == "China",
    metric_name == "Percent",
    stringr::str_detect(measure_name, "DALY"),
    age_name %in% age_specific,
    rei_name %in% level3_risks,
    year >= 1990,
    year <= 2023
  ) %>%
  dplyr::mutate(Risk_Factor = risk_label_for_audit(rei_name))

risk_long_summary <- risk_long_raw %>%
  dplyr::group_by(Risk_Factor) %>%
  dplyr::summarise(
    n_rows = dplyr::n(),
    n_missing = sum(is.na(val) | is.nan(val)),
    pct_missing = 100 * n_missing / n_rows,
    n_observed = sum(!is.na(val) & !is.nan(val)),
    n_zero = sum(val == 0, na.rm = TRUE),
    pct_zero_among_observed = dplyr::if_else(
      n_observed > 0,
      100 * n_zero / n_observed,
      NA_real_
    ),
    n_negative = sum(val < 0, na.rm = TRUE),
    min_observed = ifelse(n_observed > 0, min(val, na.rm = TRUE), NA_real_),
    median_observed = ifelse(n_observed > 0, median(val, na.rm = TRUE), NA_real_),
    max_observed = ifelse(n_observed > 0, max(val, na.rm = TRUE), NA_real_),
    .groups = "drop"
  )

# 2. Verify whether each year-age-risk combination is unique before pivoting.
risk_duplicate_keys <- risk_long_raw %>%
  dplyr::count(year, age_name, Risk_Factor, name = "n_records") %>%
  dplyr::filter(n_records != 1) %>%
  dplyr::arrange(Risk_Factor, age_name, year)

# 3. Audit the wide feature matrix used for XGBoost.
feature_summary <- purrr::map_dfr(feature_cols, function(v) {
  x <- df_features_x[[v]]
  tibble::tibble(
    Feature = v,
    n_rows = length(x),
    n_missing = sum(is.na(x) | is.nan(x)),
    pct_missing = 100 * n_missing / n_rows,
    n_observed = sum(!is.na(x) & !is.nan(x)),
    n_zero = sum(x == 0, na.rm = TRUE),
    pct_zero_among_observed = dplyr::if_else(
      n_observed > 0,
      100 * n_zero / n_observed,
      NA_real_
    ),
    n_negative = sum(x < 0, na.rm = TRUE),
    min_observed = ifelse(n_observed > 0, min(x, na.rm = TRUE), NA_real_),
    median_observed = ifelse(n_observed > 0, median(x, na.rm = TRUE), NA_real_),
    max_observed = ifelse(n_observed > 0, max(x, na.rm = TRUE), NA_real_)
  )
})

expected_grid <- tidyr::expand_grid(
  year = 1990:2023,
  age_name = age_specific
)

feature_grid_check <- expected_grid %>%
  dplyr::left_join(df_features_x, by = c("year", "age_name")) %>%
  dplyr::mutate(
    n_features_missing = rowSums(
      dplyr::across(dplyr::all_of(feature_cols), ~ is.na(.x) | is.nan(.x))
    )
  )

missing_rows <- feature_grid_check %>%
  dplyr::filter(n_features_missing > 0) %>%
  dplyr::arrange(year, factor(age_name, levels = age_specific))

missing_by_year <- feature_grid_check %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(feature_cols),
      ~ sum(is.na(.x) | is.nan(.x)),
      .names = "{.col}_n_missing"
    ),
    rows_with_any_missing = sum(n_features_missing > 0),
    .groups = "drop"
  )

missing_by_age <- feature_grid_check %>%
  dplyr::group_by(age_name) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(feature_cols),
      ~ sum(is.na(.x) | is.nan(.x)),
      .names = "{.col}_n_missing"
    ),
    rows_with_any_missing = sum(n_features_missing > 0),
    .groups = "drop"
  )

# 4. Audit target completeness and the final join before imputation.
target_summary <- df_target_y %>%
  dplyr::summarise(
    n_rows = dplyr::n(),
    n_missing_target = sum(is.na(Burden_number) | is.nan(Burden_number)),
    pct_missing_target = 100 * n_missing_target / n_rows,
    n_zero_target = sum(Burden_number == 0, na.rm = TRUE),
    min_target = min(Burden_number, na.rm = TRUE),
    median_target = median(Burden_number, na.rm = TRUE),
    max_target = max(Burden_number, na.rm = TRUE)
  )

join_summary <- tibble::tibble(
  Metric = c(
    "Expected year-age rows",
    "Rows in df_features_x",
    "Rows in df_target_y",
    "Rows after left join",
    "Rows with any missing feature",
    "Rows with missing target",
    "Rows complete for all five features and target"
  ),
  Value = c(
    nrow(expected_grid),
    nrow(df_features_x),
    nrow(df_target_y),
    nrow(df_ml_raw),
    sum(rowSums(is.na(df_ml_raw[, feature_cols, drop = FALSE])) > 0),
    sum(is.na(df_ml_raw$Burden_number) | is.nan(df_ml_raw$Burden_number)),
    sum(
      rowSums(is.na(df_ml_raw[, feature_cols, drop = FALSE])) == 0 &
        !is.na(df_ml_raw$Burden_number) & !is.nan(df_ml_raw$Burden_number)
    )
  )
)

zero_rows <- feature_grid_check %>%
  dplyr::filter(
    dplyr::if_any(dplyr::all_of(feature_cols), ~ !is.na(.x) & .x == 0)
  ) %>%
  dplyr::mutate(
    n_features_equal_zero = rowSums(
      dplyr::across(dplyr::all_of(feature_cols), ~ !is.na(.x) & .x == 0)
    )
  ) %>%
  dplyr::arrange(year, factor(age_name, levels = age_specific))

# 5. Export a single audit workbook plus machine-readable CSV files.
audit_book <- list(
  Risk_long_summary = risk_long_summary,
  Feature_summary = feature_summary,
  Join_summary = join_summary,
  Target_summary = target_summary,
  Missing_by_year = missing_by_year,
  Missing_by_age = missing_by_age,
  Missing_rows = missing_rows,
  Zero_rows = zero_rows,
  Duplicate_keys = risk_duplicate_keys
)

openxlsx::write.xlsx(
  audit_book,
  file = file.path(analysis_tables_dir, "DataIntegrity_missing_data_audit.xlsx"),
  overwrite = TRUE
)

readr::write_csv(
  feature_summary,
  file.path(analysis_tables_dir, "DataIntegrity_feature_missingness_summary.csv")
)
readr::write_csv(
  missing_rows,
  file.path(analysis_tables_dir, "DataIntegrity_rows_with_missing_features.csv")
)
readr::write_csv(
  zero_rows,
  file.path(analysis_tables_dir, "DataIntegrity_rows_with_observed_zeros.csv")
)

capture.output(
  list(
    risk_long_summary = risk_long_summary,
    feature_summary = feature_summary,
    join_summary = join_summary,
    target_summary = target_summary
  ),
  file = file.path(analysis_tables_dir, "DataIntegrity_missing_data_audit_console.txt")
)

message(
  "  Missing-data audit exported to: ",
  file.path(analysis_tables_dir, "DataIntegrity_missing_data_audit.xlsx")
)

# ==============================================================================
# END EMBEDDED MODULE: 01_data_integrity_audit.R
# ==============================================================================


# ==============================================================================
# BEGIN EMBEDDED MODULE: 02_bmi_source_validation.R
# ==============================================================================

# ==============================================================================
# High-BMI PAF source-data validation
# ==============================================================================

message("▶ High-BMI PAF source-data validation...")

required_objects <- c(
  "df_data3", "data3_files", "age_specific", "level3_risks",
  "analysis_tables_dir"
)
missing_objects <- required_objects[!vapply(required_objects, exists, logical(1), inherits = TRUE)]
if (length(missing_objects) > 0) {
  stop("Module 02 cannot run. Missing objects: ", paste(missing_objects, collapse = ", "))
}

is_bmi_label <- function(x) {
  stringr::str_detect(
    x,
    stringr::regex("body[- ]?mass|high bmi|body mass index|\\bbmi\\b", ignore_case = TRUE)
  )
}

bmi_inventory <- df_data3 %>%
  dplyr::filter(is_bmi_label(rei_name)) %>%
  dplyr::select(dplyr::any_of(c(
    "rei_name", "cause_name", "sex_name", "measure_name", "metric_name",
    "location_name", "age_name"
  ))) %>%
  dplyr::distinct()

# Exact rows used in the China 25-49-year DALY-PAF analysis.
bmi_selected_raw <- df_data3 %>%
  dplyr::filter(
    location_name == "China",
    metric_name == "Percent",
    stringr::str_detect(measure_name, "DALY"),
    age_name %in% age_specific,
    is_bmi_label(rei_name),
    year >= 1990,
    year <= 2023
  ) %>%
  dplyr::arrange(year, factor(age_name, levels = age_specific))

value_cols <- intersect(c("val", "lower", "upper"), names(bmi_selected_raw))

summarise_numeric_values <- function(data, group_vars = character()) {
  if (length(group_vars) > 0) {
    data <- data %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars)))
  }

  data %>%
    dplyr::summarise(
      n_rows = dplyr::n(),
      dplyr::across(
        dplyr::all_of(value_cols),
        list(
          n_missing = ~ sum(is.na(.x) | is.nan(.x)),
          n_zero = ~ sum(.x == 0, na.rm = TRUE),
          n_negative = ~ sum(.x < 0, na.rm = TRUE),
          min = ~ ifelse(all(is.na(.x)), NA_real_, min(.x, na.rm = TRUE)),
          median = ~ ifelse(all(is.na(.x)), NA_real_, stats::median(.x, na.rm = TRUE)),
          max = ~ ifelse(all(is.na(.x)), NA_real_, max(.x, na.rm = TRUE))
        ),
        .names = "{.col}_{.fn}"
      ),
      .groups = "drop"
    )
}

bmi_selected_summary <- summarise_numeric_values(bmi_selected_raw)
bmi_selected_by_age <- summarise_numeric_values(bmi_selected_raw, "age_name")
bmi_selected_by_year <- summarise_numeric_values(bmi_selected_raw, "year")

# Examine all available ages for China, retaining the same DALY/Percent context.
bmi_china_all_ages <- df_data3 %>%
  dplyr::filter(
    location_name == "China",
    metric_name == "Percent",
    stringr::str_detect(measure_name, "DALY"),
    is_bmi_label(rei_name)
  )

# Apply cause/sex restrictions only when those columns are present.
if ("cause_name" %in% names(bmi_china_all_ages)) {
  bmi_china_all_ages <- bmi_china_all_ages %>%
    dplyr::filter(cause_name == "Breast cancer")
}
if ("sex_name" %in% names(bmi_china_all_ages)) {
  bmi_china_all_ages <- bmi_china_all_ages %>%
    dplyr::filter(sex_name == "Female")
}

bmi_china_all_ages_summary <- summarise_numeric_values(
  bmi_china_all_ages,
  intersect(c("age_name", "rei_name", "measure_name", "metric_name"), names(bmi_china_all_ages))
) %>%
  dplyr::arrange(age_name)

# Compare China with all locations available in the same extract.
bmi_by_location <- df_data3 %>%
  dplyr::filter(
    metric_name == "Percent",
    stringr::str_detect(measure_name, "DALY"),
    age_name %in% age_specific,
    is_bmi_label(rei_name)
  )
if ("cause_name" %in% names(bmi_by_location)) {
  bmi_by_location <- bmi_by_location %>% dplyr::filter(cause_name == "Breast cancer")
}
if ("sex_name" %in% names(bmi_by_location)) {
  bmi_by_location <- bmi_by_location %>% dplyr::filter(sex_name == "Female")
}
bmi_by_location_summary <- summarise_numeric_values(bmi_by_location, "location_name") %>%
  dplyr::arrange(location_name)

# Check exact key uniqueness in the selected analytical rows.
bmi_duplicate_keys <- bmi_selected_raw %>%
  dplyr::count(year, age_name, rei_name, name = "n_records") %>%
  dplyr::filter(n_records != 1)

# Record source filenames, sizes, and MD5 hashes.
source_manifest <- tibble::tibble(
  file = basename(data3_files),
  full_path = normalizePath(data3_files, winslash = "/", mustWork = FALSE),
  size_bytes = file.info(data3_files)$size,
  modified_time = as.character(file.info(data3_files)$mtime),
  md5 = unname(tools::md5sum(data3_files))
)

validation_checks <- tibble::tibble(
  Check = c(
    "Selected China 25-49 BMI rows",
    "Expected China 25-49 BMI rows",
    "Selected rows with missing val",
    "Selected rows with val equal to zero",
    "Selected rows with negative val",
    "Selected duplicate year-age keys",
    "Distinct selected rei_name values"
  ),
  Value = c(
    nrow(bmi_selected_raw),
    length(1990:2023) * length(age_specific),
    sum(is.na(bmi_selected_raw$val) | is.nan(bmi_selected_raw$val)),
    sum(bmi_selected_raw$val == 0, na.rm = TRUE),
    sum(bmi_selected_raw$val < 0, na.rm = TRUE),
    nrow(bmi_duplicate_keys),
    dplyr::n_distinct(bmi_selected_raw$rei_name)
  )
)

validation_book <- list(
  Validation_checks = validation_checks,
  Selected_summary = bmi_selected_summary,
  Selected_by_age = bmi_selected_by_age,
  Selected_by_year = bmi_selected_by_year,
  Selected_raw_rows = bmi_selected_raw,
  BMI_label_inventory = bmi_inventory,
  China_all_ages = bmi_china_all_ages_summary,
  By_location_25_49 = bmi_by_location_summary,
  Duplicate_keys = bmi_duplicate_keys,
  Source_manifest = source_manifest
)

openxlsx::write.xlsx(
  validation_book,
  file = file.path(analysis_tables_dir, "BMISource_high_BMI_source_validation.xlsx"),
  overwrite = TRUE
)

readr::write_csv(
  validation_checks,
  file.path(analysis_tables_dir, "BMISource_high_BMI_validation_checks.csv")
)
readr::write_csv(
  bmi_selected_raw,
  file.path(analysis_tables_dir, "BMISource_high_BMI_selected_raw_rows.csv")
)

message(
  "  High-BMI source validation exported to: ",
  file.path(analysis_tables_dir, "BMISource_high_BMI_source_validation.xlsx")
)

# ==============================================================================
# END EMBEDDED MODULE: 02_bmi_source_validation.R
# ==============================================================================


# All 170 year-age observations contain complete feature and target data.
# No automatic imputation is applied; a future incomplete extract stops the pipeline.
if (
  any(is.na(df_ml_raw[, feature_cols, drop = FALSE])) ||
  any(is.nan(as.matrix(df_ml_raw[, feature_cols, drop = FALSE]))) ||
  any(is.na(df_ml_raw$Burden_number)) ||
  any(is.nan(df_ml_raw$Burden_number))
) {
  stop(
    "Unexpected missing values detected after the data-integrity audit. ",
    "Do not impute automatically; inspect DataIntegrity_missing_data_audit.xlsx."
  )
}

df_ml <- df_ml_raw %>%
  tidyr::drop_na(dplyr::any_of(c(feature_cols, "Burden_number")))

# Identify predictors that cannot contribute to a tree model because they have
# no variation. In the current extract, High_BMI is zero in every one of the
# 170 year-age observations and is therefore excluded from SHAP ranking.
feature_variance <- purrr::map_dfr(feature_cols, function(v) {
  x <- df_ml[[v]]
  tibble::tibble(
    Feature = v,
    n_observed = sum(!is.na(x)),
    n_unique = dplyr::n_distinct(x, na.rm = TRUE),
    Mean = mean(x, na.rm = TRUE),
    SD = stats::sd(x, na.rm = TRUE),
    Min = min(x, na.rm = TRUE),
    Max = max(x, na.rm = TRUE),
    Zero_variance = dplyr::n_distinct(x, na.rm = TRUE) <= 1
  )
})

model_feature_cols <- feature_variance %>%
  dplyr::filter(!Zero_variance) %>%
  dplyr::pull(Feature)

if (length(model_feature_cols) < 2) {
  stop("Fewer than two non-constant risk features remain for XGBoost.")
}

readr::write_csv(
  feature_variance,
  file.path(analysis_tables_dir, "BMISource_model_feature_variance.csv")
)

X_matrix <- as.matrix(as.data.frame(df_ml)[, model_feature_cols, drop = FALSE])
Y_target <- as.numeric(df_ml$Burden_number) / 1000  # for graphical readability

dtrain <- xgb.DMatrix(
  data = X_matrix,
  label = Y_target,
  missing = NA_real_
)
set.seed(2026)

xgb_model <- xgb.train(
  params = list(max_depth = 4, eta = 0.05, objective = "reg:squarederror", eval_metric = "rmse"),
  data = dtrain,
  nrounds = 150,
  verbose = 0
)

shp <- shapviz(xgb_model, X_pred = X_matrix)

p_shap <- sv_importance(shp, kind = "beeswarm") +
  scale_color_viridis(option = "C", name = "Feature value\n(PAF)") +
  scale_y_discrete(labels = function(x) str_to_sentence(gsub("_", " ", x))) +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "B. SHAP summary for exploratory ranking (non-constant features)", x = "SHAP value: contribution to predicted DALYs in thousands") +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold", color = "black"),
    axis.text.x = element_text(color = "black"),
    legend.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank()
  )

shap_contrib <- predict(xgb_model, X_matrix, predcontrib = TRUE)

# xgboost appends a baseline contribution column. Depending on package/version,
# this column may be named "BIAS" or "(Intercept)". Retain only the prespecified
# model features so the baseline is never ranked as a risk factor.
shap_feature_names <- intersect(colnames(shap_contrib), model_feature_cols)
shap_values <- shap_contrib[, shap_feature_names, drop = FALSE]

if (!setequal(colnames(shap_values), model_feature_cols)) {
  stop(
    "Unexpected SHAP contribution columns. Expected: ",
    paste(model_feature_cols, collapse = ", "),
    "; obtained: ", paste(colnames(shap_values), collapse = ", ")
  )
}

risk_factor_labels <- tibble::tibble(
  Feature = feature_cols,
  Risk_Factor = dplyr::case_when(
    Feature == "High_fasting_plasma_glucose" ~ "High fasting plasma glucose",
    Feature == "High_BMI" ~ "High BMI",
    Feature == "High_alcohol_use" ~ "High alcohol use",
    Feature == "Low_physical_activity" ~ "Low physical activity",
    Feature == "Tobacco_use" ~ "Tobacco use",
    TRUE ~ Feature
  )
)

df_shap_importance_included <- tibble::tibble(
  Feature = colnames(shap_values),
  Mean_abs_SHAP = colMeans(abs(shap_values), na.rm = TRUE)
) %>%
  dplyr::left_join(risk_factor_labels, by = "Feature") %>%
  dplyr::arrange(dplyr::desc(Mean_abs_SHAP)) %>%
  dplyr::mutate(`SHAP rank` = dplyr::row_number())

# Preserve all five prespecified GBD risk factors in the exported table, while
# distinguishing predictors that were not statistically rankable because they
# were constant in the source data.
df_shap_importance <- risk_factor_labels %>%
  dplyr::left_join(df_shap_importance_included, by = c("Feature", "Risk_Factor")) %>%
  dplyr::left_join(feature_variance, by = "Feature") %>%
  dplyr::mutate(
    Model_status = dplyr::if_else(
      Feature %in% model_feature_cols,
      "Included in XGBoost-SHAP",
      "Excluded from XGBoost-SHAP: zero variance"
    )
  ) %>%
  dplyr::arrange(is.na(`SHAP rank`), `SHAP rank`, Feature)

readr::write_csv(
  df_shap_importance,
  file.path(tables_dir, "Supplementary_Table_S6_SHAP_importance.csv")
)

df_paf_2023 <- df_micro %>%
  filter(location_name == "China", year == 2023) %>%
  group_by(Risk_Factor, age_name) %>%
  summarise(PAF = mean(val, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = age_name, values_from = PAF) %>%
  left_join(
    df_micro %>%
      filter(location_name == "China", year == 2023) %>%
      group_by(Risk_Factor) %>%
      summarise(`Overall PAF (25-49y)` = mean(val, na.rm = TRUE), .groups = "drop"),
    by = "Risk_Factor"
  ) %>%
  left_join(df_shap_importance %>% select(Risk_Factor, `SHAP rank`, Mean_abs_SHAP), by = "Risk_Factor") %>%
  arrange(`SHAP rank`) %>%
  mutate(
    across(where(is.numeric), ~ round(.x, 4)),
    Mean_abs_SHAP = round(Mean_abs_SHAP, 1)
  )

write_csv(df_paf_2023, file.path(tables_dir, "Supplementary_Table_S3_PAF_SHAP_ranking.csv"))

# Assess temporal stability with forward validation and grouped block bootstrap.
# ==============================================================================
# BEGIN EMBEDDED MODULE: 03_shap_temporal_stability.R
# ==============================================================================

# ==============================================================================
# Temporal stability of exploratory SHAP rankings
# ==============================================================================

required_objects_step3 <- c(
  "df_ml", "model_feature_cols", "risk_factor_labels",
  "df_shap_importance_included", "analysis_tables_dir",
  "analysis_figures_dir", "sensitivity_results_dir"
)
missing_objects_step3 <- required_objects_step3[
  !vapply(required_objects_step3, exists, logical(1), inherits = TRUE)
]
if (length(missing_objects_step3) > 0) {
  stop(
    "03_shap_temporal_stability.R is missing required objects: ",
    paste(missing_objects_step3, collapse = ", ")
  )
}

message("▶ Temporal stability of exploratory SHAP rankings...")

temporal_seed <- 20260726L
temporal_n_boot <- getOption("analysis.temporal_boot", 200L)
temporal_block_length <- 5L

# Human-readable labels for the four non-constant model features.
temporal_labels <- risk_factor_labels %>%
  dplyr::filter(Feature %in% model_feature_cols)

# Keep a stable row order for all sensitivity analyses.
temporal_data <- df_ml %>%
  dplyr::arrange(year, factor(age_name, levels = age_specific))

temporal_fit_model <- function(train_df, seed_value) {
  x_train <- as.matrix(as.data.frame(train_df)[, model_feature_cols, drop = FALSE])
  y_train <- as.numeric(train_df$Burden_number) / 1000
  dtrain_local <- xgboost::xgb.DMatrix(
    data = x_train,
    label = y_train,
    missing = NA_real_
  )
  set.seed(seed_value)
  xgboost::xgb.train(
    params = list(
      max_depth = 4,
      eta = 0.05,
      objective = "reg:squarederror",
      eval_metric = "rmse"
    ),
    data = dtrain_local,
    nrounds = 150,
    verbose = 0
  )
}

temporal_importance <- function(model, eval_df, analysis_id, analysis_type) {
  x_eval <- as.matrix(as.data.frame(eval_df)[, model_feature_cols, drop = FALSE])
  contrib <- predict(model, x_eval, predcontrib = TRUE)

  # xgboost includes a baseline contribution column whose name varies by
  # package/version (commonly "BIAS" or "(Intercept)"). Restrict the matrix
  # explicitly to the actual model predictors before normalization and ranking.
  shap_feature_names <- intersect(colnames(contrib), model_feature_cols)
  contrib <- contrib[, shap_feature_names, drop = FALSE]

  if (!setequal(colnames(contrib), model_feature_cols)) {
    stop(
      "Unexpected SHAP contribution columns in temporal-stability analysis. Expected: ",
      paste(model_feature_cols, collapse = ", "),
      "; obtained: ", paste(colnames(contrib), collapse = ", ")
    )
  }

  imp <- colMeans(abs(contrib), na.rm = TRUE)
  tibble::tibble(
    Analysis_type = analysis_type,
    Analysis_ID = analysis_id,
    Feature = names(imp),
    Mean_abs_SHAP = as.numeric(imp)
  ) %>%
    dplyr::mutate(
      Normalized_importance = Mean_abs_SHAP / sum(Mean_abs_SHAP),
      Rank = rank(-Mean_abs_SHAP, ties.method = "min")
    ) %>%
    dplyr::left_join(temporal_labels, by = "Feature")
}

temporal_metrics <- function(observed, predicted) {
  rmse <- sqrt(mean((observed - predicted)^2, na.rm = TRUE))
  mae <- mean(abs(observed - predicted), na.rm = TRUE)
  denom <- sum((observed - mean(observed, na.rm = TRUE))^2, na.rm = TRUE)
  r2 <- if (is.finite(denom) && denom > 0) {
    1 - sum((observed - predicted)^2, na.rm = TRUE) / denom
  } else {
    NA_real_
  }
  rho <- suppressWarnings(stats::cor(observed, predicted, method = "spearman", use = "complete.obs"))
  c(RMSE = rmse, MAE = mae, R2 = r2, Spearman_rho = rho)
}

# ------------------------------------------------------------------------------
# A. Forward-chaining temporal validation
# ------------------------------------------------------------------------------

temporal_folds <- tibble::tribble(
  ~Fold,    ~Train_start, ~Train_end, ~Test_start, ~Test_end,
  "Fold 1", 1990L,        2005L,      2006L,       2011L,
  "Fold 2", 1990L,        2011L,      2012L,       2017L,
  "Fold 3", 1990L,        2017L,      2018L,       2023L
)

temporal_fold_perf <- list()
temporal_fold_imp <- list()
temporal_fold_pred <- list()

for (i in seq_len(nrow(temporal_folds))) {
  fold_def <- temporal_folds[i, ]
  train_df <- temporal_data %>%
    dplyr::filter(year >= fold_def$Train_start, year <= fold_def$Train_end)
  test_df <- temporal_data %>%
    dplyr::filter(year >= fold_def$Test_start, year <= fold_def$Test_end)

  model_i <- temporal_fit_model(train_df, temporal_seed + i)
  x_test <- as.matrix(as.data.frame(test_df)[, model_feature_cols, drop = FALSE])
  pred_i <- as.numeric(predict(model_i, x_test))
  obs_i <- as.numeric(test_df$Burden_number) / 1000
  metrics_i <- temporal_metrics(obs_i, pred_i)

  temporal_fold_perf[[i]] <- tibble::tibble(
    Fold = fold_def$Fold,
    Train_start = fold_def$Train_start,
    Train_end = fold_def$Train_end,
    Test_start = fold_def$Test_start,
    Test_end = fold_def$Test_end,
    N_train = nrow(train_df),
    N_test = nrow(test_df),
    RMSE_thousand_DALYs = unname(metrics_i["RMSE"]),
    MAE_thousand_DALYs = unname(metrics_i["MAE"]),
    R2 = unname(metrics_i["R2"]),
    Spearman_rho = unname(metrics_i["Spearman_rho"])
  )

  temporal_fold_imp[[i]] <- temporal_importance(
    model_i, test_df, fold_def$Fold, "Forward temporal validation"
  )

  temporal_fold_pred[[i]] <- test_df %>%
    dplyr::transmute(
      Fold = fold_def$Fold,
      year,
      age_name,
      Observed_thousand_DALYs = obs_i,
      Predicted_thousand_DALYs = pred_i,
      Residual = obs_i - pred_i
    )
}

temporal_fold_perf <- dplyr::bind_rows(temporal_fold_perf)
temporal_fold_imp <- dplyr::bind_rows(temporal_fold_imp)
temporal_fold_pred <- dplyr::bind_rows(temporal_fold_pred)

# Compare each temporal-fold ranking with the full-panel exploratory ranking.
temporal_full_rank <- df_shap_importance_included %>%
  dplyr::select(Feature, Full_panel_rank = `SHAP rank`)

temporal_fold_concordance <- temporal_fold_imp %>%
  dplyr::left_join(temporal_full_rank, by = "Feature") %>%
  dplyr::group_by(Analysis_ID) %>%
  dplyr::summarise(
    Kendall_tau_vs_full = suppressWarnings(
      stats::cor(Rank, Full_panel_rank, method = "kendall", use = "complete.obs")
    ),
    .groups = "drop"
  ) %>%
  dplyr::rename(Fold = Analysis_ID)

# ------------------------------------------------------------------------------
# B. Grouped non-overlapping calendar-block bootstrap by calendar year
# ------------------------------------------------------------------------------

min_year_step3 <- min(temporal_data$year, na.rm = TRUE)
temporal_data <- temporal_data %>%
  dplyr::mutate(
    Time_block = floor((year - min_year_step3) / temporal_block_length) + 1L
  )
temporal_blocks <- sort(unique(temporal_data$Time_block))

# Reporting-only audit table: six complete 5-year blocks and one final 4-year block.
temporal_block_definitions <- temporal_data %>%
  dplyr::distinct(Time_block, year) %>%
  dplyr::group_by(Time_block) %>%
  dplyr::summarise(
    Block_start = min(year),
    Block_end = max(year),
    N_years = dplyr::n_distinct(year),
    N_year_age_rows = dplyr::n_distinct(year) * length(age_specific),
    .groups = "drop"
  )

temporal_boot_imp <- vector("list", temporal_n_boot)
temporal_boot_draws <- vector("list", temporal_n_boot)
set.seed(temporal_seed)

# Fixed evaluation matrix: all observed year-age combinations. Each bootstrap
# model is therefore compared on the same support.
for (b in seq_len(temporal_n_boot)) {
  sampled_blocks <- sample(temporal_blocks, length(temporal_blocks), replace = TRUE)
  boot_rows <- unlist(
    lapply(sampled_blocks, function(block_id) which(temporal_data$Time_block == block_id)),
    use.names = FALSE
  )
  boot_train <- temporal_data[boot_rows, , drop = FALSE]

  temporal_boot_draws[[b]] <- tibble::tibble(
    Bootstrap_ID = b,
    Draw_order = seq_along(sampled_blocks),
    Time_block = sampled_blocks
  ) %>%
    dplyr::left_join(temporal_block_definitions, by = "Time_block") %>%
    dplyr::mutate(
      N_training_rows_in_replicate = nrow(boot_train),
      N_unique_years_in_replicate = dplyr::n_distinct(boot_train$year)
    )

  model_b <- tryCatch(
    temporal_fit_model(boot_train, temporal_seed + 1000L + b),
    error = function(e) NULL
  )

  if (!is.null(model_b)) {
    temporal_boot_imp[[b]] <- temporal_importance(
      model_b,
      temporal_data,
      paste0("Bootstrap ", b),
      "Grouped calendar-block bootstrap"
    ) %>%
      dplyr::mutate(Bootstrap_ID = b)
  }

  if (b %% 25L == 0L || b == temporal_n_boot) {
    message("  Temporal bootstrap completed: ", b, "/", temporal_n_boot)
  }
}

temporal_boot_imp <- dplyr::bind_rows(temporal_boot_imp)
temporal_boot_draws <- dplyr::bind_rows(temporal_boot_draws)

if (nrow(temporal_boot_imp) == 0) {
  stop("All temporal block-bootstrap model fits failed.")
}

temporal_boot_summary <- temporal_boot_imp %>%
  dplyr::group_by(Feature, Risk_Factor) %>%
  dplyr::summarise(
    N_successful = dplyr::n(),
    Mean_normalized_importance = mean(Normalized_importance, na.rm = TRUE),
    SD_normalized_importance = stats::sd(Normalized_importance, na.rm = TRUE),
    Median_normalized_importance = stats::median(Normalized_importance, na.rm = TRUE),
    Q1_normalized_importance = stats::quantile(Normalized_importance, 0.25, na.rm = TRUE),
    Q3_normalized_importance = stats::quantile(Normalized_importance, 0.75, na.rm = TRUE),
    Median_rank = stats::median(Rank, na.rm = TRUE),
    Q1_rank = stats::quantile(Rank, 0.25, na.rm = TRUE),
    Q3_rank = stats::quantile(Rank, 0.75, na.rm = TRUE),
    Top1_frequency = mean(Rank == 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(Median_rank, dplyr::desc(Mean_normalized_importance))

temporal_boot_concordance <- temporal_boot_imp %>%
  dplyr::left_join(temporal_full_rank, by = "Feature") %>%
  dplyr::group_by(Bootstrap_ID) %>%
  dplyr::summarise(
    Kendall_tau_vs_full = suppressWarnings(
      stats::cor(Rank, Full_panel_rank, method = "kendall", use = "complete.obs")
    ),
    .groups = "drop"
  )

temporal_boot_concordance_summary <- temporal_boot_concordance %>%
  dplyr::summarise(
    N_bootstrap = dplyr::n(),
    Mean_Kendall_tau = mean(Kendall_tau_vs_full, na.rm = TRUE),
    SD_Kendall_tau = stats::sd(Kendall_tau_vs_full, na.rm = TRUE),
    Median_Kendall_tau = stats::median(Kendall_tau_vs_full, na.rm = TRUE),
    Q1_Kendall_tau = stats::quantile(Kendall_tau_vs_full, 0.25, na.rm = TRUE),
    Q3_Kendall_tau = stats::quantile(Kendall_tau_vs_full, 0.75, na.rm = TRUE),
    Min_Kendall_tau = min(Kendall_tau_vs_full, na.rm = TRUE),
    Max_Kendall_tau = max(Kendall_tau_vs_full, na.rm = TRUE)
  )

# ------------------------------------------------------------------------------
# C. Export tables, figure, and transparent interpretation note
# ------------------------------------------------------------------------------

readr::write_csv(
  temporal_block_definitions,
  file.path(analysis_tables_dir, "SHAPTemporal_calendar_block_definitions.csv")
)
readr::write_csv(
  temporal_boot_draws,
  file.path(sensitivity_results_dir, "SHAPTemporal_calendar_block_bootstrap_draws.csv")
)
readr::write_csv(
  temporal_fold_perf,
  file.path(analysis_tables_dir, "SHAPTemporal_temporal_validation_performance.csv")
)
readr::write_csv(
  temporal_fold_imp,
  file.path(analysis_tables_dir, "SHAPTemporal_temporal_SHAP_by_fold.csv")
)
readr::write_csv(
  temporal_fold_concordance,
  file.path(analysis_tables_dir, "SHAPTemporal_temporal_rank_concordance.csv")
)
readr::write_csv(
  temporal_boot_imp,
  file.path(sensitivity_results_dir, "SHAPTemporal_block_bootstrap_SHAP_all_replicates.csv")
)
readr::write_csv(
  temporal_boot_summary,
  file.path(analysis_tables_dir, "SHAPTemporal_block_bootstrap_SHAP_summary.csv")
)
readr::write_csv(
  temporal_boot_concordance_summary,
  file.path(analysis_tables_dir, "SHAPTemporal_block_bootstrap_concordance_summary.csv")
)

openxlsx::write.xlsx(
  list(
    Block_definitions = temporal_block_definitions,
    Bootstrap_draws = temporal_boot_draws,
    Temporal_performance = temporal_fold_perf,
    Temporal_SHAP = temporal_fold_imp,
    Temporal_predictions = temporal_fold_pred,
    Temporal_concordance = temporal_fold_concordance,
    Bootstrap_summary = temporal_boot_summary,
    Bootstrap_concordance = temporal_boot_concordance_summary,
    Full_feature_status = df_shap_importance
  ),
  file = file.path(analysis_tables_dir, "SHAPTemporal_temporal_stability.xlsx"),
  overwrite = TRUE
)

temporal_plot_fold <- temporal_fold_imp %>%
  dplyr::mutate(
    Risk_Factor = factor(
      Risk_Factor,
      levels = rev(temporal_boot_summary$Risk_Factor)
    )
  ) %>%
  ggplot2::ggplot(
    ggplot2::aes(
      x = Normalized_importance,
      y = Risk_Factor,
      shape = Analysis_ID
    )
  ) +
  ggplot2::geom_point(size = 2.8, position = ggplot2::position_dodge(width = 0.45)) +
  ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  ggplot2::labs(
    title = "A. Forward temporal validation",
    subtitle = "Normalized mean absolute SHAP importance in held-out future periods",
    x = "Normalized SHAP importance",
    y = NULL,
    shape = "Temporal fold"
  ) +
  theme_pub(12) +
  ggplot2::theme(legend.position = "bottom")

temporal_plot_boot <- temporal_boot_imp %>%
  dplyr::mutate(
    Risk_Factor = factor(
      Risk_Factor,
      levels = rev(temporal_boot_summary$Risk_Factor)
    )
  ) %>%
  ggplot2::ggplot(
    ggplot2::aes(x = Normalized_importance, y = Risk_Factor)
  ) +
  ggplot2::geom_boxplot(outlier.alpha = 0.18, width = 0.65) +
  ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  ggplot2::labs(
    title = "B. Grouped calendar-block bootstrap",
    subtitle = paste0(nrow(temporal_boot_concordance), " successful resamples; seven calendar blocks sampled with replacement"),
    x = "Normalized SHAP importance",
    y = NULL
  ) +
  theme_pub(12)

temporal_plot <- temporal_plot_fold / temporal_plot_boot +
  patchwork::plot_layout(heights = c(1, 1.1)) +
  patchwork::plot_annotation(
    title = "Sensitivity analysis of temporal SHAP-ranking stability",
    subtitle = "High BMI was constant at zero in the selected GBD series and was therefore not rankable."
  )

ggplot2::ggsave(
  file.path(analysis_figures_dir, "SHAPTemporal_temporal_SHAP_stability.pdf"),
  temporal_plot, width = 10, height = 9, device = "pdf"
)
ggplot2::ggsave(
  file.path(analysis_figures_dir, "SHAPTemporal_temporal_SHAP_stability.png"),
  temporal_plot, width = 10, height = 9, dpi = 600, bg = "white"
)

temporal_note <- c(
  "analysis stage INTERPRETATION RULES",
  "",
  "1. Forward temporal validation prevents future calendar years from entering the training data for each held-out period.",
  "2. Fold-specific predictive performance and SHAP importance are calculated exclusively in held-out future-period observations.",
  "3. XGBoost hyperparameters are fixed in the pipeline and applied unchanged across temporal folds; held-out periods are not used for tuning, early stopping, or model selection.",
  "4. The grouped bootstrap resamples six complete non-overlapping 5-year calendar blocks and one final 4-year block (2020-2023), with replacement, retaining all five age groups within each selected year and refitting the model in every replicate.",
  "5. Stable ranking requires broadly similar ranks/normalized importance across folds and high bootstrap top-rank frequency.",
  "6. Predictive performance is reported transparently but does not convert this exploratory analysis into a validated clinical model.",
  "7. High BMI is excluded from SHAP ranking because all 170 China-specific age-year PAF estimates are exactly zero; correlation or mediation with FPG cannot be estimated from a constant series.",
  "8. No result from this module should be described as causal evidence."
)
writeLines(
  temporal_note,
  con = file.path(sensitivity_results_dir, "SHAPTemporal_interpretation_rules.txt")
)

message("✓ Temporal stability of exploratory SHAP rankings outputs exported.")

# ==============================================================================
# END EMBEDDED MODULE: 03_shap_temporal_stability.R
# ==============================================================================


top_plots <- cowplot::plot_grid(
  p_macro + theme(legend.position = "bottom"),
  p_shap,
  align = "h",
  axis = "tb",
  rel_widths = c(1, 1.15)
)

fig3_final <- wrap_elements(top_plots) /
  wrap_elements(p_micro_china) /
  wrap_elements(p_micro_global) +
  plot_layout(heights = c(1.15, 1, 1))

save_plot_both(fig3_final, "Figure 3", width = 18, height = 15)

# ------------------------------------------------------------------------------
# 8. Figure 4: MIR and HCA productivity losses
# ------------------------------------------------------------------------------

message("▶ Building Figure 4...")

# Calculate the mortality-to-incidence ratio consistently for ages 25-49
# from the recalculated weighted ASDR and ASIR series.
df_mir <- df_asir_25_49 %>%
  full_join(
    df_asdr_25_49,
    by = c("location_name", "year")
  ) %>%
  mutate(
    MIR = ASDR_25_49 / ASIR_25_49,
    Incidence = ASIR_25_49,
    Deaths = ASDR_25_49,
    location_name = factor(location_name, levels = target_locations)
  )

df_mir_bg <- df_mir %>% filter(location_name != "China")
df_mir_china <- df_mir %>% filter(location_name == "China")

contrast_colors <- c("Global" = "#888888", sdi_colors)

p_mir <- ggplot() +
  geom_line(
    data = df_mir_bg,
    aes(x = year, y = MIR, color = location_name, linetype = location_name == "Global"),
    linewidth = 0.8,
    alpha = 0.55
  ) +
  scale_color_manual(values = contrast_colors, name = "Reference region") +
  scale_linetype_manual(values = c("TRUE" = "dashed", "FALSE" = "solid"), guide = "none") +
  geom_line(
    data = df_mir_china,
    aes(x = year, y = MIR),
    color = "black",
    linewidth = 1.2,
    linetype = "solid"
  ) +
  geom_point(
    data = df_mir_china,
    aes(x = year, y = MIR),
    shape = 21,
    size = 2.4,
    fill = "black",
    color = "black",
    stroke = 0.5
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "A. Mortality-to-incidence ratio trajectories, ages 25–49",
    subtitle = "Ratio of recalculated weighted ASDR to weighted ASIR",
    x = "", y = "MIR"
  ) +
  theme_pub(13)

df_yll_age <- df_full %>%
  filter(location_name == "China", metric_name == "Number", str_detect(measure_name, "YLL"), age_name %in% age_specific) %>%
  group_by(year, age_name) %>%
  summarise(Total_YLLs = sum(val, na.rm = TRUE), .groups = "drop")

# Risk-specific productivity losses use YLL-specific PAFs, matching the
# mortality-related Human Capital Approach definition. The frozen analysis does
# not permit automatic substitution with DALY PAFs.
df_paf_source_candidates <- df_data3 %>%
  filter(
    location_name == "China",
    metric_name == "Percent",
    str_detect(measure_name, "YLL|DALY"),
    rei_name %in% level3_risks,
    age_name %in% age_specific,
    year >= 1990,
    year <= 2023
  ) %>%
  mutate(
    Risk_Factor = risk_name_clean(rei_name),
    PAF_Measure = case_when(
      str_detect(measure_name, "YLL") ~ "YLL",
      str_detect(measure_name, "DALY") ~ "DALY",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(PAF_Measure))

expected_hca_paf_keys <- length(1990:2023) *
  length(age_specific) *
  length(level3_risks)

df_paf_measure_inventory <- df_paf_source_candidates %>%
  group_by(PAF_Measure) %>%
  summarise(
    Raw_rows = n(),
    Unique_year_age_risk_keys =
      n_distinct(year, age_name, Risk_Factor),
    Missing_val = sum(is.na(val)),
    Missing_lower = sum(is.na(lower)),
    Missing_upper = sum(is.na(upper)),
    Expected_keys = expected_hca_paf_keys,
    Complete_for_all_keys =
      Unique_year_age_risk_keys == expected_hca_paf_keys &&
      Missing_val == 0,
    .groups = "drop"
  )

yll_complete <- df_paf_measure_inventory %>%
  filter(PAF_Measure == "YLL") %>%
  pull(Complete_for_all_keys)

yll_complete <- length(yll_complete) == 1 && isTRUE(yll_complete)

if (!yll_complete) {
  stop(
    "YLL-specific PAFs are incomplete for one or more required ",
    "China year-age-risk combinations. The frozen HCA analysis does not ",
    "permit substitution with DALY PAFs."
  )
}

hca_paf_measure_used <- "YLL"
hca_paf_measure_note <- paste(
  "YLL-specific PAFs were used, consistent with",
  "mortality-related productivity loss."
)

df_paf_all <- df_paf_source_candidates %>%
  filter(PAF_Measure == hca_paf_measure_used) %>%
  group_by(year, age_name, Risk_Factor) %>%
  summarise(
    PAF = mean(val, na.rm = TRUE),
    PAF_lower = mean(lower, na.rm = TRUE),
    PAF_upper = mean(upper, na.rm = TRUE),
    .groups = "drop"
  )

df_econ_base <- df_yll_age %>%
  left_join(df_gdp, by = "year") %>%
  left_join(df_ilo, by = c("year", "age_name")) %>%
  mutate(
    Total_Loss_Billion = (Total_YLLs * GDP_per_capita * LFPR) / 1e9
  )

if (any(is.na(df_econ_base$GDP_per_capita))) {
  stop("Missing GDP-per-capita values after joining the 1990-2023 HCA inputs.")
}

if (any(is.na(df_econ_base$LFPR))) {
  stop("Missing age-specific female LFPR values after joining the HCA inputs.")
}

df_econ_master <- df_econ_base %>%
  left_join(df_paf_all, by = c("year", "age_name"))

if (any(is.na(df_econ_master$PAF))) {
  stop(
    "Missing YLL-specific PAF values after joining HCA inputs. ",
    "Inspect year-age-risk keys; missing values must not be replaced by zero."
  )
}

df_econ_master <- df_econ_master %>%
  mutate(
    PAF_Source = PAF,
    PAF_For_Loss = pmax(PAF_Source, 0),
    Specific_Risk_Loss_Billion = Total_Loss_Billion * PAF_For_Loss
  )

hca_paf_truncation_audit <- df_econ_master %>%
  summarise(
    PAF_measure = hca_paf_measure_used,
    Rows = n(),
    Negative_source_PAF_rows = sum(PAF_Source < 0, na.rm = TRUE),
    Zero_source_PAF_rows = sum(PAF_Source == 0, na.rm = TRUE),
    Missing_source_PAF_rows = sum(is.na(PAF_Source)),
    Minimum_source_PAF = min(PAF_Source, na.rm = TRUE),
    Maximum_source_PAF = max(PAF_Source, na.rm = TRUE),
    Rule_for_loss_calculation =
      "Negative signed source PAFs were retained for audit and truncated to zero only for monetary loss calculations."
  )

write_csv(
  hca_paf_truncation_audit,
  file.path(analysis_tables_dir, "HCA_PAF_truncation_audit.csv")
)

df_total_2023 <- df_econ_base %>%
  filter(year == 2023) %>%
  select(age_name, Total_Loss_Billion) %>%
  distinct() %>%
  mutate(age_name = factor(age_name, levels = age_specific))

total_loss_sum <- sum(df_total_2023$Total_Loss_Billion, na.rm = TRUE)

p_bar_total <- ggplot(df_total_2023, aes(x = age_name, y = Total_Loss_Billion)) +
  geom_col(fill = "#34495E", color = "black", linewidth = 0.35, width = 0.65) +
  geom_text(aes(label = paste0("US$", round(Total_Loss_Billion, 2), "B")), vjust = -0.4, fontface = "bold", size = 3.4) +
  scale_y_continuous(labels = label_dollar(prefix = "US$", suffix = "B"), expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "B. Estimated productivity loss in 2023",
    subtitle = paste0("Total across ages 25-49: US$", round(total_loss_sum, 2), " billion (constant 2015 dollars)"),
    x = "Age group",
    y = "Loss, billion constant 2015 US$"
  ) +
  theme_pub(13)

df_risk_trend <- df_econ_master %>%
  group_by(year, Risk_Factor) %>%
  summarise(Loss_Billion = sum(Specific_Risk_Loss_Billion, na.rm = TRUE), .groups = "drop") %>%
  drop_na()

df_risk_label <- df_risk_trend %>% filter(year == max(year))

risk_colors <- c(
  "High fasting plasma glucose" = "#00A087FF",
  "High BMI" = "#A3A500",
  "Low physical activity" = "#00B0F6",
  "High alcohol use" = "#F8766D",
  "Tobacco use" = "#E76BF3"
)

p_risk_trend <- ggplot(df_risk_trend, aes(x = year, y = Loss_Billion, color = Risk_Factor, group = Risk_Factor)) +
  geom_line(linewidth = 1.3) +
  geom_point(data = df_risk_trend %>% filter(year %% 5 == 0 | year == 2023), size = 1.6) +
  geom_text_repel(data = df_risk_label, aes(label = Risk_Factor), nudge_x = 1.5, direction = "y", hjust = 0, fontface = "bold", size = 3.3) +
  scale_color_manual(values = risk_colors) +
  scale_y_continuous(labels = label_dollar(prefix = "US$", suffix = "B")) +
  scale_x_continuous(breaks = seq(1990, 2023, by = 5), expand = expansion(mult = c(0.02, 0.30))) +
  labs(title = "C. Risk-attributable productivity losses", x = "Year", y = "Attributable loss, billion constant 2015 US$") +
  theme_pub(13) +
  theme(legend.position = "none")

df_hca_total_summary <- df_econ_base %>%
  group_by(year) %>%
  summarise(
    `Total YLLs, 25-49y` = sum(Total_YLLs, na.rm = TRUE),
    `Total productivity loss, billion constant 2015 US$` =
      sum(Total_Loss_Billion, na.rm = TRUE),
    .groups = "drop"
  )

df_hca_risk_summary <- df_econ_master %>%
  group_by(year) %>%
  summarise(
    `High BMI loss, billion constant 2015 US$` =
      sum(
        Specific_Risk_Loss_Billion[Risk_Factor == "High BMI"],
        na.rm = TRUE
      ),
    `High fasting plasma glucose loss, billion constant 2015 US$` =
      sum(
        Specific_Risk_Loss_Billion[
          Risk_Factor == "High fasting plasma glucose"
        ],
        na.rm = TRUE
      ),
    `Tobacco use loss, billion constant 2015 US$` =
      sum(
        Specific_Risk_Loss_Billion[Risk_Factor == "Tobacco use"],
        na.rm = TRUE
      ),
    `High alcohol use loss, billion constant 2015 US$` =
      sum(
        Specific_Risk_Loss_Billion[Risk_Factor == "High alcohol use"],
        na.rm = TRUE
      ),
    `Low physical activity loss, billion constant 2015 US$` =
      sum(
        Specific_Risk_Loss_Billion[
          Risk_Factor == "Low physical activity"
        ],
        na.rm = TRUE
      ),
    .groups = "drop"
  )

df_hca_summary <- df_hca_total_summary %>%
  left_join(df_hca_risk_summary, by = "year") %>%
  mutate(
    Location = "China",
    .before = year
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

write_csv(df_hca_summary, file.path(tables_dir, "Supplementary_Table_S4_HCA_productivity_losses_full.csv"))
write_csv(df_hca_summary %>% filter(year %in% c(1990, 2000, 2010, 2020, 2023)), file.path(tables_dir, "Supplementary_Table_S4_HCA_productivity_losses_selected_years.csv"))

fig4_final <- p_mir / (p_bar_total | p_risk_trend) + plot_layout(heights = c(1.2, 1))
save_plot_both(fig4_final, "Figure 4", width = 16, height = 12)

# ------------------------------------------------------------------------------
# 9. Figure 5: BAPC projection
# Important: this block reproduces the original Figure 5 style and logic.
# It does not use flextable, ggpubr, tab_dir, or any Word-table functions.
# ------------------------------------------------------------------------------

message("▶ Building Figure 5...")

gbd_weights <- tibble(
  age_name = age_specific,
  weight = c(0.0807, 0.0754, 0.0699, 0.0645, 0.0585)
)

df_hist_bapc <- df_full %>%
  filter(
    metric_name %in% c("Number", "Rate"),
    str_detect(measure_name, "Incidence"),
    age_name %in% age_specific
  ) %>%
  select(location_name, year, age_name, metric_name, val) %>%
  group_by(location_name, year, age_name, metric_name) %>%
  summarise(val = mean(val, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = metric_name, values_from = val) %>%
  mutate(
    Population = Number / (Rate / 100000),
    Cases = Number
  ) %>%
  drop_na(Cases, Population)

file_un_pop <- list.files(input_path, pattern = "(WPP2024|UN_Pop).*\\.csv$", full.names = TRUE)[1]
if (is.na(file_un_pop)) stop("No WPP2024 or UN_Pop future population file found.")

df_fut_china_raw <- read_csv(file_un_pop, show_col_types = FALSE)

required_cols <- c("Location", "Time", "AgeGrp", "PopFemale")
missing_cols <- setdiff(required_cols, names(df_fut_china_raw))
if (length(missing_cols) > 0) {
  stop(paste0("Future population file missing columns: ", paste(missing_cols, collapse = ", ")))
}

df_fut_china <- df_fut_china_raw %>%
  filter(
    Location == "China",
    Time > 2023,
    Time <= 2050,
    AgeGrp %in% c("25-29", "30-34", "35-39", "40-44", "45-49")
  ) %>%
  mutate(
    location_name = "China",
    PopFemale = PopFemale * 1000,
    age_name = paste0(AgeGrp, " years"),
    year = Time,
    Cases = NA_real_,
    Population = PopFemale
  ) %>%
  select(location_name, year, age_name, Cases, Population)

df_fut_global <- df_hist_bapc %>%
  filter(location_name == "Global") %>%
  group_by(location_name, age_name) %>%
  group_modify(~ {
    model <- auto.arima(.x$Population)
    pred <- forecast(model, h = length(2024:2050))$mean
    tibble(year = 2024:2050, Population = pmax(as.numeric(pred), 1))
  }) %>%
  ungroup() %>%
  mutate(Cases = NA_real_)

df_bapc_all <- bind_rows(
  df_hist_bapc %>%
    filter(location_name %in% c("China", "Global")) %>%
    select(location_name, year, age_name, Cases, Population),
  df_fut_china,
  df_fut_global
) %>%
  mutate(
    age_name = factor(age_name, levels = age_specific),
    age_idx = as.numeric(age_name),
    year_idx = year - min(year) + 1,
    age_start = as.numeric(str_extract(as.character(age_name), "^[0-9]+")),
    cohort_idx = as.numeric(factor(year - age_start)),
    Cases_Int = round(Cases),
    Population = ifelse(Population <= 0, 0.01, Population)
  )

inla.setOption(num.threads = 1)
inla.setOption(inla.mode = "classic")

quantiles_req <- c(0.025, 0.1, 0.2, 0.3, 0.4, 0.45, 0.5, 0.55, 0.6, 0.7, 0.8, 0.9, 0.975)

run_bapc_for_loc <- function(loc) {
  message("  - BAPC model for: ", loc)

  df_sub <- df_bapc_all %>% filter(location_name == loc)

  res <- inla(
    Cases_Int ~ 1 +
      f(age_idx, model = "iid") +
      f(year_idx, model = "rw2", scale.model = TRUE) +
      f(cohort_idx, model = "rw2", scale.model = TRUE),
    family = "poisson",
    data = df_sub,
    E = Population,
    control.predictor = list(link = 1, compute = TRUE, quantiles = quantiles_req)
  )

  # Original Figure 5 conversion:
  # fitted values are multiplied by 100,000; do not divide by Population.
  df_sub %>%
    mutate(
      Pred_Rate = res$summary.fitted.values$mean * 100000,
      Lwr_95 = res$summary.fitted.values$`0.025quant` * 100000,
      Upr_95 = res$summary.fitted.values$`0.975quant` * 100000,
      Lwr_80 = res$summary.fitted.values$`0.1quant` * 100000,
      Upr_80 = res$summary.fitted.values$`0.9quant` * 100000,
      Lwr_60 = res$summary.fitted.values$`0.2quant` * 100000,
      Upr_60 = res$summary.fitted.values$`0.8quant` * 100000,
      Lwr_40 = res$summary.fitted.values$`0.3quant` * 100000,
      Upr_40 = res$summary.fitted.values$`0.7quant` * 100000,
      Lwr_20 = res$summary.fitted.values$`0.4quant` * 100000,
      Upr_20 = res$summary.fitted.values$`0.6quant` * 100000,
      Lwr_10 = res$summary.fitted.values$`0.45quant` * 100000,
      Upr_10 = res$summary.fitted.values$`0.55quant` * 100000,
      Observed_Rate = (Cases / Population) * 100000
    )
}

df_pred_unanchored <- map_dfr(c("China", "Global"), run_bapc_for_loc)

# ------------------------------------------------------------------------------
# Projection-anchor sensitivity analysis: explicit multiplicative anchoring at the 2023 boundary
# ------------------------------------------------------------------------------
# The earlier implementation only replaced the single 2023 point with the observed rate.
# That did not propagate the adjustment to 2024-2050 and collapsed the 2023
# uncertainty interval to zero width. The anchored analysis estimates a separate
# multiplicative anchor ratio for every location-age series:
#
#   anchor ratio = observed rate in 2023 / unanchored fitted rate in 2023
#
# The ratio is applied to the posterior mean and every posterior interval bound
# from 2023 onward. This guarantees continuity at the observed 2023 mean while
# preserving the relative shape and uncertainty width of the model projection.

projection_rate_cols <- c(
  "Pred_Rate", "Lwr_95", "Upr_95", "Lwr_80", "Upr_80",
  "Lwr_60", "Upr_60", "Lwr_40", "Upr_40", "Lwr_20", "Upr_20",
  "Lwr_10", "Upr_10"
)

df_anchor_factors <- df_pred_unanchored %>%
  filter(year == 2023) %>%
  transmute(
    location_name,
    age_name = as.character(age_name),
    Observed_2023 = Observed_Rate,
    Predicted_2023_unanchored = Pred_Rate,
    Anchor_Ratio = Observed_2023 / Predicted_2023_unanchored,
    Absolute_Boundary_Difference = Observed_2023 - Predicted_2023_unanchored,
    Relative_Boundary_Difference_Percent =
      100 * Absolute_Boundary_Difference / Predicted_2023_unanchored
  )

if (
  nrow(df_anchor_factors) != length(c("China", "Global")) * length(age_specific) ||
  any(!is.finite(df_anchor_factors$Anchor_Ratio)) ||
  any(df_anchor_factors$Anchor_Ratio <= 0)
) {
  stop(
    "Invalid 2023 anchoring factors. Check the 2023 observed and fitted rates ",
    "before using the projection results."
  )
}

df_pred_all <- df_pred_unanchored %>%
  mutate(age_name = as.character(age_name)) %>%
  rename_with(
    ~ paste0(.x, "_Unanchored"),
    all_of(projection_rate_cols)
  ) %>%
  left_join(
    df_anchor_factors %>% select(location_name, age_name, Anchor_Ratio),
    by = c("location_name", "age_name")
  ) %>%
  mutate(
    Pred_Rate = if_else(
      year >= 2023,
      Pred_Rate_Unanchored * Anchor_Ratio,
      Pred_Rate_Unanchored
    ),
    Lwr_95 = if_else(year >= 2023, Lwr_95_Unanchored * Anchor_Ratio, Lwr_95_Unanchored),
    Upr_95 = if_else(year >= 2023, Upr_95_Unanchored * Anchor_Ratio, Upr_95_Unanchored),
    Lwr_80 = if_else(year >= 2023, Lwr_80_Unanchored * Anchor_Ratio, Lwr_80_Unanchored),
    Upr_80 = if_else(year >= 2023, Upr_80_Unanchored * Anchor_Ratio, Upr_80_Unanchored),
    Lwr_60 = if_else(year >= 2023, Lwr_60_Unanchored * Anchor_Ratio, Lwr_60_Unanchored),
    Upr_60 = if_else(year >= 2023, Upr_60_Unanchored * Anchor_Ratio, Upr_60_Unanchored),
    Lwr_40 = if_else(year >= 2023, Lwr_40_Unanchored * Anchor_Ratio, Lwr_40_Unanchored),
    Upr_40 = if_else(year >= 2023, Upr_40_Unanchored * Anchor_Ratio, Upr_40_Unanchored),
    Lwr_20 = if_else(year >= 2023, Lwr_20_Unanchored * Anchor_Ratio, Lwr_20_Unanchored),
    Upr_20 = if_else(year >= 2023, Upr_20_Unanchored * Anchor_Ratio, Upr_20_Unanchored),
    Lwr_10 = if_else(year >= 2023, Lwr_10_Unanchored * Anchor_Ratio, Lwr_10_Unanchored),
    Upr_10 = if_else(year >= 2023, Upr_10_Unanchored * Anchor_Ratio, Upr_10_Unanchored),
    Projection_Phase = if_else(year <= 2023, "Observed period", "Projected period")
  )

# The posterior mean must meet the observed 2023 rate after anchoring. Posterior
# intervals are scaled, not collapsed, so uncertainty remains visible at 2023.
anchor_tolerance <- 1e-8
anchor_error <- df_pred_all %>%
  filter(year == 2023) %>%
  summarise(max_error = max(abs(Pred_Rate - Observed_Rate), na.rm = TRUE)) %>%
  pull(max_error)

if (!is.finite(anchor_error) || anchor_error > anchor_tolerance) {
  stop("Anchored 2023 posterior means do not match the observed rates.")
}

# IMPORTANT: the posterior mean is not mathematically required to lie inside
# every narrow central quantile interval (especially the 10% or 20% bands) when
# the posterior distribution is skewed. Therefore, interval validity must be
# checked from the quantile bounds themselves, not by forcing every interval to
# contain the posterior mean.
ui_order_diagnostics <- df_pred_all %>%
  filter(year >= 2023) %>%
  mutate(
    Invalid_95 = Lwr_95 > Upr_95,
    Invalid_80 = Lwr_80 > Upr_80,
    Invalid_60 = Lwr_60 > Upr_60,
    Invalid_40 = Lwr_40 > Upr_40,
    Invalid_20 = Lwr_20 > Upr_20,
    Invalid_10 = Lwr_10 > Upr_10,
    Invalid_Lower_Nesting = !(
      Lwr_95 <= Lwr_80 & Lwr_80 <= Lwr_60 &
      Lwr_60 <= Lwr_40 & Lwr_40 <= Lwr_20 &
      Lwr_20 <= Lwr_10
    ),
    Invalid_Upper_Nesting = !(
      Upr_10 <= Upr_20 & Upr_20 <= Upr_40 &
      Upr_40 <= Upr_60 & Upr_60 <= Upr_80 &
      Upr_80 <= Upr_95
    ),
    Mean_Outside_95 = Pred_Rate < Lwr_95 | Pred_Rate > Upr_95,
    Mean_Outside_80 = Pred_Rate < Lwr_80 | Pred_Rate > Upr_80,
    Mean_Outside_60 = Pred_Rate < Lwr_60 | Pred_Rate > Upr_60,
    Mean_Outside_40 = Pred_Rate < Lwr_40 | Pred_Rate > Upr_40,
    Mean_Outside_20 = Pred_Rate < Lwr_20 | Pred_Rate > Upr_20,
    Mean_Outside_10 = Pred_Rate < Lwr_10 | Pred_Rate > Upr_10
  )

write_csv(
  ui_order_diagnostics %>%
    filter(
      Invalid_95 | Invalid_80 | Invalid_60 | Invalid_40 | Invalid_20 |
      Invalid_10 | Invalid_Lower_Nesting | Invalid_Upper_Nesting |
      Mean_Outside_95 | Mean_Outside_80 | Mean_Outside_60 |
      Mean_Outside_40 | Mean_Outside_20 | Mean_Outside_10
    ) %>%
    select(
      location_name, age_name, year, Pred_Rate,
      Lwr_95, Upr_95, Lwr_80, Upr_80, Lwr_60, Upr_60,
      Lwr_40, Upr_40, Lwr_20, Upr_20, Lwr_10, Upr_10,
      starts_with("Invalid_"), starts_with("Mean_Outside_")
    ),
  file.path(analysis_tables_dir, "ProjectionAnchor_interval_order_diagnostics.csv")
)

ui_order_invalid <- ui_order_diagnostics %>%
  summarise(
    invalid = any(
      Invalid_95 | Invalid_80 | Invalid_60 | Invalid_40 | Invalid_20 |
      Invalid_10 | Invalid_Lower_Nesting | Invalid_Upper_Nesting,
      na.rm = TRUE
    )
  ) %>%
  pull(invalid)

if (isTRUE(ui_order_invalid)) {
  stop(
    "At least one anchored projection has crossed or non-nested quantile bounds. ",
    "See ProjectionAnchor_interval_order_diagnostics.csv."
  )
}

# Weighted 25-49-year summary. The overall uncertainty limits are standardized
# weighted aggregations of the age-specific posterior limits, matching the
# aggregation approach used throughout the pipeline.
df_asir_all <- df_pred_all %>%
  left_join(gbd_weights, by = "age_name") %>%
  group_by(location_name, year) %>%
  summarise(
    Observed_Rate = if (all(is.na(Observed_Rate))) {
      NA_real_
    } else {
      sum(Observed_Rate * weight, na.rm = TRUE) /
        sum(weight[!is.na(Observed_Rate)])
    },
    Pred_Rate = sum(Pred_Rate * weight, na.rm = TRUE) / sum(weight),
    Lwr_95 = sum(Lwr_95 * weight, na.rm = TRUE) / sum(weight),
    Upr_95 = sum(Upr_95 * weight, na.rm = TRUE) / sum(weight),
    Lwr_80 = sum(Lwr_80 * weight, na.rm = TRUE) / sum(weight),
    Upr_80 = sum(Upr_80 * weight, na.rm = TRUE) / sum(weight),
    Lwr_60 = sum(Lwr_60 * weight, na.rm = TRUE) / sum(weight),
    Upr_60 = sum(Upr_60 * weight, na.rm = TRUE) / sum(weight),
    Lwr_40 = sum(Lwr_40 * weight, na.rm = TRUE) / sum(weight),
    Upr_40 = sum(Upr_40 * weight, na.rm = TRUE) / sum(weight),
    Lwr_20 = sum(Lwr_20 * weight, na.rm = TRUE) / sum(weight),
    Upr_20 = sum(Upr_20 * weight, na.rm = TRUE) / sum(weight),
    Lwr_10 = sum(Lwr_10 * weight, na.rm = TRUE) / sum(weight),
    Upr_10 = sum(Upr_10 * weight, na.rm = TRUE) / sum(weight),
    .groups = "drop"
  ) %>%
  mutate(age_name = "Overall ASIR (25-49y)")

df_asir_unanchored <- df_pred_all %>%
  left_join(gbd_weights, by = "age_name") %>%
  group_by(location_name, year) %>%
  summarise(
    Observed_Rate = if (all(is.na(Observed_Rate))) {
      NA_real_
    } else {
      sum(Observed_Rate * weight, na.rm = TRUE) /
        sum(weight[!is.na(Observed_Rate)])
    },
    Pred_Rate = sum(Pred_Rate_Unanchored * weight, na.rm = TRUE) / sum(weight),
    Lwr_95 = sum(Lwr_95_Unanchored * weight, na.rm = TRUE) / sum(weight),
    Upr_95 = sum(Upr_95_Unanchored * weight, na.rm = TRUE) / sum(weight),
    Lwr_80 = sum(Lwr_80_Unanchored * weight, na.rm = TRUE) / sum(weight),
    Upr_80 = sum(Upr_80_Unanchored * weight, na.rm = TRUE) / sum(weight),
    Lwr_60 = sum(Lwr_60_Unanchored * weight, na.rm = TRUE) / sum(weight),
    Upr_60 = sum(Upr_60_Unanchored * weight, na.rm = TRUE) / sum(weight),
    Lwr_40 = sum(Lwr_40_Unanchored * weight, na.rm = TRUE) / sum(weight),
    Upr_40 = sum(Upr_40_Unanchored * weight, na.rm = TRUE) / sum(weight),
    Lwr_20 = sum(Lwr_20_Unanchored * weight, na.rm = TRUE) / sum(weight),
    Upr_20 = sum(Upr_20_Unanchored * weight, na.rm = TRUE) / sum(weight),
    Lwr_10 = sum(Lwr_10_Unanchored * weight, na.rm = TRUE) / sum(weight),
    Upr_10 = sum(Upr_10_Unanchored * weight, na.rm = TRUE) / sum(weight),
    .groups = "drop"
  ) %>%
  mutate(age_name = "Overall ASIR (25-49y)")

df_final_fig5 <- bind_rows(
  df_asir_all,
  df_pred_all %>%
    select(
      location_name, year, age_name, Observed_Rate, Pred_Rate,
      Lwr_95, Upr_95, Lwr_80, Upr_80, Lwr_60, Upr_60,
      Lwr_40, Upr_40, Lwr_20, Upr_20, Lwr_10, Upr_10
    )
)

df_final_fig5_unanchored <- bind_rows(
  df_asir_unanchored,
  df_pred_all %>%
    transmute(
      location_name,
      year,
      age_name,
      Observed_Rate,
      Pred_Rate = Pred_Rate_Unanchored,
      Lwr_95 = Lwr_95_Unanchored,
      Upr_95 = Upr_95_Unanchored,
      Lwr_80 = Lwr_80_Unanchored,
      Upr_80 = Upr_80_Unanchored,
      Lwr_60 = Lwr_60_Unanchored,
      Upr_60 = Upr_60_Unanchored,
      Lwr_40 = Lwr_40_Unanchored,
      Upr_40 = Upr_40_Unanchored,
      Lwr_20 = Lwr_20_Unanchored,
      Upr_20 = Upr_20_Unanchored,
      Lwr_10 = Lwr_10_Unanchored,
      Upr_10 = Upr_10_Unanchored
    )
)

write_csv(
  df_final_fig5,
  file.path(tables_dir, "Table5_BAPC_Dual_Gradient_anchored_Figure5.csv")
)

# Backward-compatible name, now containing the explicitly anchored main result.
write_csv(
  df_final_fig5,
  file.path(tables_dir, "Table5_BAPC_Dual_Gradient_original_Figure5.csv")
)

df_bapc_selected <- df_final_fig5 %>%
  filter(year %in% c(2023, 2030, 2040, 2050)) %>%
  mutate(
    Estimate_UI = paste0(
      round(Pred_Rate, 2), " (",
      round(Lwr_95, 2), " to ", round(Upr_95, 2), ")"
    )
  ) %>%
  select(
    Location = location_name,
    `Age group` = age_name,
    Year = year,
    `Projected incidence rate per 100,000, 95% UI` = Estimate_UI
  ) %>%
  arrange(Location, `Age group`, Year)

write_csv(
  df_bapc_selected,
  file.path(tables_dir, "Supplementary_Table_S5_BAPC_projection_selected_years.csv")
)

# Export anchoring and uncertainty diagnostics.
# ==============================================================================
# BEGIN EMBEDDED MODULE: 04_projection_anchor_sensitivity.R
# ==============================================================================

# ==============================================================================
# Analysis module 05: projection anchoring and uncertainty diagnostics
#
# Required objects from the main pipeline:
#   df_anchor_factors
#   df_pred_all
#   df_final_fig5
#   df_final_fig5_unanchored
#   analysis_tables_dir
#   analysis_figures_dir
#
# Outputs:
#   ProjectionAnchor_projection_anchoring_diagnostics.csv
#   ProjectionAnchor_anchored_vs_unanchored_selected_years.csv
#   ProjectionAnchor_projection_selected_years_95UI.csv
#   ProjectionAnchor_projection_quality_checks.csv
#   ProjectionAnchor_projection_sensitivity.xlsx
#   ProjectionAnchor_anchored_vs_unanchored_projection.pdf/.png
#   ProjectionAnchor_status_quo_assumptions.txt
# ============================================================================== 

message("▶ BAPC anchoring and uncertainty diagnostics...")

required_projection_objects <- c(
  "df_anchor_factors", "df_pred_all", "df_final_fig5",
  "df_final_fig5_unanchored", "analysis_tables_dir",
  "analysis_figures_dir"
)
missing_projection_objects <- required_projection_objects[
  !vapply(required_projection_objects, exists, logical(1), inherits = TRUE)
]
if (length(missing_projection_objects) > 0) {
  stop(
    "Projection sensitivity module is missing required objects: ",
    paste(missing_projection_objects, collapse = ", ")
  )
}

selected_projection_years <- c(2023, 2030, 2040, 2050)

# 1. Series-specific anchor factors --------------------------------------------
anchor_diagnostics <- df_anchor_factors %>%
  mutate(
    age_name = as.character(age_name),
    Anchor_Ratio = as.numeric(Anchor_Ratio),
    Anchoring_Direction = case_when(
      Anchor_Ratio > 1 ~ "Upward anchoring",
      Anchor_Ratio < 1 ~ "Downward anchoring",
      TRUE ~ "No anchoring"
    )
  ) %>%
  arrange(location_name, factor(age_name, levels = age_specific))

write_csv(
  anchor_diagnostics,
  file.path(
    analysis_tables_dir,
    "ProjectionAnchor_projection_anchoring_diagnostics.csv"
  )
)

# 2. Anchored versus unanchored estimates -------------------------------------
anchored_selected <- df_final_fig5 %>%
  filter(year %in% selected_projection_years) %>%
  transmute(
    location_name,
    age_name = as.character(age_name),
    year,
    Observed_Rate,
    Anchored_Rate = Pred_Rate,
    Anchored_Lwr_95 = Lwr_95,
    Anchored_Upr_95 = Upr_95
  )

unanchored_selected <- df_final_fig5_unanchored %>%
  filter(year %in% selected_projection_years) %>%
  transmute(
    location_name,
    age_name = as.character(age_name),
    year,
    Unanchored_Rate = Pred_Rate,
    Unanchored_Lwr_95 = Lwr_95,
    Unanchored_Upr_95 = Upr_95
  )

anchored_vs_unanchored <- anchored_selected %>%
  left_join(
    unanchored_selected,
    by = c("location_name", "age_name", "year")
  ) %>%
  mutate(
    Absolute_Change = Anchored_Rate - Unanchored_Rate,
    Relative_Change_Percent = 100 * Absolute_Change / Unanchored_Rate,
    Anchored_95UI = paste0(
      round(Anchored_Rate, 2), " (",
      round(Anchored_Lwr_95, 2), " to ",
      round(Anchored_Upr_95, 2), ")"
    ),
    Unanchored_95UI = paste0(
      round(Unanchored_Rate, 2), " (",
      round(Unanchored_Lwr_95, 2), " to ",
      round(Unanchored_Upr_95, 2), ")"
    )
  ) %>%
  arrange(location_name, age_name, year)

write_csv(
  anchored_vs_unanchored,
  file.path(
    analysis_tables_dir,
    "ProjectionAnchor_anchored_vs_unanchored_selected_years.csv"
  )
)

# 3. Clean manuscript-ready 95% UI table --------------------------------------
projection_selected_95ui <- anchored_selected %>%
  mutate(
    `Observed rate in 2023` = dplyr::if_else(
      .data$year == 2023,
      as.numeric(.data$Observed_Rate),
      NA_real_
    ),
    `Projected incidence rate per 100,000 (95% UI)` = paste0(
      round(.data$Anchored_Rate, 2), " (",
      round(.data$Anchored_Lwr_95, 2), "-",
      round(.data$Anchored_Upr_95, 2), ")"
    )
  ) %>%
  select(
    Location = .data$location_name,
    `Age group` = .data$age_name,
    Year = .data$year,
    `Observed rate in 2023`,
    `Projected mean` = .data$Anchored_Rate,
    `Lower 95% UI` = .data$Anchored_Lwr_95,
    `Upper 95% UI` = .data$Anchored_Upr_95,
    `Projected incidence rate per 100,000 (95% UI)`
  ) %>%
  arrange(.data$Location, .data$`Age group`, .data$Year)

write_csv(
  projection_selected_95ui,
  file.path(
    analysis_tables_dir,
    "ProjectionAnchor_projection_selected_years_95UI.csv"
  )
)

# 4. Automated quality checks -------------------------------------------------
future_projection_rows <- df_final_fig5 %>% filter(year >= 2023)

quality_checks <- tibble(
  Check = c(
    "All age-specific anchor ratios are finite",
    "All age-specific anchor ratios are positive",
    "Anchored age-specific posterior means equal observed 2023 rates",
    "No negative anchored posterior means or 95% bounds",
    "All anchored 95% intervals have correctly ordered bounds",
    "All anchored fan-chart intervals are properly nested",
    "Selected projection years are present for both locations and all series",
    "All selected-year anchored estimates are finite"
  ),
  Passed = c(
    all(is.finite(anchor_diagnostics$Anchor_Ratio)),
    all(anchor_diagnostics$Anchor_Ratio > 0),
    df_pred_all %>%
      filter(year == 2023) %>%
      summarise(x = all(abs(Pred_Rate - Observed_Rate) < 1e-8)) %>%
      pull(x),
    future_projection_rows %>%
      summarise(x = all(Pred_Rate >= 0 & Lwr_95 >= 0 & Upr_95 >= 0)) %>%
      pull(x),
    future_projection_rows %>%
      summarise(x = all(Lwr_95 <= Upr_95)) %>%
      pull(x),
    future_projection_rows %>%
      summarise(
        x = all(
          Lwr_95 <= Lwr_80 & Lwr_80 <= Lwr_60 &
          Lwr_60 <= Lwr_40 & Lwr_40 <= Lwr_20 &
          Lwr_20 <= Lwr_10 &
          Upr_10 <= Upr_20 & Upr_20 <= Upr_40 &
          Upr_40 <= Upr_60 & Upr_60 <= Upr_80 &
          Upr_80 <= Upr_95
        )
      ) %>%
      pull(x),
    nrow(anchored_selected) ==
      length(c("China", "Global")) *
      length(c(age_specific, "Overall ASIR (25-49y)")) *
      length(selected_projection_years),
    all(
      is.finite(anchored_selected$Anchored_Rate) &
      is.finite(anchored_selected$Anchored_Lwr_95) &
      is.finite(anchored_selected$Anchored_Upr_95)
    )
  )
) %>%
  mutate(Status = if_else(Passed, "PASS", "FAIL"))

write_csv(
  quality_checks,
  file.path(analysis_tables_dir, "ProjectionAnchor_projection_quality_checks.csv")
)

if (any(!quality_checks$Passed)) {
  warning(
    "At least one projection quality check failed. Review ",
    file.path(analysis_tables_dir, "ProjectionAnchor_projection_quality_checks.csv")
  )
}

# 5. Overall-ASIR comparison figure -------------------------------------------
overall_anchored <- df_final_fig5 %>%
  filter(age_name == "Overall ASIR (25-49y)", year >= 2023) %>%
  transmute(
    location_name,
    year,
    Scenario = "2023-anchored projection",
    Rate = Pred_Rate,
    Lwr_95,
    Upr_95,
    Observed_Rate
  )

overall_unanchored <- df_final_fig5_unanchored %>%
  filter(age_name == "Overall ASIR (25-49y)", year >= 2023) %>%
  transmute(
    location_name,
    year,
    Scenario = "Unanchored model output",
    Rate = Pred_Rate,
    Lwr_95,
    Upr_95,
    Observed_Rate
  )

overall_compare_plot_data <- bind_rows(overall_anchored, overall_unanchored)

p_projection_compare <- ggplot(
  overall_compare_plot_data,
  aes(x = year, y = Rate, linetype = Scenario)
) +
  geom_ribbon(
    data = overall_anchored,
    mapping = aes(x = year, ymin = Lwr_95, ymax = Upr_95),
    inherit.aes = FALSE,
    fill = "grey70",
    alpha = 0.30
  ) +
  geom_line(linewidth = 1.1, color = "black") +
  geom_point(
    data = overall_anchored %>% filter(year == 2023),
    aes(x = year, y = Observed_Rate),
    inherit.aes = FALSE,
    size = 2.7,
    color = "black"
  ) +
  geom_vline(xintercept = 2023, linetype = "dotted", linewidth = 0.8) +
  facet_wrap(~ location_name, scales = "free_y", ncol = 1) +
  scale_linetype_manual(
    values = c(
      "2023-anchored projection" = "solid",
      "Unanchored model output" = "dashed"
    )
  ) +
  scale_x_continuous(breaks = c(2023, 2030, 2040, 2050)) +
  labs(
    title = "Sensitivity analysis of the 2023 projection anchor",
    subtitle = "Solid lines are the anchored main projections; shaded bands are anchored 95% uncertainty intervals",
    x = "Year",
    y = "Overall incidence rate per 100,000",
    linetype = "Projection"
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.background = element_rect(fill = "grey95", color = "black"),
    strip.text = element_text(face = "bold"),
    legend.position = "top"
  )

ggsave(
  file.path(
    analysis_figures_dir,
    "ProjectionAnchor_anchored_vs_unanchored_projection.pdf"
  ),
  p_projection_compare,
  width = 9,
  height = 8,
  device = "pdf"
)
ggsave(
  file.path(
    analysis_figures_dir,
    "ProjectionAnchor_anchored_vs_unanchored_projection.png"
  ),
  p_projection_compare,
  width = 9,
  height = 8,
  dpi = 600,
  bg = "white"
)

# 6. Workbook -----------------------------------------------------------------
projection_workbook <- openxlsx::createWorkbook()
openxlsx::addWorksheet(projection_workbook, "Anchor_factors")
openxlsx::writeData(projection_workbook, "Anchor_factors", anchor_diagnostics)
openxlsx::addWorksheet(projection_workbook, "Selected_anchored")
openxlsx::writeData(projection_workbook, "Selected_anchored", projection_selected_95ui)
openxlsx::addWorksheet(projection_workbook, "Anchored_vs_unanchored")
openxlsx::writeData(
  projection_workbook,
  "Anchored_vs_unanchored",
  anchored_vs_unanchored
)
openxlsx::addWorksheet(projection_workbook, "Quality_checks")
openxlsx::writeData(projection_workbook, "Quality_checks", quality_checks)
openxlsx::saveWorkbook(
  projection_workbook,
  file.path(analysis_tables_dir, "ProjectionAnchor_projection_sensitivity.xlsx"),
  overwrite = TRUE
)

# 7. Explicit status-quo assumptions ------------------------------------------
status_quo_assumptions <- c(
  "BAPC projection assumptions",
  "",
  "1. Historical incidence counts and population denominators cover 1990-2023.",
  "2. The Bayesian age-period-cohort model contains an age effect, a second-order random-walk period effect, and a second-order random-walk cohort effect.",
  "3. The status-quo scenario extrapolates the fitted historical age, period, and cohort structure without imposing a new screening, treatment, prevention, or risk-factor intervention scenario.",
  "4. China female population denominators for 2024-2050 are taken from the UN World Population Prospects 2024 input file used by the pipeline.",
  "5. Global female population denominators for 2024-2050 are extrapolated separately by age group with the ARIMA procedure used in this pipeline.",
  "6. A location- and age-specific multiplicative ratio aligns the posterior mean at 2023 with the observed 2023 incidence rate and is applied to all posterior means and interval bounds from 2023 onward.",
  "7. The 95% uncertainty intervals quantify posterior uncertainty under the fitted model and specified population trajectories; they do not incorporate all uncertainty from GBD source estimation, population forecasts, alternative model structures, or future policy changes.",
  "8. Overall 25-49-year uncertainty limits are standardized weighted aggregations of age-specific posterior limits, consistent with the analytical pipeline."
)
writeLines(
  status_quo_assumptions,
  file.path(analysis_tables_dir, "ProjectionAnchor_status_quo_assumptions.txt")
)

writeLines(
  c(
    paste0("Completed: ", Sys.time()),
    paste0("Age-specific anchor series: ", nrow(anchor_diagnostics)),
    paste0("All projection checks passed: ", all(quality_checks$Passed)),
    "Main Figure 5 now uses explicit multiplicative 2023 anchoring."
  ),
  file.path(logs_dir, "projection_anchor_status.txt")
)

message("✓ BAPC anchoring and uncertainty diagnostics outputs exported.")

# ==============================================================================
# END EMBEDDED MODULE: 04_projection_anchor_sensitivity.R
# ==============================================================================


plot_dual_gradient <- function(data, title_text, center_title = FALSE) {
  ggplot(data, aes(x = year, color = location_name, fill = location_name)) +
    geom_ribbon(data = filter(data, year >= 2023), aes(ymin = Lwr_95, ymax = Upr_95), alpha = 0.05, color = NA) +
    geom_ribbon(data = filter(data, year >= 2023), aes(ymin = Lwr_80, ymax = Upr_80), alpha = 0.10, color = NA) +
    geom_ribbon(data = filter(data, year >= 2023), aes(ymin = Lwr_60, ymax = Upr_60), alpha = 0.15, color = NA) +
    geom_ribbon(data = filter(data, year >= 2023), aes(ymin = Lwr_40, ymax = Upr_40), alpha = 0.20, color = NA) +
    geom_ribbon(data = filter(data, year >= 2023), aes(ymin = Lwr_20, ymax = Upr_20), alpha = 0.25, color = NA) +
    geom_ribbon(data = filter(data, year >= 2023), aes(ymin = Lwr_10, ymax = Upr_10), alpha = 0.30, color = NA) +
    geom_point(data = filter(data, year <= 2023), aes(y = Observed_Rate), size = 1.0, alpha = 0.90) +
    geom_line(data = filter(data, year <= 2023), aes(y = Pred_Rate), linewidth = 1.2) +
    geom_line(data = filter(data, year >= 2023), aes(y = Pred_Rate), linetype = "dashed", linewidth = 1.2) +
    geom_vline(xintercept = 2023, linetype = "dotted", color = "grey35", linewidth = 1) +
    scale_x_continuous(breaks = c(1990, 2010, 2023, 2035, 2050), limits = c(1989, 2051)) +
    scale_color_manual(values = c("China" = color_china, "Global" = color_global), name = "Location") +
    scale_fill_manual(values = c("China" = color_china, "Global" = color_global), name = "Location") +
    labs(title = title_text, x = "", y = "Incidence Rate (per 100k)") +
    theme_classic(base_size = 14) +
    theme(
      strip.background = element_rect(fill = "#F2F2F2", color = "black", linewidth = 1),
      strip.text = element_text(face = "bold", size = 12, color = "black"),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
      axis.line = element_blank(),
      axis.text = element_text(color = "black", size = 11),
      axis.title = element_text(color = "black", size = 14),
      plot.title = element_text(face = "bold", size = 16, hjust = ifelse(center_title, 0.5, 0)),
      legend.position = "top",
      legend.title = element_text(face = "bold", size = 12),
      legend.text = element_text(size = 11)
    )
}

p_partA_f5 <- plot_dual_gradient(
  df_final_fig5 %>%
    filter(age_name != "Overall ASIR (25-49y)") %>%
    mutate(age_name = factor(age_name, levels = age_specific)),
  "A. Age–Specific Incidence Rates Projections to 2050"
) +
  facet_wrap(~ age_name, ncol = 3, scales = "free_y")

p_partB_f5 <- plot_dual_gradient(
  df_final_fig5 %>% filter(age_name == "Overall ASIR (25-49y)"),
  "B. Overall ASIR Projection (25–49y)",
  center_title = TRUE
)

fig5_final <- p_partA_f5 / p_partB_f5 +
  plot_layout(heights = c(2, 1.2), guides = "collect") &
  theme(legend.position = "top", legend.box = "horizontal")

save_plot_both(fig5_final, "Figure 5", width = 15, height = 12)

# ------------------------------------------------------------------------------
# 10. Figure 6: GBD and GLOBOCAN external comparison
# ------------------------------------------------------------------------------

message("▶ Building Figure 6...")

# GLOBOCAN 2022 age-specific case estimates downloaded from Cancer Today,
# Global Cancer Observatory. These ten values are retained inline so this
# standalone release preserves the validated v1.1.6 internal schema.
# They are used only to derive comparison rates with the matched study
# population denominators; they are not official GLOBOCAN incidence rates.
df_globo_raw <- tibble::tribble(
  ~location_name, ~age_name,       ~Cases_2022,
  "China",        "25-29 years",   2675,
  "China",        "30-34 years",  10405,
  "China",        "35-39 years",  18603,
  "China",        "40-44 years",  29963,
  "China",        "45-49 years",  49686,
  "Global",       "25-29 years",  33777,
  "Global",       "30-34 years",  76407,
  "Global",       "35-39 years", 128734,
  "Global",       "40-44 years", 183276,
  "Global",       "45-49 years", 239314
)

if (nrow(df_globo_raw) != 10L) {
  stop(
    "Expected 10 GLOBOCAN age-location case rows, but found ",
    nrow(df_globo_raw), "."
  )
}

df_pop_2022 <- df_bapc_all %>%
  filter(year == 2022) %>%
  select(location_name, age_name, Population)

df_globo_val <- df_globo_raw %>%
  inner_join(df_pop_2022, by = c("location_name", "age_name")) %>%
  mutate(
    GLOBOCAN_Rate = (Cases_2022 / Population) * 100000
  )

df_gbd_val <- df_bapc_all %>%
  filter(year == 2022, location_name %in% c("China", "Global")) %>%
  mutate(GBD_Rate = (Cases / Population) * 100000) %>%
  select(location_name, age_name, GBD_Rate)

df_scatter <- df_globo_val %>%
  inner_join(df_gbd_val, by = c("location_name", "age_name"))

if (nrow(df_scatter) != 10L) {
  stop(
    "Expected 10 matched GBD-GLOBOCAN age-location rows, but found ",
    nrow(df_scatter), "."
  )
}

df_validation <- df_scatter %>%
  pivot_longer(
    cols = c("GLOBOCAN_Rate", "GBD_Rate"),
    names_to = "Source",
    values_to = "Rate"
  ) %>%
  mutate(
    Source = ifelse(
      Source == "GBD_Rate",
      "GBD estimate",
      "Rate derived from GLOBOCAN case estimates"
    )
  )

pearson_r <- cor(
  df_scatter$GLOBOCAN_Rate,
  df_scatter$GBD_Rate,
  use = "complete.obs"
)

df_validation_table <- df_scatter %>%
  mutate(
    Absolute_Difference = GBD_Rate - GLOBOCAN_Rate,
    Relative_Difference_Percent =
      100 * (GBD_Rate - GLOBOCAN_Rate) / GLOBOCAN_Rate
  ) %>%
  # Retain the validated internal column schema used by the downstream sections.
  # 05-09. Module 10 rewrites the manuscript-facing table with precise labels.
  select(
    Location = location_name,
    `Age group` = age_name,
    `GLOBOCAN cases 2022` = Cases_2022,
    Population,
    `GLOBOCAN rate` = GLOBOCAN_Rate,
    `GBD rate` = GBD_Rate,
    `Absolute difference` = Absolute_Difference,
    `Relative difference (%)` = Relative_Difference_Percent
  ) %>%
  arrange(Location, `Age group`) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

write_csv(
  df_validation_table,
  file.path(
    tables_dir,
    "Supplementary_Table_S1_GBD_vs_GLOBOCAN_2022.csv"
  )
)

p_val_bar <- ggplot(
  df_validation,
  aes(x = age_name, y = Rate, fill = Source)
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7,
    color = "black"
  ) +
  facet_wrap(~ location_name, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = c(
    "GBD estimate" = "#E64B35",
    "Rate derived from GLOBOCAN case estimates" = "#4DBBD5"
  )) +
  labs(
    title = paste(
      "A. GBD rates and rates derived from",
      "GLOBOCAN 2022 case estimates"
    ),
    x = "Age group",
    y = "Incidence rate per 100,000",
    fill = ""
  ) +
  theme_pub(13) +
  theme(legend.position = "top")

p_val_scatter <- ggplot(
  df_scatter,
  aes(x = GLOBOCAN_Rate, y = GBD_Rate, color = location_name)
) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed",
    color = "grey55",
    linewidth = 0.8
  ) +
  geom_point(size = 3.2, alpha = 0.85) +
  annotate(
    "text",
    x = min(df_scatter$GLOBOCAN_Rate, na.rm = TRUE),
    y = max(df_scatter$GBD_Rate, na.rm = TRUE),
    label = paste0("Pearson r = ", round(pearson_r, 3)),
    hjust = 0,
    vjust = 1,
    size = 5
  ) +
  scale_color_manual(
    values = c("China" = color_china, "Global" = color_global)
  ) +
  labs(
    title = "B. Concordance analysis",
    x = "Rate derived from GLOBOCAN case estimates per 100,000",
    y = "GBD incidence rate per 100,000",
    color = "Location"
  ) +
  theme_pub(13) +
  theme(legend.position = "top")

fig_s1 <- p_val_bar | p_val_scatter +
  plot_layout(widths = c(1.5, 1))

save_plot_both(fig_s1, "Figure 6", width = 16, height = 8)

# Evaluate an external-baseline sensitivity scenario using GLOBOCAN 2022.
# The GBD-based projection remains the primary analysis.
# ==============================================================================
# BEGIN EMBEDDED MODULE: 05_globocan_baseline_sensitivity.R
# ==============================================================================

# ==============================================================================
# GLOBOCAN 2022 external-baseline sensitivity analysis
# ==============================================================================

message("▶ GLOBOCAN 2022 external-baseline sensitivity analysis...")

required_globocan_objects <- c(
  "df_scatter", "df_pred_all", "gbd_weights", "age_specific",
  "analysis_tables_dir", "analysis_figures_dir", "color_china",
  "color_global"
)
missing_globocan_objects <- required_globocan_objects[
  !vapply(required_globocan_objects, exists, logical(1), inherits = TRUE)
]
if (length(missing_globocan_objects) > 0) {
  stop(
    "GLOBOCAN baseline-scaling module is missing required objects: ",
    paste(missing_globocan_objects, collapse = ", ")
  )
}

selected_projection_years_step5 <- c(2023, 2030, 2040, 2050)

# 1. Derive external baseline-scaling factors --------------------------------------
globocan_baseline_scaling_factors <- df_scatter %>%
  transmute(
    location_name,
    age_name = as.character(age_name),
    Cases_2022,
    Population_2022 = Population,
    GLOBOCAN_Rate_2022 = as.numeric(GLOBOCAN_Rate),
    GBD_Rate_2022 = as.numeric(GBD_Rate),
    Baseline_Scaling_Factor = GLOBOCAN_Rate_2022 / GBD_Rate_2022,
    Absolute_Difference = GLOBOCAN_Rate_2022 - GBD_Rate_2022,
    GBD_Relative_to_GLOBOCAN_Percent =
      100 * (GBD_Rate_2022 - GLOBOCAN_Rate_2022) / GLOBOCAN_Rate_2022,
    Baseline_Scaling_Change_Percent = 100 * (Baseline_Scaling_Factor - 1)
  ) %>%
  arrange(location_name, factor(age_name, levels = age_specific))

invalid_baseline_scaling_factor <- globocan_baseline_scaling_factors %>%
  summarise(
    invalid = any(
      !is.finite(Baseline_Scaling_Factor) |
        Baseline_Scaling_Factor <= 0 |
        !is.finite(GLOBOCAN_Rate_2022) |
        !is.finite(GBD_Rate_2022) |
        GLOBOCAN_Rate_2022 <= 0 |
        GBD_Rate_2022 <= 0
    )
  ) %>%
  pull(invalid)

if (isTRUE(invalid_baseline_scaling_factor)) {
  stop("At least one GLOBOCAN-to-GBD baseline-scaling factor is invalid or non-positive.")
}

write_csv(
  globocan_baseline_scaling_factors,
  file.path(
    analysis_tables_dir,
    "GLOBOCANBaseline_GLOBOCAN_baseline_scaling_factors.csv"
  )
)

# 2. Apply ratios to the main age-specific projection -------------------------
# The 2022 factor is held constant through 2050. This changes the baseline level
# only; it does not impose a different age-period-cohort trend.
globocan_baseline_scaled_age_specific <- df_pred_all %>%
  filter(location_name %in% c("China", "Global"), year >= 2023) %>%
  left_join(
    globocan_baseline_scaling_factors %>%
      select(location_name, age_name, Baseline_Scaling_Factor),
    by = c("location_name", "age_name")
  ) %>%
  mutate(
    Scenario = "GLOBOCAN-2022 baseline-scaled sensitivity",
    Main_GBD_Rate = Pred_Rate,
    Main_GBD_Lwr_95 = Lwr_95,
    Main_GBD_Upr_95 = Upr_95,
    Baseline_Scaled_Rate = Pred_Rate * Baseline_Scaling_Factor,
    Baseline_Scaled_Lwr_95 = Lwr_95 * Baseline_Scaling_Factor,
    Baseline_Scaled_Upr_95 = Upr_95 * Baseline_Scaling_Factor,
    Baseline_Scaled_Lwr_80 = Lwr_80 * Baseline_Scaling_Factor,
    Baseline_Scaled_Upr_80 = Upr_80 * Baseline_Scaling_Factor,
    Baseline_Scaled_Lwr_60 = Lwr_60 * Baseline_Scaling_Factor,
    Baseline_Scaled_Upr_60 = Upr_60 * Baseline_Scaling_Factor,
    Baseline_Scaled_Lwr_40 = Lwr_40 * Baseline_Scaling_Factor,
    Baseline_Scaled_Upr_40 = Upr_40 * Baseline_Scaling_Factor,
    Baseline_Scaled_Lwr_20 = Lwr_20 * Baseline_Scaling_Factor,
    Baseline_Scaled_Upr_20 = Upr_20 * Baseline_Scaling_Factor,
    Baseline_Scaled_Lwr_10 = Lwr_10 * Baseline_Scaling_Factor,
    Baseline_Scaled_Upr_10 = Upr_10 * Baseline_Scaling_Factor
  )

if (any(is.na(globocan_baseline_scaled_age_specific$Baseline_Scaling_Factor))) {
  stop("Some projected age-specific series did not receive a GLOBOCAN baseline-scaling factor.")
}

# 3. Recalculate the overall 25-49-year ASIR -----------------------------------
globocan_baseline_scaled_overall <- globocan_baseline_scaled_age_specific %>%
  left_join(gbd_weights, by = "age_name") %>%
  group_by(location_name, year) %>%
  summarise(
    Main_GBD_Rate = sum(Main_GBD_Rate * weight, na.rm = TRUE) / sum(weight),
    Main_GBD_Lwr_95 = sum(Main_GBD_Lwr_95 * weight, na.rm = TRUE) / sum(weight),
    Main_GBD_Upr_95 = sum(Main_GBD_Upr_95 * weight, na.rm = TRUE) / sum(weight),
    Baseline_Scaled_Rate = sum(Baseline_Scaled_Rate * weight, na.rm = TRUE) / sum(weight),
    Baseline_Scaled_Lwr_95 = sum(Baseline_Scaled_Lwr_95 * weight, na.rm = TRUE) / sum(weight),
    Baseline_Scaled_Upr_95 = sum(Baseline_Scaled_Upr_95 * weight, na.rm = TRUE) / sum(weight),
    Baseline_Scaled_Lwr_80 = sum(Baseline_Scaled_Lwr_80 * weight, na.rm = TRUE) / sum(weight),
    Baseline_Scaled_Upr_80 = sum(Baseline_Scaled_Upr_80 * weight, na.rm = TRUE) / sum(weight),
    Baseline_Scaled_Lwr_60 = sum(Baseline_Scaled_Lwr_60 * weight, na.rm = TRUE) / sum(weight),
    Baseline_Scaled_Upr_60 = sum(Baseline_Scaled_Upr_60 * weight, na.rm = TRUE) / sum(weight),
    Baseline_Scaled_Lwr_40 = sum(Baseline_Scaled_Lwr_40 * weight, na.rm = TRUE) / sum(weight),
    Baseline_Scaled_Upr_40 = sum(Baseline_Scaled_Upr_40 * weight, na.rm = TRUE) / sum(weight),
    Baseline_Scaled_Lwr_20 = sum(Baseline_Scaled_Lwr_20 * weight, na.rm = TRUE) / sum(weight),
    Baseline_Scaled_Upr_20 = sum(Baseline_Scaled_Upr_20 * weight, na.rm = TRUE) / sum(weight),
    Baseline_Scaled_Lwr_10 = sum(Baseline_Scaled_Lwr_10 * weight, na.rm = TRUE) / sum(weight),
    Baseline_Scaled_Upr_10 = sum(Baseline_Scaled_Upr_10 * weight, na.rm = TRUE) / sum(weight),
    .groups = "drop"
  ) %>%
  mutate(
    age_name = "Overall ASIR (25-49y)",
    Baseline_Scaling_Factor = Baseline_Scaled_Rate / Main_GBD_Rate,
    Scenario = "GLOBOCAN-2022 baseline-scaled sensitivity"
  )

# 4. Create selected-year comparison tables -----------------------------------
age_specific_selected_step5 <- globocan_baseline_scaled_age_specific %>%
  filter(year %in% selected_projection_years_step5) %>%
  transmute(
    location_name,
    age_name = as.character(age_name),
    year,
    Baseline_Scaling_Factor,
    Main_GBD_Rate,
    Main_GBD_Lwr_95,
    Main_GBD_Upr_95,
    Baseline_Scaled_Rate,
    Baseline_Scaled_Lwr_95,
    Baseline_Scaled_Upr_95
  )

overall_selected_step5 <- globocan_baseline_scaled_overall %>%
  filter(year %in% selected_projection_years_step5) %>%
  transmute(
    location_name,
    age_name,
    year,
    Baseline_Scaling_Factor,
    Main_GBD_Rate,
    Main_GBD_Lwr_95,
    Main_GBD_Upr_95,
    Baseline_Scaled_Rate,
    Baseline_Scaled_Lwr_95,
    Baseline_Scaled_Upr_95
  )

main_vs_globocan_baseline_scaled <- bind_rows(
  age_specific_selected_step5,
  overall_selected_step5
) %>%
  mutate(
    Absolute_Change = Baseline_Scaled_Rate - Main_GBD_Rate,
    Relative_Change_Percent = 100 * Absolute_Change / Main_GBD_Rate,
    Main_GBD_95UI = paste0(
      round(Main_GBD_Rate, 2), " (",
      round(Main_GBD_Lwr_95, 2), " to ",
      round(Main_GBD_Upr_95, 2), ")"
    ),
    GLOBOCAN_Baseline_Scaled_95UI = paste0(
      round(Baseline_Scaled_Rate, 2), " (",
      round(Baseline_Scaled_Lwr_95, 2), " to ",
      round(Baseline_Scaled_Upr_95, 2), ")"
    )
  ) %>%
  arrange(location_name, factor(age_name, levels = c(age_specific, "Overall ASIR (25-49y)")), year)

write_csv(
  main_vs_globocan_baseline_scaled,
  file.path(
    analysis_tables_dir,
    "GLOBOCANBaseline_main_vs_GLOBOCAN_baseline_scaled_selected_years.csv"
  )
)

manuscript_ready_baseline_scaled <- main_vs_globocan_baseline_scaled %>%
  transmute(
    Location = location_name,
    `Age group` = age_name,
    Year = year,
    `Baseline-scaling factor` = Baseline_Scaling_Factor,
    `Main GBD-based projection per 100,000 (95% UI)` = Main_GBD_95UI,
    `Case-estimate-based baseline-scaled sensitivity per 100,000 (95% UI)` =
      GLOBOCAN_Baseline_Scaled_95UI,
    `Relative change after baseline scaling (%)` = Relative_Change_Percent
  )

write_csv(
  manuscript_ready_baseline_scaled,
  file.path(
    analysis_tables_dir,
    "GLOBOCANBaseline_GLOBOCAN_baseline_scaled_projection_selected_years.csv"
  )
)

# 5. Quality checks ------------------------------------------------------------
baseline_scaled_future <- globocan_baseline_scaled_age_specific

quality_checks_step5 <- tibble(
  Check = c(
    "Baseline-scaling factors available for both locations and all five age groups",
    "All baseline-scaling factors are finite and positive",
    "Applying each factor to the 2022 GBD rate reproduces the GLOBOCAN rate",
    "No negative baseline-scaled posterior means or 95% bounds",
    "All baseline-scaled 95% intervals have correctly ordered bounds",
    "All baseline-scaled fan-chart intervals are properly nested",
    "Selected years are complete for both locations and all series",
    "All selected-year baseline-scaled estimates are finite"
  ),
  Passed = c(
    nrow(globocan_baseline_scaling_factors) ==
      length(c("China", "Global")) * length(age_specific),
    all(
      is.finite(globocan_baseline_scaling_factors$Baseline_Scaling_Factor) &
        globocan_baseline_scaling_factors$Baseline_Scaling_Factor > 0
    ),
    globocan_baseline_scaling_factors %>%
      summarise(
        x = all(
          abs(GBD_Rate_2022 * Baseline_Scaling_Factor - GLOBOCAN_Rate_2022) < 1e-8
        )
      ) %>%
      pull(x),
    baseline_scaled_future %>%
      summarise(
        x = all(
          Baseline_Scaled_Rate >= 0 &
            Baseline_Scaled_Lwr_95 >= 0 &
            Baseline_Scaled_Upr_95 >= 0
        )
      ) %>%
      pull(x),
    baseline_scaled_future %>%
      summarise(x = all(Baseline_Scaled_Lwr_95 <= Baseline_Scaled_Upr_95)) %>%
      pull(x),
    baseline_scaled_future %>%
      summarise(
        x = all(
          Baseline_Scaled_Lwr_95 <= Baseline_Scaled_Lwr_80 &
            Baseline_Scaled_Lwr_80 <= Baseline_Scaled_Lwr_60 &
            Baseline_Scaled_Lwr_60 <= Baseline_Scaled_Lwr_40 &
            Baseline_Scaled_Lwr_40 <= Baseline_Scaled_Lwr_20 &
            Baseline_Scaled_Lwr_20 <= Baseline_Scaled_Lwr_10 &
            Baseline_Scaled_Upr_10 <= Baseline_Scaled_Upr_20 &
            Baseline_Scaled_Upr_20 <= Baseline_Scaled_Upr_40 &
            Baseline_Scaled_Upr_40 <= Baseline_Scaled_Upr_60 &
            Baseline_Scaled_Upr_60 <= Baseline_Scaled_Upr_80 &
            Baseline_Scaled_Upr_80 <= Baseline_Scaled_Upr_95
        )
      ) %>%
      pull(x),
    nrow(main_vs_globocan_baseline_scaled) ==
      length(c("China", "Global")) *
      length(c(age_specific, "Overall ASIR (25-49y)")) *
      length(selected_projection_years_step5),
    all(
      is.finite(main_vs_globocan_baseline_scaled$Baseline_Scaled_Rate) &
        is.finite(main_vs_globocan_baseline_scaled$Baseline_Scaled_Lwr_95) &
        is.finite(main_vs_globocan_baseline_scaled$Baseline_Scaled_Upr_95)
    )
  )
) %>%
  mutate(Status = if_else(Passed, "PASS", "FAIL"))

write_csv(
  quality_checks_step5,
  file.path(
    analysis_tables_dir,
    "GLOBOCANBaseline_GLOBOCAN_baseline_scaling_quality_checks.csv"
  )
)

if (any(!quality_checks_step5$Passed)) {
  warning(
    "At least one GLOBOCAN baseline-scaling quality check failed. Review ",
    file.path(
      analysis_tables_dir,
      "GLOBOCANBaseline_GLOBOCAN_baseline_scaling_quality_checks.csv"
    )
  )
}

# 6. Sensitivity figure --------------------------------------------------------
ratio_plot_data <- globocan_baseline_scaling_factors %>%
  filter(location_name == "China") %>%
  mutate(age_name = factor(age_name, levels = age_specific))

p_ratio_step5 <- ggplot(
  ratio_plot_data,
  aes(x = age_name, y = Baseline_Scaling_Factor)
) +
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.8) +
  geom_col(width = 0.68, fill = color_china, color = "black") +
  geom_text(
    aes(label = paste0(round(Baseline_Scaling_Change_Percent, 1), "%")),
    vjust = -0.35,
    size = 4
  ) +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.1),
    expand = expansion(mult = c(0, 0.13))
  ) +
  labs(
    title = "A. China age-specific baseline-scaling factors derived from GLOBOCAN 2022 case estimates",
    subtitle = "Labels show the percentage change implied by the case-estimate-derived/GBD baseline ratio",
    x = "Age group",
    y = "Case-estimate-derived rate / GBD rate"
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 25, hjust = 1)
  )

china_overall_step5 <- globocan_baseline_scaled_overall %>%
  filter(location_name == "China")

china_main_long_step5 <- china_overall_step5 %>%
  transmute(
    year,
    Scenario = "Main GBD-based projection",
    Rate = Main_GBD_Rate,
    Lwr_95 = Main_GBD_Lwr_95,
    Upr_95 = Main_GBD_Upr_95
  )

china_baseline_scaled_long_step5 <- china_overall_step5 %>%
  transmute(
    year,
    Scenario = "Case-estimate-based baseline-scaling sensitivity",
    Rate = Baseline_Scaled_Rate,
    Lwr_95 = Baseline_Scaled_Lwr_95,
    Upr_95 = Baseline_Scaled_Upr_95
  )

china_projection_long_step5 <- bind_rows(
  china_main_long_step5,
  china_baseline_scaled_long_step5
)

p_projection_step5 <- ggplot(
  china_projection_long_step5,
  aes(x = year, y = Rate, linetype = Scenario)
) +
  geom_ribbon(
    data = china_baseline_scaled_long_step5,
    aes(x = year, ymin = Lwr_95, ymax = Upr_95),
    inherit.aes = FALSE,
    fill = "grey70",
    alpha = 0.30
  ) +
  geom_line(color = "black", linewidth = 1.1) +
  geom_vline(xintercept = 2023, linetype = "dotted", linewidth = 0.8) +
  scale_linetype_manual(
    values = c(
      "Main GBD-based projection" = "solid",
      "Case-estimate-based baseline-scaling sensitivity" = "dashed"
    )
  ) +
  scale_x_continuous(breaks = c(2023, 2030, 2040, 2050)) +
  labs(
    title = "B. China overall incidence projection under external-baseline scaling",
    subtitle = "Each 2022 age-specific ratio is held constant through 2050; scaling-factor uncertainty is not propagated",
    x = "Year",
    y = "Overall incidence rate per 100,000",
    linetype = "Projection"
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "top"
  )

p_step5 <- p_ratio_step5 / p_projection_step5 +
  patchwork::plot_layout(heights = c(1, 1.25)) +
  patchwork::plot_annotation(
    title = "Sensitivity analysis using rates derived from GLOBOCAN 2022 case estimates as an external incidence baseline"
  )

ggsave(
  file.path(
    analysis_figures_dir,
    "GLOBOCANBaseline_GLOBOCAN_baseline_scaled_projection.pdf"
  ),
  p_step5,
  width = 10,
  height = 10,
  device = "pdf"
)
ggsave(
  file.path(
    analysis_figures_dir,
    "GLOBOCANBaseline_GLOBOCAN_baseline_scaled_projection.png"
  ),
  p_step5,
  width = 10,
  height = 10,
  dpi = 600,
  bg = "white"
)

# 7. Workbook -----------------------------------------------------------------
globocan_workbook <- openxlsx::createWorkbook()
openxlsx::addWorksheet(globocan_workbook, "Baseline_scaling_factors")
openxlsx::writeData(
  globocan_workbook,
  "Baseline_scaling_factors",
  globocan_baseline_scaling_factors
)
openxlsx::addWorksheet(globocan_workbook, "Selected_comparison")
openxlsx::writeData(
  globocan_workbook,
  "Selected_comparison",
  main_vs_globocan_baseline_scaled
)
openxlsx::addWorksheet(globocan_workbook, "Manuscript_ready")
openxlsx::writeData(
  globocan_workbook,
  "Manuscript_ready",
  manuscript_ready_baseline_scaled
)
openxlsx::addWorksheet(globocan_workbook, "Quality_checks")
openxlsx::writeData(
  globocan_workbook,
  "Quality_checks",
  quality_checks_step5
)
openxlsx::saveWorkbook(
  globocan_workbook,
  file.path(
    analysis_tables_dir,
    "GLOBOCANBaseline_GLOBOCAN_baseline_scaling_sensitivity.xlsx"
  ),
  overwrite = TRUE
)

# 8. Explicit assumptions and interpretation limits ---------------------------
globocan_assumptions <- c(
  "GLOBOCAN 2022 external-baseline scaling sensitivity analysis",
  "",
  "1. The GBD-based, 2023-anchored BAPC projection remains the prespecified main analysis.",
  "2. GLOBOCAN 2022 age-specific incidence case estimates are converted to rates using the same 2022 population denominators used in the external comparison.",
  "3. For each location and five-year age group, the sensitivity factor is the 2022 rate derived from GLOBOCAN case estimates divided by the corresponding 2022 GBD incidence rate.",
  "4. Each age-specific factor is held constant and applied to the main posterior mean and interval bounds from 2023 through 2050.",
  "5. The sensitivity analysis changes the incidence baseline level but preserves the relative age-period-cohort trajectory estimated from GBD data.",
  "6. The scaled 95% uncertainty intervals retain the relative posterior width of the main model; they do not quantify uncertainty in the GLOBOCAN/GBD baseline-scaling factor itself.",
  "7. This scenario is not an independent GLOBOCAN-based forecast and does not assume that the 2022 difference between the two sources will necessarily persist unchanged through 2050.",
  "8. Results are interpreted only as an indication of how a higher external baseline could affect the projected magnitude, not as a replacement for the main projection."
)
writeLines(
  globocan_assumptions,
  file.path(
    analysis_tables_dir,
    "GLOBOCANBaseline_GLOBOCAN_baseline_scaling_assumptions.txt"
  )
)

message("✓ GLOBOCAN 2022 external-baseline sensitivity analysis outputs exported.")

# ==============================================================================
# END EMBEDDED MODULE: 05_globocan_baseline_sensitivity.R
# ==============================================================================


# Evaluate Human Capital Approach assumptions and productivity-value sensitivity.
# The GDP-per-capita × age-specific female LFPR estimate remains primary.
# ==============================================================================
# BEGIN EMBEDDED MODULE: 06_hca_sensitivity.R
# ==============================================================================

# ==============================================================================
# Human Capital Approach sensitivity analysis
# ==============================================================================

message("▶ Human Capital Approach sensitivity analysis...")

required_objects_step6 <- c(
  "df_econ_base", "df_econ_master", "df_paf_all", "df_yll_age",
  "df_gdp", "df_ilo", "age_specific", "total_loss_sum",
  "analysis_tables_dir", "analysis_figures_dir"
)

missing_objects_step6 <- required_objects_step6[
  !vapply(required_objects_step6, exists, logical(1), inherits = TRUE)
]

if (length(missing_objects_step6) > 0) {
  stop(
    "Human Capital Approach sensitivity analysis is missing required object(s): ",
    paste(missing_objects_step6, collapse = ", ")
  )
}

# ------------------------------------------------------------------------------
# 1. Input completeness and structural audit
# ------------------------------------------------------------------------------

hca_audit_by_age <- df_econ_base %>%
  mutate(age_name = as.character(age_name)) %>%
  group_by(age_name) %>%
  summarise(
    n_rows = n(),
    first_year = min(year, na.rm = TRUE),
    last_year = max(year, na.rm = TRUE),
    missing_YLL = sum(is.na(Total_YLLs)),
    missing_GDP = sum(is.na(GDP_per_capita)),
    missing_LFPR = sum(is.na(LFPR)),
    min_LFPR = min(LFPR, na.rm = TRUE),
    max_LFPR = max(LFPR, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    age_name = factor(age_name, levels = age_specific)
  ) %>%
  arrange(age_name) %>%
  mutate(age_name = as.character(age_name))

hca_duplicate_check <- df_econ_base %>%
  count(year, age_name, name = "n") %>%
  filter(n != 1)

hca_2023_main_age <- df_econ_base %>%
  filter(year == 2023) %>%
  transmute(
    age_name = as.character(age_name),
    Total_YLLs,
    GDP_per_capita,
    LFPR,
    Main_Loss_Billion = Total_Loss_Billion
  ) %>%
  mutate(
    age_name = factor(age_name, levels = age_specific)
  ) %>%
  arrange(age_name) %>%
  mutate(age_name = as.character(age_name))

# ------------------------------------------------------------------------------
# 2. Define transparent scenario bounds
# ------------------------------------------------------------------------------

# This raw profile represents an illustrative life-course earnings pattern:
# lower at ages 25-29, rising into mid-career, then broadly plateauing.
# It is normalized below so that its 2023 loss-weighted mean equals 1.0.
age_profile_raw <- tibble(
  age_name = age_specific,
  raw_age_multiplier = c(0.75, 0.90, 1.05, 1.10, 1.00)
)

normalization_denominator <- hca_2023_main_age %>%
  left_join(age_profile_raw, by = "age_name") %>%
  summarise(
    weighted_raw = sum(Main_Loss_Billion * raw_age_multiplier, na.rm = TRUE) /
      sum(Main_Loss_Billion, na.rm = TRUE)
  ) %>%
  pull(weighted_raw)

if (!is.finite(normalization_denominator) || normalization_denominator <= 0) {
  stop("Could not normalize the illustrative age-productivity profile.")
}

age_profile <- age_profile_raw %>%
  mutate(
    normalized_age_multiplier = raw_age_multiplier / normalization_denominator
  )

scenario_definitions <- bind_rows(
  tibble(
    Scenario = "Main HCA estimate",
    Monetary_Scale = 1.00,
    Age_Profile = "Flat across age groups",
    Interpretation = paste(
      "Prespecified main estimate using GDP per capita and",
      "age-specific female labour-force participation."
    )
  ),
  tibble(
    Scenario = "Uniform 20% lower productivity value",
    Monetary_Scale = 0.80,
    Age_Profile = "Flat across age groups",
    Interpretation = paste(
      "Lower-bound scenario in which the monetary value of each",
      "productive life-year is 20% below GDP per capita."
    )
  ),
  tibble(
    Scenario = "Uniform 20% higher productivity value",
    Monetary_Scale = 1.20,
    Age_Profile = "Flat across age groups",
    Interpretation = paste(
      "Upper-bound scenario in which the monetary value of each",
      "productive life-year is 20% above GDP per capita."
    )
  ),
  tibble(
    Scenario = "Age-graded profile, total-normalized",
    Monetary_Scale = 1.00,
    Age_Profile = "Illustrative normalized age profile",
    Interpretation = paste(
      "Redistributes the 2023 loss across age groups while preserving",
      "the overall 2023 monetary scale."
    )
  ),
  tibble(
    Scenario = "Age-graded profile plus 20% lower value",
    Monetary_Scale = 0.80,
    Age_Profile = "Illustrative normalized age profile",
    Interpretation = paste(
      "Combines age grading with a 20% lower monetary value."
    )
  ),
  tibble(
    Scenario = "Age-graded profile plus 20% higher value",
    Monetary_Scale = 1.20,
    Age_Profile = "Illustrative normalized age profile",
    Interpretation = paste(
      "Combines age grading with a 20% higher monetary value."
    )
  )
)

scenario_age_multipliers <- tidyr::crossing(
  Scenario = scenario_definitions$Scenario,
  age_name = age_specific
) %>%
  left_join(
    scenario_definitions %>% select(Scenario, Monetary_Scale, Age_Profile),
    by = "Scenario"
  ) %>%
  left_join(age_profile, by = "age_name") %>%
  mutate(
    Age_Multiplier = case_when(
      Age_Profile == "Flat across age groups" ~ 1,
      TRUE ~ normalized_age_multiplier
    ),
    Combined_Multiplier = Monetary_Scale * Age_Multiplier
  ) %>%
  select(
    Scenario, age_name, Monetary_Scale,
    Age_Multiplier, Combined_Multiplier
  )

# Optional empirical profile:
# Users may place a file named china_female_age_earnings_profile.csv in data/
# with columns:
#   age_name
#   relative_multiplier
# The profile will be included as an additional sensitivity scenario.
optional_profile_path <- file.path(
  input_path,
  "china_female_age_earnings_profile.csv"
)

optional_profile_used <- FALSE

if (file.exists(optional_profile_path)) {
  empirical_profile <- readr::read_csv(
    optional_profile_path,
    show_col_types = FALSE
  )

  required_profile_cols <- c("age_name", "relative_multiplier")
  missing_profile_cols <- setdiff(
    required_profile_cols,
    names(empirical_profile)
  )

  if (length(missing_profile_cols) > 0) {
    stop(
      "Optional age-earnings profile is missing columns: ",
      paste(missing_profile_cols, collapse = ", ")
    )
  }

  empirical_profile <- empirical_profile %>%
    transmute(
      age_name = as.character(age_name),
      relative_multiplier = as.numeric(relative_multiplier)
    ) %>%
    filter(age_name %in% age_specific)

  if (
    nrow(empirical_profile) != length(age_specific) ||
    any(!is.finite(empirical_profile$relative_multiplier)) ||
    any(empirical_profile$relative_multiplier <= 0) ||
    anyDuplicated(empirical_profile$age_name)
  ) {
    stop(
      "Optional age-earnings profile must contain one positive finite ",
      "multiplier for each of the five age groups."
    )
  }

  empirical_norm <- hca_2023_main_age %>%
    left_join(empirical_profile, by = "age_name") %>%
    summarise(
      weighted_multiplier =
        sum(Main_Loss_Billion * relative_multiplier, na.rm = TRUE) /
        sum(Main_Loss_Billion, na.rm = TRUE)
    ) %>%
    pull(weighted_multiplier)

  empirical_profile <- empirical_profile %>%
    mutate(
      normalized_multiplier = relative_multiplier / empirical_norm
    )

  scenario_definitions <- bind_rows(
    scenario_definitions,
    tibble(
      Scenario = "User-supplied age-sex earnings profile, total-normalized",
      Monetary_Scale = 1.00,
      Age_Profile = "User-supplied empirical profile",
      Interpretation = paste(
        "Uses the optional user-supplied relative age-by-sex earnings",
        "profile and normalizes it to preserve the overall 2023 scale."
      )
    )
  )

  scenario_age_multipliers <- bind_rows(
    scenario_age_multipliers,
    empirical_profile %>%
      transmute(
        Scenario =
          "User-supplied age-sex earnings profile, total-normalized",
        age_name,
        Monetary_Scale = 1.00,
        Age_Multiplier = normalized_multiplier,
        Combined_Multiplier = normalized_multiplier
      )
  )

  optional_profile_used <- TRUE
}

# ------------------------------------------------------------------------------
# 3. Calculate scenario results for all years and for 2023
# ------------------------------------------------------------------------------

hca_scenario_all_years <- df_econ_base %>%
  mutate(age_name = as.character(age_name)) %>%
  inner_join(scenario_age_multipliers, by = "age_name") %>%
  mutate(
    Scenario_Loss_Billion =
      Total_Loss_Billion * Combined_Multiplier
  )

hca_scenario_totals <- hca_scenario_all_years %>%
  group_by(year, Scenario) %>%
  summarise(
    Total_Loss_Billion = sum(Scenario_Loss_Billion, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(year) %>%
  mutate(
    Main_Loss_Billion =
      Total_Loss_Billion[Scenario == "Main HCA estimate"][1],
    Absolute_Change_Billion =
      Total_Loss_Billion - Main_Loss_Billion,
    Relative_Change_Percent =
      100 * (Total_Loss_Billion / Main_Loss_Billion - 1)
  ) %>%
  ungroup()

hca_2023_scenario_totals <- hca_scenario_totals %>%
  filter(year == 2023) %>%
  left_join(
    scenario_definitions %>%
      select(Scenario, Interpretation),
    by = "Scenario"
  ) %>%
  arrange(Total_Loss_Billion)

hca_2023_age_breakdown <- hca_scenario_all_years %>%
  filter(year == 2023) %>%
  select(
    Scenario, age_name, Total_YLLs, GDP_per_capita, LFPR,
    Monetary_Scale, Age_Multiplier, Combined_Multiplier,
    Main_Loss_Billion = Total_Loss_Billion,
    Scenario_Loss_Billion
  ) %>%
  mutate(
    Absolute_Change_Billion =
      Scenario_Loss_Billion - Main_Loss_Billion,
    Relative_Change_Percent =
      100 * (Scenario_Loss_Billion / Main_Loss_Billion - 1),
    age_name = factor(age_name, levels = age_specific)
  ) %>%
  arrange(Scenario, age_name) %>%
  mutate(age_name = as.character(age_name))

hca_selected_years <- hca_scenario_totals %>%
  filter(year %in% c(1990, 2000, 2010, 2020, 2023)) %>%
  arrange(year, Scenario)

# ------------------------------------------------------------------------------
# 4. Risk-attributable productivity-loss status
# ------------------------------------------------------------------------------

risk_source_status <- df_paf_all %>%
  group_by(Risk_Factor) %>%
  summarise(
    n_rows = n(),
    n_missing = sum(is.na(PAF)),
    n_zero = sum(PAF == 0, na.rm = TRUE),
    n_negative = sum(PAF < 0, na.rm = TRUE),
    min_PAF = min(PAF, na.rm = TRUE),
    max_PAF = max(PAF, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Model_Interpretation = case_when(
      n_zero == n_rows ~
        "Constant at zero; risk-attributable loss is not estimable as a varying series.",
      n_negative > 0 ~
        paste(
          "Contains negative PAF values. The primary loss plot truncates",
          "negative values at zero because a negative attributable fraction",
          "does not represent a positive productivity loss."
        ),
      TRUE ~
        "Variable non-negative PAF series."
    )
  )

risk_loss_2023 <- df_econ_master %>%
  filter(year == 2023) %>%
  group_by(Risk_Factor) %>%
  summarise(
    Risk_Attributable_Loss_Billion =
      sum(Specific_Risk_Loss_Billion, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(risk_source_status, by = "Risk_Factor") %>%
  arrange(desc(Risk_Attributable_Loss_Billion))

# ------------------------------------------------------------------------------
# 5. Quality checks
# ------------------------------------------------------------------------------

main_2023_recalculated <- hca_2023_scenario_totals %>%
  filter(Scenario == "Main HCA estimate") %>%
  pull(Total_Loss_Billion)

age_normalized_2023 <- hca_2023_scenario_totals %>%
  filter(Scenario == "Age-graded profile, total-normalized") %>%
  pull(Total_Loss_Billion)

quality_checks_step6 <- tibble(
  Check = c(
    "One HCA input row is available for each year-age combination",
    "No missing YLL values in the HCA input",
    "No missing GDP-per-capita values in the HCA input",
    "No missing female LFPR values in the HCA input",
    "All LFPR values lie between 0 and 1",
    "The recalculated 2023 main total matches the primary pipeline total",
    "The normalized age profile preserves the 2023 total",
    "All scenario multipliers are positive and finite",
    "All scenario loss estimates are non-negative and finite",
    "Risk-factor status table contains all five prespecified risks"
  ),
  Passed = c(
    nrow(hca_duplicate_check) == 0,
    sum(is.na(df_econ_base$Total_YLLs)) == 0,
    sum(is.na(df_econ_base$GDP_per_capita)) == 0,
    sum(is.na(df_econ_base$LFPR)) == 0,
    all(df_econ_base$LFPR >= 0 & df_econ_base$LFPR <= 1, na.rm = TRUE),
    isTRUE(all.equal(
      as.numeric(main_2023_recalculated),
      as.numeric(total_loss_sum),
      tolerance = 1e-10
    )),
    isTRUE(all.equal(
      as.numeric(age_normalized_2023),
      as.numeric(main_2023_recalculated),
      tolerance = 1e-8
    )),
    all(
      is.finite(scenario_age_multipliers$Combined_Multiplier) &
        scenario_age_multipliers$Combined_Multiplier > 0
    ),
    all(
      is.finite(hca_scenario_all_years$Scenario_Loss_Billion) &
        hca_scenario_all_years$Scenario_Loss_Billion >= 0
    ),
    all(
      c(
        "High alcohol use", "High BMI",
        "High fasting plasma glucose",
        "Low physical activity", "Tobacco use"
      ) %in% risk_source_status$Risk_Factor
    )
  )
) %>%
  mutate(Status = if_else(Passed, "PASS", "FAIL"))

if (any(!quality_checks_step6$Passed)) {
  failed_checks <- quality_checks_step6 %>%
    filter(!Passed) %>%
    pull(Check)

  stop(
    "Human Capital Approach sensitivity analysis HCA quality check(s) failed: ",
    paste(failed_checks, collapse = "; ")
  )
}

# ------------------------------------------------------------------------------
# 6. Export tables
# ------------------------------------------------------------------------------

readr::write_csv(
  hca_audit_by_age,
  file.path(
    analysis_tables_dir,
    "HCASensitivity_HCA_input_audit_by_age.csv"
  )
)

readr::write_csv(
  scenario_definitions,
  file.path(
    analysis_tables_dir,
    "HCASensitivity_HCA_scenario_definitions.csv"
  )
)

readr::write_csv(
  scenario_age_multipliers,
  file.path(
    analysis_tables_dir,
    "HCASensitivity_HCA_scenario_age_multipliers.csv"
  )
)

readr::write_csv(
  hca_2023_scenario_totals,
  file.path(
    analysis_tables_dir,
    "HCASensitivity_HCA_2023_scenario_totals.csv"
  )
)

readr::write_csv(
  hca_2023_age_breakdown,
  file.path(
    analysis_tables_dir,
    "HCASensitivity_HCA_2023_age_breakdown.csv"
  )
)

readr::write_csv(
  hca_selected_years,
  file.path(
    analysis_tables_dir,
    "HCASensitivity_HCA_selected_years_sensitivity.csv"
  )
)

readr::write_csv(
  risk_loss_2023,
  file.path(
    analysis_tables_dir,
    "HCASensitivity_HCA_risk_attributable_loss_status_2023.csv"
  )
)

readr::write_csv(
  quality_checks_step6,
  file.path(
    analysis_tables_dir,
    "HCASensitivity_HCA_quality_checks.csv"
  )
)

openxlsx::write.xlsx(
  list(
    Input_audit = hca_audit_by_age,
    Scenario_definitions = scenario_definitions,
    Scenario_age_multipliers = scenario_age_multipliers,
    Scenario_totals_2023 = hca_2023_scenario_totals,
    Age_breakdown_2023 = hca_2023_age_breakdown,
    Selected_years = hca_selected_years,
    Risk_loss_status_2023 = risk_loss_2023,
    Quality_checks = quality_checks_step6
  ),
  file = file.path(
    analysis_tables_dir,
    "HCASensitivity_HCA_sensitivity.xlsx"
  ),
  overwrite = TRUE
)

# ------------------------------------------------------------------------------
# 7. Sensitivity figure
# ------------------------------------------------------------------------------

plot_scenario_order <- hca_2023_scenario_totals %>%
  arrange(Total_Loss_Billion) %>%
  pull(Scenario)

plot_hca_totals <- hca_2023_scenario_totals %>%
  mutate(Scenario = factor(Scenario, levels = plot_scenario_order)) %>%
  ggplot(
    aes(x = Total_Loss_Billion, y = Scenario)
  ) +
  geom_vline(
    xintercept = main_2023_recalculated,
    linetype = "dashed",
    linewidth = 0.8
  ) +
  geom_segment(
    aes(x = 0, xend = Total_Loss_Billion, yend = Scenario),
    linewidth = 1.1
  ) +
  geom_point(size = 3.2) +
  geom_text(
    aes(label = paste0("$", round(Total_Loss_Billion, 2), "B")),
    hjust = -0.15,
    size = 3.6
  ) +
  scale_x_continuous(
    labels = scales::label_dollar(suffix = "B"),
    expand = expansion(mult = c(0, 0.18))
  ) +
  labs(
    title = "A. Total 2023 productivity-loss sensitivity",
    subtitle = "Dashed line indicates the prespecified main HCA estimate",
    x = "Estimated loss, billion constant 2015 US$",
    y = NULL
  ) +
  theme_pub(12)

age_plot_scenarios <- c(
  "Main HCA estimate",
  "Age-graded profile, total-normalized"
)

plot_hca_age <- hca_2023_age_breakdown %>%
  filter(Scenario %in% age_plot_scenarios) %>%
  mutate(
    age_name = factor(age_name, levels = age_specific),
    Scenario = factor(Scenario, levels = age_plot_scenarios)
  ) %>%
  ggplot(
    aes(
      x = age_name,
      y = Scenario_Loss_Billion,
      group = Scenario,
      linetype = Scenario,
      shape = Scenario
    )
  ) +
  geom_line(linewidth = 1.0) +
  geom_point(size = 2.8) +
  scale_y_continuous(
    labels = scales::label_dollar(suffix = "B")
  ) +
  labs(
    title = "B. Effect of age grading on the allocation of loss",
    x = "Age group",
    y = "Estimated loss, billion constant 2015 US$",
    linetype = NULL,
    shape = NULL
  ) +
  theme_pub(12) +
  theme(
    axis.text.x = element_text(angle = 20, hjust = 1),
    legend.position = "bottom"
  )

hca_sensitivity_figure <- plot_hca_totals / plot_hca_age +
  patchwork::plot_annotation(
    title = "Sensitivity of Human Capital Approach productivity-loss estimates",
    subtitle = paste(
      "Alternative multipliers are illustrative scenario assumptions rather than",
      "empirical age-specific female wage estimates."
    )
  )

ggsave(
  filename = file.path(
    analysis_figures_dir,
    "HCASensitivity_HCA_sensitivity.pdf"
  ),
  plot = hca_sensitivity_figure,
  width = 12,
  height = 10,
  device = "pdf"
)

ggsave(
  filename = file.path(
    analysis_figures_dir,
    "HCASensitivity_HCA_sensitivity.png"
  ),
  plot = hca_sensitivity_figure,
  width = 12,
  height = 10,
  dpi = 600,
  bg = "white"
)

# ------------------------------------------------------------------------------
# 8. Assumptions and completion marker
# ------------------------------------------------------------------------------

writeLines(
  c(
    "Human Capital Approach sensitivity analysis: Human Capital Approach sensitivity assumptions",
    "",
    "1. The primary estimate remains Total YLLs multiplied by World Bank GDP per capita in constant 2015 US dollars (NY.GDP.PCAP.KD) and age-specific female labour-force participation.",
    "2. The analysis estimates mortality-related productivity loss, not the complete economic burden of breast cancer.",
    "3. The uniform +/-20% scenarios quantify sensitivity to the monetary value assigned to one productive life-year.",
    "4. The age-graded profile is illustrative and is normalized to preserve the overall 2023 monetary scale; it evaluates redistribution across age groups rather than asserting an observed wage schedule.",
    "5. The combined scenarios apply both the age profile and the +/-20% monetary bounds.",
    paste0(
      "6. An optional empirical age-by-sex earnings profile was ",
      ifelse(optional_profile_used, "detected and included.", "not supplied.")
    ),
    "7. The scenarios do not account for unpaid household production, informal caregiving, treatment expenditure, morbidity-related work loss, or quality-of-life loss.",
    "8. Risk-specific losses are based on GBD PAFs and are not additive because risk exposures may overlap.",
    "9. High BMI is reported according to the source data; a constant-zero PAF series cannot be interpreted as evidence that adiposity is clinically unimportant."
  ),
  con = file.path(
    analysis_tables_dir,
    "HCASensitivity_HCA_sensitivity_assumptions.txt"
  )
)

message("✓ Human Capital Approach sensitivity analysis outputs exported.")

# ==============================================================================
# END EMBEDDED MODULE: 06_hca_sensitivity.R
# ==============================================================================


# Export reporting-completeness and internal-consistency diagnostics.
# ==============================================================================
# BEGIN EMBEDDED MODULE: 07_reporting_consistency.R
# ==============================================================================

# ==============================================================================
# Reporting completeness and internal consistency
# ==============================================================================

message("▶ Reporting completeness and internal consistency...")

required_objects_step7 <- c(
  "df_full", "df_data3", "df_mir", "df_asir_25_49", "df_asdr_25_49",
  "std_weights_25_49", "target_locations", "age_specific", "level3_risks",
  "risk_name_clean", "df_paf_measure_inventory", "hca_paf_measure_used",
  "hca_paf_measure_note", "df_paf_source_candidates", "df_paf_all",
  "analysis_tables_dir", "analysis_figures_dir", "tables_dir"
)

missing_objects_step7 <- required_objects_step7[
  !vapply(required_objects_step7, exists, logical(1), inherits = TRUE)
]

if (length(missing_objects_step7) > 0) {
  stop(
    "Reporting and consistency analysis is missing required object(s): ",
    paste(missing_objects_step7, collapse = ", ")
  )
}

# ------------------------------------------------------------------------------
# 1. Burden counts and recalculated 25-49-year rates with uncertainty bounds
# ------------------------------------------------------------------------------

measure_label_step7 <- function(x) {
  dplyr::case_when(
    stringr::str_detect(x, "Incidence") ~ "Incidence",
    stringr::str_detect(x, "Deaths|Death") ~ "Deaths",
    stringr::str_detect(x, "YLL") ~ "YLLs",
    stringr::str_detect(x, "YLD") ~ "YLDs",
    stringr::str_detect(x, "DALY") ~ "DALYs",
    TRUE ~ NA_character_
  )
}

selected_burden_years <- c(1990, 2023)

burden_number_ui <- df_full %>%
  filter(
    location_name %in% target_locations,
    age_name %in% age_specific,
    metric_name == "Number",
    year %in% selected_burden_years,
    str_detect(measure_name, "Incidence|Deaths|Death|YLL|YLD|DALY")
  ) %>%
  mutate(Measure = measure_label_step7(measure_name)) %>%
  filter(!is.na(Measure)) %>%
  group_by(location_name, year, Measure) %>%
  summarise(
    Estimate = sum(val, na.rm = TRUE),
    Lower_95_UI = sum(lower, na.rm = TRUE),
    Upper_95_UI = sum(upper, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Metric = "Number")

burden_rate_ui <- df_full %>%
  filter(
    location_name %in% target_locations,
    age_name %in% age_specific,
    metric_name == "Rate",
    year %in% selected_burden_years,
    str_detect(measure_name, "Incidence|Deaths|Death|YLL|YLD|DALY")
  ) %>%
  mutate(Measure = measure_label_step7(measure_name)) %>%
  filter(!is.na(Measure)) %>%
  left_join(std_weights_25_49, by = "age_name") %>%
  group_by(location_name, year, Measure) %>%
  summarise(
    Estimate = sum(val * std_weight, na.rm = TRUE) /
      sum(std_weight[!is.na(val)], na.rm = TRUE),
    Lower_95_UI = sum(lower * std_weight, na.rm = TRUE) /
      sum(std_weight[!is.na(lower)], na.rm = TRUE),
    Upper_95_UI = sum(upper * std_weight, na.rm = TRUE) /
      sum(std_weight[!is.na(upper)], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Metric = "Weighted rate per 100,000")

burden_selected_ui <- bind_rows(
  burden_number_ui,
  burden_rate_ui
) %>%
  mutate(
    Formatted_95_UI = paste0(
      round(Estimate, if_else(Metric == "Number", 0L, 2L)),
      " (",
      round(Lower_95_UI, if_else(Metric == "Number", 0L, 2L)),
      " to ",
      round(Upper_95_UI, if_else(Metric == "Number", 0L, 2L)),
      ")"
    ),
    Location = factor(location_name, levels = target_locations),
    Measure = factor(
      Measure,
      levels = c("Incidence", "Deaths", "YLLs", "YLDs", "DALYs")
    )
  ) %>%
  arrange(Location, year, Measure, Metric) %>%
  mutate(
    Location = as.character(Location),
    Measure = as.character(Measure)
  ) %>%
  select(
    Location,
    Year = year,
    Measure,
    Metric,
    Estimate,
    Lower_95_UI,
    Upper_95_UI,
    Formatted_95_UI
  )

# Important caveat: bounds are aggregated using the same weighted/summed
# procedure as the main point estimates. They do not reconstruct covariance
# among age-specific GBD draws.
burden_ui_note <- tibble(
  Note = paste(
    "Counts and weighted 25-49-year rates were aggregated from age-specific",
    "GBD point estimates and corresponding lower/upper bounds using the same",
    "pipeline weights. These aggregated bounds do not reconstruct covariance",
    "among the underlying GBD draws."
  )
)

# ------------------------------------------------------------------------------
# 2. MIR: crude count ratio and weighted rate-based ratio
# ------------------------------------------------------------------------------

mir_counts <- df_full %>%
  filter(
    location_name %in% target_locations,
    age_name %in% age_specific,
    metric_name == "Number",
    str_detect(measure_name, "Incidence|Deaths|Death")
  ) %>%
  mutate(
    Measure = if_else(
      str_detect(measure_name, "Incidence"),
      "Incidence",
      "Deaths"
    )
  ) %>%
  group_by(location_name, year, Measure) %>%
  summarise(Value = sum(val, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Measure, values_from = Value) %>%
  mutate(Crude_Count_Ratio = Deaths / Incidence)

mir_rate_based <- df_mir %>%
  transmute(
    location_name = as.character(location_name),
    year,
    Weighted_ASIR_25_49 = ASIR_25_49,
    Weighted_ASDR_25_49 = ASDR_25_49,
    Weighted_Rate_MIR = MIR
  )

mir_comparison <- full_join(
  mir_counts,
  mir_rate_based,
  by = c("location_name", "year")
) %>%
  mutate(
    Difference = Weighted_Rate_MIR - Crude_Count_Ratio,
    Relative_Difference_Percent =
      100 * (Weighted_Rate_MIR / Crude_Count_Ratio - 1)
  ) %>%
  arrange(
    factor(location_name, levels = target_locations),
    year
  )

mir_selected <- mir_comparison %>%
  filter(year %in% c(1990, 2000, 2010, 2020, 2023))

# ------------------------------------------------------------------------------
# 3. Signed temporal PAF values by age
# ------------------------------------------------------------------------------

risk_paf_temporal <- df_data3 %>%
  filter(
    location_name == "China",
    metric_name == "Percent",
    str_detect(measure_name, "DALY"),
    rei_name %in% level3_risks,
    age_name %in% age_specific,
    year >= 1990,
    year <= 2023
  ) %>%
  mutate(
    Risk_Factor = risk_name_clean(rei_name)
  ) %>%
  group_by(year, age_name, Risk_Factor) %>%
  summarise(
    PAF = mean(val, na.rm = TRUE),
    Lower_95_UI = mean(lower, na.rm = TRUE),
    Upper_95_UI = mean(upper, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Source_Status = case_when(
      is.na(PAF) ~ "Missing",
      PAF == 0 ~ "Observed zero",
      PAF < 0 ~ "Observed negative",
      TRUE ~ "Observed positive"
    ),
    age_name = factor(age_name, levels = age_specific)
  ) %>%
  arrange(Risk_Factor, age_name, year) %>%
  mutate(age_name = as.character(age_name))

risk_paf_selected <- risk_paf_temporal %>%
  filter(year %in% c(1990, 2000, 2010, 2020, 2023))

# Faceted supplementary line figure. Negative source values are retained.
risk_paf_plot <- risk_paf_temporal %>%
  mutate(age_name = factor(age_name, levels = age_specific)) %>%
  ggplot(
    aes(
      x = year,
      y = PAF,
      group = age_name,
      linetype = age_name
    )
  ) +
  geom_hline(yintercept = 0, linewidth = 0.4, linetype = "dotted") +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ Risk_Factor, scales = "free_y", ncol = 2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  scale_x_continuous(breaks = c(1990, 2000, 2010, 2020, 2023)) +
  labs(
    title = "Temporal changes in China-specific GBD DALY PAFs, 1990-2023",
    subtitle = "Signed source values are retained; High BMI is constant at zero",
    x = "Year",
    y = "Population attributable fraction",
    linetype = "Age group"
  ) +
  theme_pub(11) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

ggsave(
  file.path(
    analysis_figures_dir,
    "ReportingAudit_risk_PAF_temporal_by_age.pdf"
  ),
  risk_paf_plot,
  width = 12,
  height = 10,
  device = "pdf"
)

ggsave(
  file.path(
    analysis_figures_dir,
    "ReportingAudit_risk_PAF_temporal_by_age.png"
  ),
  risk_paf_plot,
  width = 12,
  height = 10,
  dpi = 600,
  bg = "white"
)

# ------------------------------------------------------------------------------
# 4. HCA PAF measure-selection audit
# ------------------------------------------------------------------------------

hca_paf_selection <- tibble(
  Selected_PAF_Measure = hca_paf_measure_used,
  Selection_Note = hca_paf_measure_note
)

hca_paf_measure_comparison <- df_paf_source_candidates %>%
  group_by(PAF_Measure, year, age_name, Risk_Factor) %>%
  summarise(PAF = mean(val, na.rm = TRUE), .groups = "drop") %>%
  filter(year == 2023) %>%
  pivot_wider(
    names_from = PAF_Measure,
    values_from = PAF,
    names_prefix = "PAF_"
  ) %>%
  mutate(
    Absolute_Difference_YLL_minus_DALY =
      if ("PAF_YLL" %in% names(.)) PAF_YLL -
        if ("PAF_DALY" %in% names(.)) PAF_DALY else NA_real_ else NA_real_
  )

# ------------------------------------------------------------------------------
# 5. Data provenance
# ------------------------------------------------------------------------------

data_provenance <- tribble(
  ~Output, ~Source_or_derivation, ~Author_derived,
  "Table 1 burden estimates",
  "GBD age-specific counts/rates; ages 25-49 aggregated or weighted by the authors",
  "Yes",
  "EAPC",
  "Log-linear regression fitted by the authors to recalculated weighted rates",
  "Yes",
  "Decomposition",
  "GBD incidence counts/rates with author-constructed demographic counterfactuals",
  "Yes",
  "Risk PAF trends",
  "GBD Comparative Risk Assessment PAF estimates; summarized by the authors",
  "Partly",
  "XGBoost-SHAP ranking",
  "Author-fitted exploratory model using GBD-derived DALY PAF features",
  "Yes",
  "MIR",
  "Author-calculated ratio; Figure 4 uses weighted ASDR divided by weighted ASIR for ages 25-49",
  "Yes",
  "HCA total productivity loss",
  "GBD YLLs × World Bank GDP per capita × ILO age-specific female LFPR",
  "Yes",
  "Risk-attributable productivity loss",
  paste0(
    "HCA total multiplied by ", hca_paf_measure_used,
    "-specific GBD PAFs"
  ),
  "Yes",
  "BAPC projection",
  "Author-fitted BAPC model using historical GBD incidence and population denominators",
  "Yes",
  "GLOBOCAN comparison",
  "GLOBOCAN 2022 counts converted to rates and compared with GBD 2022 rates",
  "Yes"
)

# ------------------------------------------------------------------------------
# 6. Quality checks
# ------------------------------------------------------------------------------

expected_burden_rows <- length(target_locations) *
  length(selected_burden_years) *
  5L *
  2L

quality_checks_step7 <- tibble(
  Check = c(
    "Burden table contains all locations, selected years, five measures and two metrics",
    "All burden point estimates and UI bounds are finite",
    "All burden lower bounds are <= point estimates",
    "All burden point estimates are <= upper bounds",
    "MIR table contains complete incidence and death values",
    "Weighted MIR values are finite and non-negative",
    "Temporal PAF table contains all 170 year-age rows for each of five risks",
    "HCA PAF selection identifies exactly one measure",
    "The selected HCA PAF measure is complete according to the inventory"
  ),
  Passed = c(
    nrow(burden_selected_ui) == expected_burden_rows,
    all(
      is.finite(burden_selected_ui$Estimate) &
        is.finite(burden_selected_ui$Lower_95_UI) &
        is.finite(burden_selected_ui$Upper_95_UI)
    ),
    all(
      burden_selected_ui$Lower_95_UI <=
        burden_selected_ui$Estimate
    ),
    all(
      burden_selected_ui$Estimate <=
        burden_selected_ui$Upper_95_UI
    ),
    all(
      !is.na(mir_comparison$Incidence) &
        !is.na(mir_comparison$Deaths) &
        !is.na(mir_comparison$Weighted_ASIR_25_49) &
        !is.na(mir_comparison$Weighted_ASDR_25_49)
    ),
    all(
      is.finite(mir_comparison$Weighted_Rate_MIR) &
        mir_comparison$Weighted_Rate_MIR >= 0
    ),
    nrow(risk_paf_temporal) ==
      length(1990:2023) * length(age_specific) * 5L,
    length(hca_paf_measure_used) == 1 &&
      hca_paf_measure_used %in% c("YLL", "DALY"),
    df_paf_measure_inventory %>%
      filter(PAF_Measure == hca_paf_measure_used) %>%
      pull(Complete_for_all_keys) %>%
      isTRUE()
  )
) %>%
  mutate(Status = if_else(Passed, "PASS", "FAIL"))

if (any(!quality_checks_step7$Passed)) {
  failed <- quality_checks_step7 %>%
    filter(!Passed) %>%
    pull(Check)

  stop(
    "Reporting and consistency analysis quality check(s) failed: ",
    paste(failed, collapse = "; ")
  )
}

# ------------------------------------------------------------------------------
# 7. Exports
# ------------------------------------------------------------------------------

readr::write_csv(
  burden_selected_ui,
  file.path(
    analysis_tables_dir,
    "ReportingAudit_burden_1990_2023_counts_rates_95UI.csv"
  )
)

readr::write_csv(
  mir_comparison,
  file.path(
    analysis_tables_dir,
    "ReportingAudit_MIR_full_series.csv"
  )
)

readr::write_csv(
  mir_selected,
  file.path(
    analysis_tables_dir,
    "ReportingAudit_MIR_selected_years.csv"
  )
)

readr::write_csv(
  risk_paf_temporal,
  file.path(
    analysis_tables_dir,
    "ReportingAudit_risk_PAF_temporal_by_age_full.csv"
  )
)

readr::write_csv(
  risk_paf_selected,
  file.path(
    analysis_tables_dir,
    "ReportingAudit_risk_PAF_temporal_by_age_selected_years.csv"
  )
)

readr::write_csv(
  df_paf_measure_inventory,
  file.path(
    analysis_tables_dir,
    "ReportingAudit_HCA_PAF_measure_inventory.csv"
  )
)

readr::write_csv(
  hca_paf_selection,
  file.path(
    analysis_tables_dir,
    "ReportingAudit_HCA_PAF_measure_selected.csv"
  )
)

readr::write_csv(
  hca_paf_measure_comparison,
  file.path(
    analysis_tables_dir,
    "ReportingAudit_HCA_PAF_YLL_DALY_comparison_2023.csv"
  )
)

readr::write_csv(
  data_provenance,
  file.path(
    analysis_tables_dir,
    "ReportingAudit_data_provenance.csv"
  )
)

readr::write_csv(
  quality_checks_step7,
  file.path(
    analysis_tables_dir,
    "ReportingAudit_quality_checks.csv"
  )
)

openxlsx::write.xlsx(
  list(
    Burden_1990_2023_95UI = burden_selected_ui,
    Burden_UI_note = burden_ui_note,
    MIR_full = mir_comparison,
    MIR_selected = mir_selected,
    Risk_PAF_selected = risk_paf_selected,
    HCA_PAF_inventory = df_paf_measure_inventory,
    HCA_PAF_selection = hca_paf_selection,
    HCA_PAF_comparison = hca_paf_measure_comparison,
    Data_provenance = data_provenance,
    Quality_checks = quality_checks_step7
  ),
  file = file.path(
    analysis_tables_dir,
    "ReportingAudit_reporting_and_consistency_audit.xlsx"
  ),
  overwrite = TRUE
)

writeLines(
  c(
    "Reporting and consistency analysis reporting notes",
    "",
    "1. Figure 4A now uses a mortality-to-incidence ratio calculated consistently for women aged 25-49 years: recalculated weighted ASDR divided by recalculated weighted ASIR.",
    "2. The crude count ratio is retained as a separate descriptive quantity and should not be conflated with the weighted rate-based MIR.",
    "3. Counts and weighted rates with 95% UI are aggregated from age-specific GBD estimates and bounds; covariance among GBD draws is not reconstructed.",
    paste0(
      "4. Risk-attributable HCA losses use ", hca_paf_measure_used,
      "-specific PAFs. ", hca_paf_measure_note
    ),
    "5. YLL and DALY PAFs are never averaged together.",
    "6. Signed PAF values are retained in descriptive risk-trend outputs.",
    "7. High BMI remains constant at zero in the China-specific 25-49-year DALY PAF series and is not interpretable as evidence of no adiposity-related risk."
  ),
  con = file.path(
    analysis_tables_dir,
    "ReportingAudit_reporting_notes.txt"
  )
)

message("✓ Reporting completeness and internal consistency outputs exported.")

# ==============================================================================
# END EMBEDDED MODULE: 07_reporting_consistency.R
# ==============================================================================


# Assess dependence of the exploratory SHAP ranking on age structure and target scale.
# ==============================================================================
# BEGIN EMBEDDED MODULE: 08_shap_structure_sensitivity.R
# ==============================================================================

# ==============================================================================
# Age-structure and target-scale sensitivity of SHAP rankings
# ==============================================================================

message("▶ Age-structure and target-scale sensitivity of SHAP rankings...")
message("  Figure S2 layout: short labels with a two-row legend")

required_objects_step8 <- c(
  "df_ml", "df_full", "df_features_x", "model_feature_cols",
  "risk_factor_labels", "age_specific", "analysis_tables_dir",
  "analysis_figures_dir", "theme_pub"
)

missing_objects_step8 <- required_objects_step8[
  !vapply(required_objects_step8, exists, logical(1), inherits = TRUE)
]

if (length(missing_objects_step8) > 0) {
  stop(
    "Age-structure and target-scale sensitivity analysis is missing required object(s): ",
    paste(missing_objects_step8, collapse = ", ")
  )
}

structure_seed <- 20260727L
structure_n_boot <- getOption("analysis.age_structure_boot", 200L)
structure_block_length <- 5L

structure_labels <- risk_factor_labels %>%
  filter(Feature %in% model_feature_cols)

# ------------------------------------------------------------------------------
# 1. Construct age-specific DALY-rate target
# ------------------------------------------------------------------------------

df_target_daly_rate_step8 <- df_full %>%
  filter(
    location_name == "China",
    metric_name == "Rate",
    str_detect(measure_name, "DALY"),
    age_name %in% age_specific,
    year >= 1990,
    year <= 2023
  ) %>%
  group_by(year, age_name) %>%
  summarise(
    DALY_rate = sum(val, na.rm = TRUE),
    .groups = "drop"
  )

structure_base <- df_ml %>%
  left_join(
    df_target_daly_rate_step8,
    by = c("year", "age_name")
  ) %>%
  arrange(year, factor(age_name, levels = age_specific))

if (
  nrow(structure_base) != 170L ||
  any(is.na(structure_base$DALY_rate)) ||
  any(!is.finite(structure_base$DALY_rate))
) {
  stop("age-structure sensitivity analysis could not construct a complete 170-row age-specific DALY-rate target.")
}

# Safe centering and residual helpers.
structure_center <- function(x) {
  x - mean(x, na.rm = TRUE)
}

structure_residualize <- function(data, variable) {
  fit <- stats::lm(
    stats::as.formula(
      paste0(variable, " ~ factor(age_name) + splines::ns(year, df = 4)")
    ),
    data = data
  )
  as.numeric(stats::residuals(fit))
}

# ------------------------------------------------------------------------------
# 2. Define five sensitivity datasets
# ------------------------------------------------------------------------------

scenario_absolute <- structure_base %>%
  transmute(
    year,
    age_name,
    across(all_of(model_feature_cols)),
    Target = Burden_number / 1000
  )

scenario_rate <- structure_base %>%
  transmute(
    year,
    age_name,
    across(all_of(model_feature_cols)),
    Target = DALY_rate
  )

scenario_centered <- structure_base %>%
  group_by(age_name) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    across(all_of(model_feature_cols), structure_center),
    Target = structure_center(DALY_rate)
  ) %>%
  ungroup() %>%
  select(year, age_name, all_of(model_feature_cols), Target)

scenario_difference <- structure_base %>%
  group_by(age_name) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    across(
      all_of(model_feature_cols),
      ~ .x - dplyr::lag(.x)
    ),
    Target = DALY_rate - dplyr::lag(DALY_rate)
  ) %>%
  ungroup() %>%
  drop_na(Target, all_of(model_feature_cols)) %>%
  select(year, age_name, all_of(model_feature_cols), Target)

scenario_residual <- structure_base %>%
  transmute(
    year,
    age_name,
    across(all_of(model_feature_cols)),
    Target_raw = DALY_rate
  )

scenario_residual$Target <- structure_residualize(
  structure_base %>% mutate(Target_raw = DALY_rate),
  "Target_raw"
)

for (feature_name in model_feature_cols) {
  scenario_residual[[feature_name]] <- structure_residualize(
    structure_base,
    feature_name
  )
}

scenario_residual <- scenario_residual %>%
  select(year, age_name, all_of(model_feature_cols), Target)

structure_scenarios <- list(
  "Absolute DALYs (primary scale)" = scenario_absolute,
  "Age-specific DALY rate" = scenario_rate,
  "Within-age centered rate and PAFs" = scenario_centered,
  "Within-age annual first differences" = scenario_difference,
  "Age- and smooth-year-adjusted residuals" = scenario_residual
)

structure_scenario_meta <- tibble::tribble(
  ~Scenario, ~Interpretation,
  "Absolute DALYs (primary scale)",
  "Primary exploratory formulation; retains population size and between-age differences.",
  "Age-specific DALY rate",
  "Removes the direct effect of age-group population size but retains between-age rate differences.",
  "Within-age centered rate and PAFs",
  "Removes each age group's long-run mean and focuses on within-age temporal deviations.",
  "Within-age annual first differences",
  "Focuses on year-to-year co-movement and removes stable age-level differences.",
  "Age- and smooth-year-adjusted residuals",
  "Tests residual associations after removing age-group levels and smooth secular trends from both target and features."
)

# ------------------------------------------------------------------------------
# 3. Shared model, SHAP and metric functions
# ------------------------------------------------------------------------------

structure_fit_model <- function(data, seed_value) {
  x <- as.matrix(as.data.frame(data)[, model_feature_cols, drop = FALSE])
  y <- as.numeric(data$Target)

  if (
    any(!is.finite(x)) ||
    any(!is.finite(y)) ||
    stats::sd(y) == 0
  ) {
    stop("Non-finite values or a constant target were detected in age-structure sensitivity analysis.")
  }

  dtrain <- xgboost::xgb.DMatrix(
    data = x,
    label = y,
    missing = NA_real_
  )

  set.seed(seed_value)

  xgboost::xgb.train(
    params = list(
      max_depth = 4,
      eta = 0.05,
      objective = "reg:squarederror",
      eval_metric = "rmse"
    ),
    data = dtrain,
    nrounds = 150,
    verbose = 0
  )
}

structure_importance <- function(model, eval_data, scenario_name, bootstrap_id = NA_integer_) {
  x_eval <- as.matrix(
    as.data.frame(eval_data)[, model_feature_cols, drop = FALSE]
  )

  contrib <- predict(model, x_eval, predcontrib = TRUE)
  actual_features <- intersect(colnames(contrib), model_feature_cols)
  contrib <- contrib[, actual_features, drop = FALSE]

  if (!setequal(colnames(contrib), model_feature_cols)) {
    stop(
      "Unexpected SHAP columns in age-structure sensitivity analysis. Expected: ",
      paste(model_feature_cols, collapse = ", "),
      "; obtained: ",
      paste(colnames(contrib), collapse = ", ")
    )
  }

  imp <- colMeans(abs(contrib), na.rm = TRUE)

  tibble(
    Scenario = scenario_name,
    Bootstrap_ID = bootstrap_id,
    Feature = names(imp),
    Mean_abs_SHAP = as.numeric(imp),
    Normalized_importance = as.numeric(imp / sum(imp)),
    Rank = rank(-imp, ties.method = "min")
  ) %>%
    left_join(structure_labels, by = "Feature")
}

structure_fit_metrics <- function(model, data, scenario_name) {
  x <- as.matrix(as.data.frame(data)[, model_feature_cols, drop = FALSE])
  observed <- as.numeric(data$Target)
  predicted <- as.numeric(predict(model, x))

  sst <- sum((observed - mean(observed))^2)

  tibble(
    Scenario = scenario_name,
    N = nrow(data),
    Target_SD = stats::sd(observed),
    RMSE = sqrt(mean((observed - predicted)^2)),
    MAE = mean(abs(observed - predicted)),
    In_sample_R2 = ifelse(
      sst > 0,
      1 - sum((observed - predicted)^2) / sst,
      NA_real_
    ),
    Spearman_rho = suppressWarnings(
      stats::cor(observed, predicted, method = "spearman")
    )
  )
}

# ------------------------------------------------------------------------------
# 4. Full-sample scenario comparison
# ------------------------------------------------------------------------------

structure_full_importance <- list()
structure_full_metrics <- list()

scenario_counter <- 0L

for (scenario_name in names(structure_scenarios)) {
  scenario_counter <- scenario_counter + 1L
  data_s <- structure_scenarios[[scenario_name]]

  model_s <- structure_fit_model(
    data_s,
    structure_seed + scenario_counter
  )

  structure_full_importance[[scenario_counter]] <- structure_importance(
    model_s,
    data_s,
    scenario_name
  )

  structure_full_metrics[[scenario_counter]] <- structure_fit_metrics(
    model_s,
    data_s,
    scenario_name
  )
}

structure_full_importance <- bind_rows(structure_full_importance)
structure_full_metrics <- bind_rows(structure_full_metrics)

# ------------------------------------------------------------------------------
# 5. Five-year grouped block bootstrap within each scenario
# ------------------------------------------------------------------------------

structure_boot_results <- list()
boot_result_index <- 0L

for (scenario_index in seq_along(structure_scenarios)) {
  scenario_name <- names(structure_scenarios)[scenario_index]
  data_s <- structure_scenarios[[scenario_index]] %>%
    mutate(
      Time_block =
        floor((year - min(year, na.rm = TRUE)) / structure_block_length) + 1L
    )

  blocks <- sort(unique(data_s$Time_block))
  set.seed(structure_seed + 10000L * scenario_index)

  for (b in seq_len(structure_n_boot)) {
    sampled_blocks <- sample(
      blocks,
      length(blocks),
      replace = TRUE
    )

    boot_rows <- unlist(
      lapply(
        sampled_blocks,
        function(block_id) which(data_s$Time_block == block_id)
      ),
      use.names = FALSE
    )

    boot_train <- data_s[boot_rows, , drop = FALSE]

    model_b <- tryCatch(
      structure_fit_model(
        boot_train,
        structure_seed + 10000L * scenario_index + b
      ),
      error = function(e) NULL
    )

    if (!is.null(model_b)) {
      boot_result_index <- boot_result_index + 1L
      structure_boot_results[[boot_result_index]] <- structure_importance(
        model_b,
        data_s,
        scenario_name,
        b
      )
    }

    if (b %% 50L == 0L || b == structure_n_boot) {
      message(
        "  age-structure sensitivity analysis bootstrap: ", scenario_name,
        " — ", b, "/", structure_n_boot
      )
    }
  }
}

structure_boot_results <- bind_rows(structure_boot_results)

if (nrow(structure_boot_results) == 0) {
  stop("All age-structure sensitivity analysis grouped block-bootstrap fits failed.")
}

structure_boot_summary <- structure_boot_results %>%
  group_by(Scenario, Feature, Risk_Factor) %>%
  summarise(
    N_successful = n(),
    Mean_normalized_importance =
      mean(Normalized_importance, na.rm = TRUE),
    SD_normalized_importance =
      sd(Normalized_importance, na.rm = TRUE),
    Median_normalized_importance =
      median(Normalized_importance, na.rm = TRUE),
    Q1_normalized_importance =
      quantile(Normalized_importance, 0.25, na.rm = TRUE),
    Q3_normalized_importance =
      quantile(Normalized_importance, 0.75, na.rm = TRUE),
    Median_rank = median(Rank, na.rm = TRUE),
    Q1_rank = quantile(Rank, 0.25, na.rm = TRUE),
    Q3_rank = quantile(Rank, 0.75, na.rm = TRUE),
    Top1_frequency = mean(Rank == 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(
    factor(Scenario, levels = names(structure_scenarios)),
    Median_rank,
    desc(Mean_normalized_importance)
  )

# ------------------------------------------------------------------------------
# 6. Correlation diagnostics: overall, between-age and within-age
# ------------------------------------------------------------------------------

structure_overall_cor <- purrr::map_dfr(
  model_feature_cols,
  function(feature_name) {
    tibble(
      Analysis = c(
        "Overall: absolute DALYs",
        "Overall: DALY rate"
      ),
      Feature = feature_name,
      Spearman_rho = c(
        suppressWarnings(
          cor(
            structure_base[[feature_name]],
            structure_base$Burden_number,
            method = "spearman"
          )
        ),
        suppressWarnings(
          cor(
            structure_base[[feature_name]],
            structure_base$DALY_rate,
            method = "spearman"
          )
        )
      )
    )
  }
)

structure_within_age_cor <- purrr::map_dfr(
  model_feature_cols,
  function(feature_name) {
    structure_base %>%
      group_by(age_name) %>%
      summarise(
        Feature = feature_name,
        Spearman_rho = suppressWarnings(
          cor(
            .data[[feature_name]],
            DALY_rate,
            method = "spearman"
          )
        ),
        .groups = "drop"
      ) %>%
      mutate(Analysis = "Within age group: DALY rate")
  }
)

structure_between_age <- structure_base %>%
  group_by(age_name) %>%
  summarise(
    across(
      all_of(model_feature_cols),
      ~ mean(.x, na.rm = TRUE)
    ),
    Mean_DALY_rate = mean(DALY_rate, na.rm = TRUE),
    Mean_absolute_DALYs = mean(Burden_number, na.rm = TRUE),
    .groups = "drop"
  )

structure_between_age_cor <- purrr::map_dfr(
  model_feature_cols,
  function(feature_name) {
    tibble(
      Analysis = c(
        "Between age-group means: DALY rate",
        "Between age-group means: absolute DALYs"
      ),
      Feature = feature_name,
      Spearman_rho = c(
        suppressWarnings(
          cor(
            structure_between_age[[feature_name]],
            structure_between_age$Mean_DALY_rate,
            method = "spearman"
          )
        ),
        suppressWarnings(
          cor(
            structure_between_age[[feature_name]],
            structure_between_age$Mean_absolute_DALYs,
            method = "spearman"
          )
        )
      )
    )
  }
)

structure_correlations <- bind_rows(
  structure_overall_cor,
  structure_within_age_cor,
  structure_between_age_cor
) %>%
  left_join(structure_labels, by = "Feature")

# ------------------------------------------------------------------------------
# 7. Decision summary
# ------------------------------------------------------------------------------

structure_fpg_summary <- structure_boot_summary %>%
  filter(Feature == "High_fasting_plasma_glucose") %>%
  select(
    Scenario,
    FPG_Mean_importance = Mean_normalized_importance,
    FPG_Median_rank = Median_rank,
    FPG_Top1_frequency = Top1_frequency
  )

structure_decision_summary <- structure_full_importance %>%
  group_by(Scenario) %>%
  arrange(Rank, .by_group = TRUE) %>%
  summarise(
    Full_sample_top_feature = first(Risk_Factor),
    Full_sample_top_importance = first(Normalized_importance),
    .groups = "drop"
  ) %>%
  left_join(structure_fpg_summary, by = "Scenario") %>%
  left_join(structure_scenario_meta, by = "Scenario") %>%
  mutate(
    Suggested_interpretation = case_when(
      FPG_Median_rank == 1 &
        FPG_Top1_frequency >= 0.80 ~
        paste(
          "FPG-related PAF remains the leading exploratory feature",
          "under this formulation."
        ),
      FPG_Median_rank <= 2 &
        FPG_Top1_frequency >= 0.50 ~
        paste(
          "FPG-related PAF remains prominent but is not uniformly",
          "the leading feature."
        ),
      TRUE ~
        paste(
          "The FPG ranking is sensitive to this formulation;",
          "the original ranking should be interpreted cautiously."
        )
    )
  )

# ------------------------------------------------------------------------------
# 8. Quality checks
# ------------------------------------------------------------------------------

quality_checks_step8 <- tibble(
  Check = c(
    "All five planned sensitivity scenarios were constructed",
    "All scenario targets and features are finite",
    "Each scenario contains at least 150 observations",
    "Full-sample SHAP output contains four non-constant features per scenario",
    "Bootstrap summary contains four non-constant features per scenario",
    "All normalized full-sample importance values sum to one within scenario",
    "All normalized bootstrap mean importance values sum approximately to one within scenario"
  ),
  Passed = c(
    length(structure_scenarios) == 5L,
    all(
      vapply(
        structure_scenarios,
        function(x) {
          all(
            is.finite(
              as.matrix(
                x[, c(model_feature_cols, "Target"), drop = FALSE]
              )
            )
          )
        },
        logical(1)
      )
    ),
    all(vapply(structure_scenarios, nrow, integer(1)) >= 150L),
    nrow(structure_full_importance) ==
      5L * length(model_feature_cols),
    nrow(structure_boot_summary) ==
      5L * length(model_feature_cols),
    structure_full_importance %>%
      group_by(Scenario) %>%
      summarise(x = abs(sum(Normalized_importance) - 1) < 1e-8) %>%
      pull(x) %>%
      all(),
    structure_boot_summary %>%
      group_by(Scenario) %>%
      summarise(
        x = abs(sum(Mean_normalized_importance) - 1) < 0.02
      ) %>%
      pull(x) %>%
      all()
  )
) %>%
  mutate(Status = if_else(Passed, "PASS", "FAIL"))

if (any(!quality_checks_step8$Passed)) {
  failed_checks <- quality_checks_step8 %>%
    filter(!Passed) %>%
    pull(Check)

  stop(
    "Age-structure and target-scale sensitivity analysis quality check(s) failed: ",
    paste(failed_checks, collapse = "; ")
  )
}

# ------------------------------------------------------------------------------
# 9. Figures
# ------------------------------------------------------------------------------

scenario_levels_step8 <- names(structure_scenarios)

plot_structure_full <- structure_full_importance %>%
  mutate(
    Scenario = factor(Scenario, levels = scenario_levels_step8),
    Risk_Factor = factor(
      Risk_Factor,
      levels = rev(unique(structure_labels$Risk_Factor))
    )
  ) %>%
  ggplot(
    aes(
      x = Normalized_importance,
      y = Risk_Factor,
      shape = Scenario
    )
  ) +
  geom_point(
    position = position_dodge(width = 0.65),
    size = 2.8
  ) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_shape_manual(
    values = c(16, 17, 15, 3, 4),
    breaks = scenario_levels_step8,
    labels = c(
      "Absolute DALYs",
      "Age-specific DALY rate",
      "Within-age centered",
      "Annual first differences",
      "Age/year-adjusted residuals"
    ),
    name = "Formulation"
  ) +
  labs(
    title = "A. Full-sample SHAP importance across target formulations",
    x = "Normalized mean absolute SHAP importance",
    y = NULL
  ) +
  theme_pub(11) +
  guides(
    shape = guide_legend(
      nrow = 2,
      byrow = TRUE
    )
  ) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.direction = "horizontal",
    legend.title = element_text(
      face = "bold",
      size = 10
    ),
    legend.text = element_text(
      size = 8.5
    ),
    legend.key.width = grid::unit(1.0, "lines"),
    legend.spacing.x = grid::unit(0.15, "cm"),
    legend.margin = margin(
      t = 2,
      r = 2,
      b = 2,
      l = 2
    )
  )

plot_structure_boot <- structure_boot_summary %>%
  mutate(
    Scenario = factor(Scenario, levels = scenario_levels_step8),
    Risk_Factor = factor(
      Risk_Factor,
      levels = rev(unique(structure_labels$Risk_Factor))
    )
  ) %>%
  ggplot(
    aes(
      x = FPG_Top1_frequency,
      y = Scenario
    )
  )

# Use an explicit FPG-only data frame for panel B.
plot_structure_fpg <- structure_fpg_summary %>%
  mutate(
    Scenario = factor(Scenario, levels = scenario_levels_step8)
  ) %>%
  ggplot(
    aes(
      x = FPG_Top1_frequency,
      y = Scenario
    )
  ) +
  geom_segment(
    aes(x = 0, xend = FPG_Top1_frequency, yend = Scenario),
    linewidth = 0.9
  ) +
  geom_point(size = 3) +
  scale_x_continuous(
    limits = c(0, 1),
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    title = "B. Frequency with which FPG-related PAF ranked first",
    subtitle = paste0(
      "Grouped calendar-block bootstrap; ",
      structure_n_boot, " resamples per formulation"
    ),
    x = "Top-rank frequency",
    y = NULL
  ) +
  theme_pub(11)

structure_figure <- (
  plot_structure_full / plot_structure_fpg
) +
  patchwork::plot_layout(
    heights = c(1.08, 1)
  ) +
  patchwork::plot_annotation(
    title = "Sensitivity of exploratory SHAP ranking to age structure and target scale",
    subtitle = paste(
      "High BMI is excluded because its China-specific PAF series",
      "is constant at zero."
    )
  )

ggsave(
  file.path(
    analysis_figures_dir,
    "SHAPStructure_age_structure_target_sensitivity.pdf"
  ),
  structure_figure,
  width = 13,
  height = 10,
  device = "pdf"
)

ggsave(
  file.path(
    analysis_figures_dir,
    "SHAPStructure_age_structure_target_sensitivity.png"
  ),
  structure_figure,
  width = 13,
  height = 10,
  dpi = 600,
  bg = "white"
)

# ------------------------------------------------------------------------------
# 10. Exports
# ------------------------------------------------------------------------------

readr::write_csv(
  structure_full_importance,
  file.path(
    analysis_tables_dir,
    "SHAPStructure_fullsample_SHAP_by_formulation.csv"
  )
)

readr::write_csv(
  structure_full_metrics,
  file.path(
    analysis_tables_dir,
    "SHAPStructure_fullsample_model_metrics.csv"
  )
)

readr::write_csv(
  structure_boot_summary,
  file.path(
    analysis_tables_dir,
    "SHAPStructure_block_bootstrap_SHAP_by_formulation.csv"
  )
)

readr::write_csv(
  structure_correlations,
  file.path(
    analysis_tables_dir,
    "SHAPStructure_feature_outcome_correlation_diagnostics.csv"
  )
)

readr::write_csv(
  structure_between_age,
  file.path(
    analysis_tables_dir,
    "SHAPStructure_between_age_group_means.csv"
  )
)

readr::write_csv(
  structure_decision_summary,
  file.path(
    analysis_tables_dir,
    "SHAPStructure_interpretation_decision_summary.csv"
  )
)

readr::write_csv(
  quality_checks_step8,
  file.path(
    analysis_tables_dir,
    "SHAPStructure_quality_checks.csv"
  )
)

openxlsx::write.xlsx(
  list(
    Scenario_definitions = structure_scenario_meta,
    Fullsample_SHAP = structure_full_importance,
    Fullsample_metrics = structure_full_metrics,
    Bootstrap_SHAP = structure_boot_summary,
    Correlations = structure_correlations,
    Between_age_means = structure_between_age,
    Decision_summary = structure_decision_summary,
    Quality_checks = quality_checks_step8
  ),
  file = file.path(
    analysis_tables_dir,
    "SHAPStructure_age_structure_target_sensitivity.xlsx"
  ),
  overwrite = TRUE
)

writeLines(
  c(
    "Age-structure and target-scale sensitivity analysis interpretation framework",
    "",
    "1. The primary model predicts absolute DALYs and therefore retains both population size and between-age differences.",
    "2. The age-specific DALY-rate scenario removes the direct population-size component.",
    "3. The within-age centered scenario removes long-run between-age levels and focuses on temporal deviations within each age group.",
    "4. The first-difference scenario focuses on year-to-year changes.",
    "5. The residualized scenario removes age-group levels and smooth secular trends from the target and every feature.",
    "6. Persistence of a high FPG rank across the last three scenarios would support robustness beyond the simple age gradient.",
    "7. Loss of the FPG ranking in those scenarios would indicate that its original importance was substantially driven by age or secular structure.",
    "8. No scenario provides causal evidence, and all findings remain model-based and hypothesis-generating."
  ),
  con = file.path(
    analysis_tables_dir,
    "SHAPStructure_interpretation_framework.txt"
  )
)

message("✓ Age-structure and target-scale sensitivity of SHAP rankings outputs exported.")

# ==============================================================================
# END EMBEDDED MODULE: 08_shap_structure_sensitivity.R
# ==============================================================================


# Create consistently named main-manuscript and supplementary outputs.
# ==============================================================================
# BEGIN EMBEDDED MODULE: 09_manuscript_outputs.R
# ==============================================================================

# ==============================================================================
# Manuscript-ready tables, figures, source data, and output index
#
# This module does not refit statistical models. It converts the finalized
# analysis objects into consistently named files for the main manuscript and
# Supplementary Information.
# ==============================================================================

message("▶ Building manuscript-ready tables and figures...")

required_manuscript_objects <- c(
  "output_path", "target_locations", "age_specific",
  "color_china", "color_global",
  "fig1_final", "fig2_final", "fig3_final", "fig4_final", "fig_s1",
  "df_final_fig5", "burden_selected_ui", "df_eapc_final",
  "df_table2", "df_paf_2023", "df_hca_summary", "df_bapc_selected",
  "df_shap_importance", "feature_summary", "join_summary",
  "target_summary", "risk_long_summary", "validation_checks",
  "bmi_selected_summary", "bmi_selected_by_age",
  "bmi_by_location_summary", "temporal_fold_perf",
  "temporal_fold_imp", "temporal_fold_concordance",
  "temporal_boot_summary", "temporal_boot_concordance_summary",
  "structure_full_importance", "structure_full_metrics",
  "structure_boot_summary", "structure_correlations",
  "structure_decision_summary", "anchor_diagnostics",
  "anchored_vs_unanchored", "projection_selected_95ui",
  "quality_checks", "globocan_baseline_scaling_factors",
  "main_vs_globocan_baseline_scaled", "quality_checks_step5",
  "hca_2023_scenario_totals", "hca_2023_age_breakdown",
  "risk_loss_2023", "quality_checks_step6",
  "burden_selected_ui", "mir_selected", "risk_paf_selected",
  "data_provenance", "quality_checks_step7",
  "quality_checks_step8", "temporal_plot", "structure_figure",
  "p_projection_compare", "p_step5", "hca_sensitivity_figure",
  "risk_paf_plot", "df_world_raw", "df_trend_weighted",
  "df_comp", "all_decomp_results", "df_cohort",
  "df_macro", "df_micro", "df_mir", "df_total_2023",
  "df_risk_trend", "df_validation_table", "df_scatter"
)

missing_manuscript_objects <- required_manuscript_objects[
  !vapply(
    required_manuscript_objects,
    exists,
    logical(1),
    inherits = TRUE
  )
]

if (length(missing_manuscript_objects) > 0) {
  stop(
    "The manuscript-output module is missing required object(s): ",
    paste(missing_manuscript_objects, collapse = ", ")
  )
}

# ------------------------------------------------------------------------------
# 1. Output directories
# ------------------------------------------------------------------------------

manuscript_ready_dir <- file.path(output_path, "Manuscript_Ready")
main_figures_dir <- file.path(manuscript_ready_dir, "Main_Figures")
main_tables_dir <- file.path(manuscript_ready_dir, "Main_Tables")
supp_figures_dir <- file.path(manuscript_ready_dir, "Supplementary_Figures")
supp_tables_dir <- file.path(manuscript_ready_dir, "Supplementary_Tables")
source_data_dir <- file.path(manuscript_ready_dir, "Figure_Source_Data")
documentation_dir <- file.path(manuscript_ready_dir, "Documentation")

for (directory_path in c(
  manuscript_ready_dir,
  main_figures_dir,
  main_tables_dir,
  supp_figures_dir,
  supp_tables_dir,
  source_data_dir,
  documentation_dir
)) {
  dir.create(directory_path, recursive = TRUE, showWarnings = FALSE)
}

save_manuscript_plot <- function(
  plot_object,
  directory,
  file_stub,
  width,
  height,
  dpi = 600
) {
  ggplot2::ggsave(
    filename = file.path(directory, paste0(file_stub, ".pdf")),
    plot = plot_object,
    width = width,
    height = height,
    device = "pdf",
    limitsize = FALSE
  )

  ggplot2::ggsave(
    filename = file.path(directory, paste0(file_stub, ".png")),
    plot = plot_object,
    width = width,
    height = height,
    dpi = dpi,
    bg = "white",
    limitsize = FALSE
  )
}

write_manuscript_csv <- function(data, directory, file_stub) {
  readr::write_csv(
    data,
    file.path(directory, paste0(file_stub, ".csv"))
  )
}

# ------------------------------------------------------------------------------
# 2. Main Table 1: burden, rates, uncertainty bounds, and EAPC
# ------------------------------------------------------------------------------

format_count_ui <- function(estimate, lower, upper) {
  paste0(
    scales::comma(round(estimate)),
    " (",
    scales::comma(round(lower)),
    " to ",
    scales::comma(round(upper)),
    ")"
  )
}

format_rate_ui <- function(estimate, lower, upper) {
  sprintf("%.2f (%.2f to %.2f)", estimate, lower, upper)
}

table1_long <- burden_selected_ui %>%
  dplyr::filter(
    Measure %in% c("Incidence", "Deaths"),
    Year %in% c(1990, 2023)
  ) %>%
  dplyr::mutate(
    Display_value = dplyr::if_else(
      Metric == "Number",
      format_count_ui(Estimate, Lower_95_UI, Upper_95_UI),
      format_rate_ui(Estimate, Lower_95_UI, Upper_95_UI)
    ),
    Column_name = dplyr::case_when(
      Measure == "Incidence" & Metric == "Number" & Year == 1990 ~
        "Incidence cases, 1990, n (95% UI)",
      Measure == "Incidence" & Metric == "Weighted rate per 100,000" &
        Year == 1990 ~
        "ASIR, 1990, per 100,000 (95% UI)",
      Measure == "Incidence" & Metric == "Number" & Year == 2023 ~
        "Incidence cases, 2023, n (95% UI)",
      Measure == "Incidence" & Metric == "Weighted rate per 100,000" &
        Year == 2023 ~
        "ASIR, 2023, per 100,000 (95% UI)",
      Measure == "Deaths" & Metric == "Number" & Year == 1990 ~
        "Deaths, 1990, n (95% UI)",
      Measure == "Deaths" & Metric == "Weighted rate per 100,000" &
        Year == 1990 ~
        "ASDR, 1990, per 100,000 (95% UI)",
      Measure == "Deaths" & Metric == "Number" & Year == 2023 ~
        "Deaths, 2023, n (95% UI)",
      Measure == "Deaths" & Metric == "Weighted rate per 100,000" &
        Year == 2023 ~
        "ASDR, 2023, per 100,000 (95% UI)",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::filter(!is.na(Column_name)) %>%
  dplyr::select(Location, Column_name, Display_value)

table1_eapc <- df_eapc_final %>%
  dplyr::filter(Measure %in% c("ASIR", "ASDR")) %>%
  dplyr::select(
    Location = location_name,
    Measure,
    `EAPC (95% CI)`
  ) %>%
  tidyr::pivot_wider(
    names_from = Measure,
    values_from = `EAPC (95% CI)`,
    names_glue = "{Measure} EAPC, % (95% CI)"
  )

table1_manuscript <- table1_long %>%
  tidyr::pivot_wider(
    names_from = Column_name,
    values_from = Display_value
  ) %>%
  dplyr::left_join(table1_eapc, by = "Location") %>%
  dplyr::mutate(
    Location = factor(Location, levels = target_locations)
  ) %>%
  dplyr::arrange(Location) %>%
  dplyr::mutate(Location = as.character(Location)) %>%
  dplyr::select(
    Location,
    `Incidence cases, 1990, n (95% UI)`,
    `ASIR, 1990, per 100,000 (95% UI)`,
    `Incidence cases, 2023, n (95% UI)`,
    `ASIR, 2023, per 100,000 (95% UI)`,
    `ASIR EAPC, % (95% CI)`,
    `Deaths, 1990, n (95% UI)`,
    `ASDR, 1990, per 100,000 (95% UI)`,
    `Deaths, 2023, n (95% UI)`,
    `ASDR, 2023, per 100,000 (95% UI)`,
    `ASDR EAPC, % (95% CI)`
  )

write_manuscript_csv(
  table1_manuscript,
  main_tables_dir,
  "Table_1_Breast_cancer_burden_and_temporal_trends"
)

table1_notes <- c(
  "Table 1 notes",
  "",
  "Counts were summed from age-specific GBD estimates for women aged 25-49 years.",
  "ASIR and ASDR were recalculated by the authors using the study-specific 25-49-year age range and GBD standard-population weights.",
  "EAPCs and 95% confidence intervals were estimated by log-linear regression of the recalculated weighted rates.",
  "The displayed uncertainty bounds were aggregated from age-specific GBD lower and upper estimates using the same summation or weighting procedure; covariance among GBD draws was not reconstructed.",
  "Abbreviations: ASIR, age-standardized incidence rate; ASDR, age-standardized death rate; EAPC, estimated annual percentage change; UI, uncertainty interval."
)

writeLines(
  table1_notes,
  file.path(documentation_dir, "Table_1_notes.txt")
)

# ------------------------------------------------------------------------------
# 3. Main figures
# ------------------------------------------------------------------------------

save_manuscript_plot(
  fig1_final,
  main_figures_dir,
  "Figure_1_Breast_cancer_burden_and_temporal_trends",
  16,
  18
)

save_manuscript_plot(
  fig2_final,
  main_figures_dir,
  "Figure_2_Decomposition_and_birth_cohort_patterns",
  14,
  12
)

save_manuscript_plot(
  fig3_final,
  main_figures_dir,
  "Figure_3_GBD_risk_attribution_and_exploratory_SHAP_ranking",
  18,
  15
)

save_manuscript_plot(
  fig4_final,
  main_figures_dir,
  "Figure_4_MIR_and_mortality_related_productivity_loss",
  16,
  12
)

# Simplified manuscript projection figure: retain the 95% and 80% intervals.
plot_projection_manuscript <- function(
  data,
  title_text,
  center_title = FALSE
) {
  ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = year,
      color = location_name,
      fill = location_name
    )
  ) +
    ggplot2::geom_ribbon(
      data = dplyr::filter(data, year >= 2023),
      ggplot2::aes(ymin = Lwr_95, ymax = Upr_95),
      alpha = 0.08,
      color = NA
    ) +
    ggplot2::geom_ribbon(
      data = dplyr::filter(data, year >= 2023),
      ggplot2::aes(ymin = Lwr_80, ymax = Upr_80),
      alpha = 0.18,
      color = NA
    ) +
    ggplot2::geom_point(
      data = dplyr::filter(data, year <= 2023),
      ggplot2::aes(y = Observed_Rate),
      size = 1.0,
      alpha = 0.90
    ) +
    ggplot2::geom_line(
      data = dplyr::filter(data, year <= 2023),
      ggplot2::aes(y = Pred_Rate),
      linewidth = 1.1
    ) +
    ggplot2::geom_line(
      data = dplyr::filter(data, year >= 2023),
      ggplot2::aes(y = Pred_Rate),
      linetype = "dashed",
      linewidth = 1.1
    ) +
    ggplot2::geom_vline(
      xintercept = 2023,
      linetype = "dotted",
      color = "grey35",
      linewidth = 0.9
    ) +
    ggplot2::scale_x_continuous(
      breaks = c(1990, 2010, 2023, 2035, 2050),
      limits = c(1989, 2051)
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "China" = color_china,
        "Global" = color_global
      ),
      name = "Location"
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        "China" = color_china,
        "Global" = color_global
      ),
      name = "Location"
    ) +
    ggplot2::labs(
      title = title_text,
      x = NULL,
      y = "Incidence rate per 100,000"
    ) +
    ggplot2::theme_classic(base_size = 14) +
    ggplot2::theme(
      strip.background = ggplot2::element_rect(
        fill = "#F2F2F2",
        color = "black",
        linewidth = 0.8
      ),
      strip.text = ggplot2::element_text(
        face = "bold",
        size = 11,
        color = "black"
      ),
      panel.border = ggplot2::element_rect(
        color = "black",
        fill = NA,
        linewidth = 0.8
      ),
      axis.line = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(color = "black", size = 10.5),
      axis.title = ggplot2::element_text(color = "black", size = 13),
      plot.title = ggplot2::element_text(
        face = "bold",
        size = 15,
        hjust = ifelse(center_title, 0.5, 0)
      ),
      legend.position = "top",
      legend.title = ggplot2::element_text(face = "bold"),
      legend.text = ggplot2::element_text(size = 10.5)
    )
}

p_projection_age_manuscript <- plot_projection_manuscript(
  df_final_fig5 %>%
    dplyr::filter(age_name != "Overall ASIR (25-49y)") %>%
    dplyr::mutate(
      age_name = factor(age_name, levels = age_specific)
    ),
  "A. Age-specific incidence-rate projections to 2050"
) +
  ggplot2::facet_wrap(
    ~ age_name,
    ncol = 3,
    scales = "free_y"
  )

p_projection_overall_manuscript <- plot_projection_manuscript(
  df_final_fig5 %>%
    dplyr::filter(age_name == "Overall ASIR (25-49y)"),
  "B. Overall ASIR projection, ages 25-49",
  center_title = TRUE
)

figure5_manuscript <- p_projection_age_manuscript /
  p_projection_overall_manuscript +
  patchwork::plot_layout(
    heights = c(2, 1.2),
    guides = "collect"
  ) &
  ggplot2::theme(
    legend.position = "top",
    legend.box = "horizontal"
  )

save_manuscript_plot(
  figure5_manuscript,
  main_figures_dir,
  "Figure_5_Status_quo_incidence_projections",
  15,
  12
)

save_manuscript_plot(
  fig_s1,
  main_figures_dir,
  "Figure_6_GBD_and_GLOBOCAN_external_comparison",
  16,
  8
)

# ------------------------------------------------------------------------------
# 4. Supplementary figures
# ------------------------------------------------------------------------------

save_manuscript_plot(
  temporal_plot,
  supp_figures_dir,
  "Supplementary_Figure_S1_SHAP_temporal_stability",
  13,
  10
)

save_manuscript_plot(
  structure_figure,
  supp_figures_dir,
  "Supplementary_Figure_S2_SHAP_structure_and_target_sensitivity",
  13,
  10
)

save_manuscript_plot(
  risk_paf_plot,
  supp_figures_dir,
  "Supplementary_Figure_S3_Age_specific_PAF_trends",
  12,
  10
)

save_manuscript_plot(
  p_projection_compare,
  supp_figures_dir,
  "Supplementary_Figure_S4_Projection_anchor_sensitivity",
  12,
  10
)

save_manuscript_plot(
  p_step5,
  supp_figures_dir,
  "Supplementary_Figure_S5_GLOBOCAN_baseline_sensitivity",
  12,
  11
)

save_manuscript_plot(
  hca_sensitivity_figure,
  supp_figures_dir,
  "Supplementary_Figure_S6_HCA_productivity_loss_sensitivity",
  12,
  10
)

# ------------------------------------------------------------------------------
# 5. Supplementary tables
# ------------------------------------------------------------------------------

write_manuscript_csv(
  df_validation_table,
  supp_tables_dir,
  "Supplementary_Table_S1_GBD_vs_GLOBOCAN_2022"
)

write_manuscript_csv(
  df_table2,
  supp_tables_dir,
  "Supplementary_Table_S2_Decomposition_results"
)

write_manuscript_csv(
  df_paf_2023,
  supp_tables_dir,
  "Supplementary_Table_S3_PAF_and_SHAP_ranking_2023"
)

write_manuscript_csv(
  df_hca_summary %>%
    dplyr::filter(year %in% c(1990, 2000, 2010, 2020, 2023)),
  supp_tables_dir,
  "Supplementary_Table_S4_Productivity_losses_selected_years"
)

write_manuscript_csv(
  df_bapc_selected,
  supp_tables_dir,
  "Supplementary_Table_S5_BAPC_selected_years_95UI"
)

write_manuscript_csv(
  df_shap_importance,
  supp_tables_dir,
  "Supplementary_Table_S6_SHAP_feature_status_and_importance"
)


write_manuscript_csv(
  feature_summary,
  supp_tables_dir,
  "Supplementary_Table_S7_PAF_feature_completeness"
)

write_manuscript_csv(
  bmi_selected_by_age,
  supp_tables_dir,
  "Supplementary_Table_S8_High_BMI_source_validation_by_age"
)

write_manuscript_csv(
  temporal_boot_summary,
  supp_tables_dir,
  "Supplementary_Table_S9_SHAP_temporal_bootstrap_summary"
)

write_manuscript_csv(
  structure_decision_summary,
  supp_tables_dir,
  "Supplementary_Table_S10_SHAP_structure_sensitivity_summary"
)

write_manuscript_csv(
  anchored_vs_unanchored,
  supp_tables_dir,
  "Supplementary_Table_S11_Anchored_vs_unanchored_projection"
)

write_manuscript_csv(
  manuscript_ready_baseline_scaled,
  supp_tables_dir,
  "Supplementary_Table_S12_GLOBOCAN_baseline_scaled_projection"
)

write_manuscript_csv(
  hca_2023_scenario_totals,
  supp_tables_dir,
  "Supplementary_Table_S13_HCA_sensitivity_scenarios_2023"
)

write_manuscript_csv(
  burden_selected_ui,
  supp_tables_dir,
  "Supplementary_Table_S14_Burden_counts_rates_and_95UI"
)

write_manuscript_csv(
  risk_paf_selected,
  supp_tables_dir,
  "Supplementary_Table_S15_Age_specific_PAF_selected_years"
)

write_manuscript_csv(
  mir_selected,
  supp_tables_dir,
  "Supplementary_Table_S16_MIR_selected_years"
)

write_manuscript_csv(
  data_provenance,
  supp_tables_dir,
  "Supplementary_Table_S17_Data_provenance"
)

# Consolidated workbook for editorial review and manuscript drafting.
supplementary_workbook <- list(
  S1_GBD_GLOBOCAN = df_validation_table,
  S2_Decomposition = df_table2,
  S3_PAF_SHAP = df_paf_2023,
  S4_HCA_selected = df_hca_summary %>%
    dplyr::filter(year %in% c(1990, 2000, 2010, 2020, 2023)),
  S5_BAPC_selected = df_bapc_selected,
  S6_SHAP_status = df_shap_importance,
  S7_Completeness = feature_summary,
  S8_BMI_by_age = bmi_selected_by_age,
  S9_Temporal_SHAP = temporal_boot_summary,
  S10_Structure_SHAP = structure_decision_summary,
  S11_Anchor = anchored_vs_unanchored,
  S12_GLOBOCAN_sens = manuscript_ready_baseline_scaled,
  S13_HCA_sens = hca_2023_scenario_totals,
  S14_Burden_UI = burden_selected_ui,
  S15_PAF_trends = risk_paf_selected,
  S16_MIR = mir_selected,
  S17_Provenance = data_provenance
)

openxlsx::write.xlsx(
  supplementary_workbook,
  file = file.path(
    supp_tables_dir,
    "Supplementary_Tables_S1_to_S17.xlsx"
  ),
  overwrite = TRUE
)

# ------------------------------------------------------------------------------
# 6. Figure source data
# ------------------------------------------------------------------------------

figure_source_data <- list(
  Figure_1_maps = df_world_raw,
  Figure_1_trends = df_trend_weighted,
  Figure_1_composition = df_comp,
  Figure_2_decomposition = all_decomp_results,
  Figure_2_cohort = df_cohort,
  Figure_3_broad_PAF = df_macro,
  Figure_3_specific_PAF = df_micro,
  Figure_3_SHAP = df_shap_importance,
  Figure_4_MIR = df_mir,
  Figure_4_HCA_age = df_total_2023,
  Figure_4_HCA_risk = df_risk_trend,
  Figure_5_projection = df_final_fig5,
  Figure_6_comparison = df_validation_table,
  Figure_6_scatter = df_scatter
)

for (source_name in names(figure_source_data)) {
  write_manuscript_csv(
    figure_source_data[[source_name]],
    source_data_dir,
    source_name
  )
}

# ------------------------------------------------------------------------------
# 7. Captions and output index
# ------------------------------------------------------------------------------

figure_captions <- c(
  "Figure 1. Breast cancer burden and temporal trends among women aged 25-49 years. Maps show age-standardized burden estimates, while the temporal panels summarize author-recalculated weighted rates and the YLL/YLD composition. See Methods for the restricted-age standardization procedure.",
  "Figure 2. Decomposition of the change in incident breast cancer cases and descriptive birth-cohort patterns. The epidemiological component is a residual after accounting for population growth and age structure and should not be interpreted as a directly measured risk exposure.",
  "Figure 3. GBD-derived risk-attribution patterns and exploratory XGBoost-SHAP feature ranking. High BMI was not rankable because the selected China-specific breast-cancer DALY PAF series was constant at zero; this does not indicate absence of obesity exposure or biological relevance. SHAP values describe model-based feature importance and do not establish causality.",
  "Figure 4. Mortality-to-incidence ratio and mortality-related productivity loss. Panel A uses the weighted ASDR-to-ASIR ratio for ages 25-49. Productivity losses are estimated with the Human Capital Approach and do not represent the complete economic burden of breast cancer. Risk-attributable losses use YLL-specific PAFs.",
  "Figure 5. Status-quo breast cancer incidence projections to 2050. Historical observations and fitted values are shown through 2023; dashed lines indicate projections. Shaded bands represent 80% and 95% posterior uncertainty intervals after age- and location-specific multiplicative anchoring to the observed 2023 rates.",
  "Figure 6. External registry-based comparison of GBD 2022 and GLOBOCAN 2022 incidence estimates. The comparison assesses concordance between data sources and is not an external validation of the BAPC model."
)

writeLines(
  figure_captions,
  file.path(documentation_dir, "Main_Figure_Captions_Draft.txt")
)

supplementary_captions <- c(
  "Supplementary Figure S1. Temporal stability of the exploratory SHAP ranking. Fold-specific SHAP values were calculated exclusively in held-out future-period observations. The grouped bootstrap sampled six non-overlapping 5-year calendar blocks and one final 4-year block with replacement, retaining all five age groups within each selected year. High BMI was constant at zero in the selected GBD series and was therefore not rankable; this does not indicate absent obesity exposure or biological irrelevance.",
  "Supplementary Figure S2. Sensitivity of the exploratory SHAP ranking to target scale, population size, between-age differences, annual changes, and residual age/time structure.",
  "Supplementary Figure S3. Age-specific temporal patterns in China-specific GBD DALY PAFs from 1990 to 2023. Signed source estimates are retained; the High BMI series is constant at zero.",
  "Supplementary Figure S4. Sensitivity of BAPC projections to multiplicative anchoring at the observed 2023 incidence rate.",
  "Supplementary Figure S5. Sensitivity analysis using rates derived from GLOBOCAN 2022 case estimates as an external incidence baseline. This scenario preserves the GBD-derived trajectory and is not an independent GLOBOCAN forecast. The scaled intervals retain the relative posterior uncertainty of the main model but do not incorporate uncertainty in the GLOBOCAN-to-GBD baseline-scaling factors.",
  "Supplementary Figure S6. Sensitivity of Human Capital Approach productivity-loss estimates to illustrative productivity-value and age-allocation assumptions; the age multipliers are not empirical age-specific female wage estimates."
)

writeLines(
  supplementary_captions,
  file.path(documentation_dir, "Supplementary_Figure_Captions_Draft.txt")
)

output_index <- tibble::tribble(
  ~Category, ~Number, ~Title, ~Directory, ~File_stub,
  "Main figure", "Figure 1",
  "Breast cancer burden and temporal trends",
  "Main_Figures",
  "Figure_1_Breast_cancer_burden_and_temporal_trends",
  "Main figure", "Figure 2",
  "Decomposition and birth-cohort patterns",
  "Main_Figures",
  "Figure_2_Decomposition_and_birth_cohort_patterns",
  "Main figure", "Figure 3",
  "GBD risk attribution and exploratory SHAP ranking",
  "Main_Figures",
  "Figure_3_GBD_risk_attribution_and_exploratory_SHAP_ranking",
  "Main figure", "Figure 4",
  "MIR and mortality-related productivity loss",
  "Main_Figures",
  "Figure_4_MIR_and_mortality_related_productivity_loss",
  "Main figure", "Figure 5",
  "Status-quo incidence projections",
  "Main_Figures",
  "Figure_5_Status_quo_incidence_projections",
  "Main figure", "Figure 6",
  "GBD and GLOBOCAN external comparison",
  "Main_Figures",
  "Figure_6_GBD_and_GLOBOCAN_external_comparison",
  "Main table", "Table 1",
  "Breast cancer burden and temporal trends",
  "Main_Tables",
  "Table_1_Breast_cancer_burden_and_temporal_trends",
  "Supplementary figure", "Figure S1",
  "SHAP temporal stability",
  "Supplementary_Figures",
  "Supplementary_Figure_S1_SHAP_temporal_stability",
  "Supplementary figure", "Figure S2",
  "SHAP structure and target sensitivity",
  "Supplementary_Figures",
  "Supplementary_Figure_S2_SHAP_structure_and_target_sensitivity",
  "Supplementary figure", "Figure S3",
  "Age-specific PAF trends",
  "Supplementary_Figures",
  "Supplementary_Figure_S3_Age_specific_PAF_trends",
  "Supplementary figure", "Figure S4",
  "Projection anchor sensitivity",
  "Supplementary_Figures",
  "Supplementary_Figure_S4_Projection_anchor_sensitivity",
  "Supplementary figure", "Figure S5",
  "GLOBOCAN baseline sensitivity",
  "Supplementary_Figures",
  "Supplementary_Figure_S5_GLOBOCAN_baseline_sensitivity",
  "Supplementary figure", "Figure S6",
  "HCA productivity-loss sensitivity",
  "Supplementary_Figures",
  "Supplementary_Figure_S6_HCA_productivity_loss_sensitivity"
)

readr::write_csv(
  output_index,
  file.path(manuscript_ready_dir, "MANUSCRIPT_OUTPUT_INDEX.csv")
)

# ------------------------------------------------------------------------------
# 8. Quality checks
# ------------------------------------------------------------------------------

expected_main_figure_files <- unlist(lapply(
  output_index %>%
    dplyr::filter(Category == "Main figure") %>%
    dplyr::pull(File_stub),
  function(file_stub) {
    file.path(
      main_figures_dir,
      paste0(file_stub, c(".pdf", ".png"))
    )
  }
))

expected_supp_figure_files <- unlist(lapply(
  output_index %>%
    dplyr::filter(Category == "Supplementary figure") %>%
    dplyr::pull(File_stub),
  function(file_stub) {
    file.path(
      supp_figures_dir,
      paste0(file_stub, c(".pdf", ".png"))
    )
  }
))

manuscript_output_checks <- tibble::tibble(
  Check = c(
    "Main Table 1 contains every target location",
    "Main Table 1 contains no missing cells",
    "Six main figures were exported as PDF and PNG",
    "Six supplementary figures were exported as PDF and PNG",
    "Seventeen supplementary CSV tables were exported",
    "The consolidated supplementary workbook exists",
    "Figure source-data files were exported",
    "The manuscript output index exists"
  ),
  Passed = c(
    nrow(table1_manuscript) == length(target_locations),
    !any(is.na(table1_manuscript)),
    all(file.exists(expected_main_figure_files)),
    all(file.exists(expected_supp_figure_files)),
    length(list.files(
      supp_tables_dir,
      pattern = "^Supplementary_Table_S[0-9]+.*\\.csv$"
    )) == 17L,
    file.exists(file.path(
      supp_tables_dir,
      "Supplementary_Tables_S1_to_S17.xlsx"
    )),
    length(list.files(source_data_dir, pattern = "\\.csv$")) >= 10L,
    file.exists(file.path(
      manuscript_ready_dir,
      "MANUSCRIPT_OUTPUT_INDEX.csv"
    ))
  )
) %>%
  dplyr::mutate(Status = dplyr::if_else(Passed, "PASS", "FAIL"))

readr::write_csv(
  manuscript_output_checks,
  file.path(
    documentation_dir,
    "MANUSCRIPT_OUTPUT_QUALITY_CHECKS.csv"
  )
)

if (any(!manuscript_output_checks$Passed)) {
  failed_manuscript_checks <- manuscript_output_checks %>%
    dplyr::filter(!Passed) %>%
    dplyr::pull(Check)

  stop(
    "Manuscript-ready output check(s) failed: ",
    paste(failed_manuscript_checks, collapse = "; ")
  )
}

writeLines(
  c(
    "Manuscript-ready output package",
    paste0("Completed: ", Sys.time()),
    "",
    "Main manuscript:",
    "- Six figures in PDF and PNG format",
    "- One manuscript-ready Table 1",
    "",
    "Supplementary Information:",
    "- Six supplementary figures",
    "- Seventeen supplementary tables",
    "- One consolidated Excel workbook",
    "",
    "The numbering is internally consistent within this output directory.",
    "Reconcile numbering with the final manuscript immediately before submission."
  ),
  file.path(documentation_dir, "README_MANUSCRIPT_OUTPUTS.txt")
)

message("✓ Manuscript-ready outputs created in: ", manuscript_ready_dir)

# ==============================================================================
# END EMBEDDED MODULE: 09_manuscript_outputs.R
# ==============================================================================


# ------------------------------------------------------------------------------
# 10B. v1.1.10 manuscript-output and consistency layer
# ------------------------------------------------------------------------------

message("▶ Applying v1.1.10 manuscript-output and consistency layer...")

required_v1_1_10_objects <- c(
  "analysis_version", "output_path", "target_locations", "age_specific",
  "all_decomp_results", "df_table2", "df_macro", "df_micro",
  "df_validation_table", "df_scatter", "df_econ_master",
  "hca_paf_measure_used", "table1_manuscript",
  "feature_summary", "bmi_selected_by_age", "temporal_boot_summary",
  "structure_decision_summary", "anchored_vs_unanchored",
  "main_vs_globocan_baseline_scaled", "hca_2023_scenario_totals",
  "burden_selected_ui", "risk_paf_selected", "mir_selected",
  "data_provenance", "df_paf_2023", "df_hca_summary",
  "df_bapc_selected", "df_shap_importance", "df_data3",
  "level2_risks", "level3_risks", "risk_name_clean", "df_bar"
)

missing_v1_1_10_objects <- required_v1_1_10_objects[
  !vapply(
    required_v1_1_10_objects,
    exists,
    logical(1),
    inherits = TRUE
  )
]

if (length(missing_v1_1_10_objects) > 0L) {
  stop(
    "The v1.1.10 consistency layer is missing required object(s): ",
    paste(missing_v1_1_10_objects, collapse = ", ")
  )
}

manuscript_ready_dir <- file.path(output_path, "Manuscript_Ready")
main_figures_dir <- file.path(manuscript_ready_dir, "Main_Figures")
main_tables_dir <- file.path(manuscript_ready_dir, "Main_Tables")
supp_figures_dir <- file.path(manuscript_ready_dir, "Supplementary_Figures")
supp_tables_dir <- file.path(manuscript_ready_dir, "Supplementary_Tables")
figure_source_data_dir <- file.path(
  manuscript_ready_dir,
  "Figure_Source_Data"
)
documentation_dir <- file.path(manuscript_ready_dir, "Documentation")

for (directory_path in c(
  manuscript_ready_dir,
  main_figures_dir,
  main_tables_dir,
  supp_figures_dir,
  supp_tables_dir,
  figure_source_data_dir,
  documentation_dir
)) {
  dir.create(directory_path, recursive = TRUE, showWarnings = FALSE)
}

# ------------------------------------------------------------------------------
# 1. Precise manuscript-facing GLOBOCAN comparison table
# ------------------------------------------------------------------------------

df_validation_table_v1_1_10 <- df_scatter %>%
  dplyr::mutate(
    Absolute_Difference = GBD_Rate - GLOBOCAN_Rate,
    Relative_Difference_Percent =
      100 * (GBD_Rate - GLOBOCAN_Rate) / GLOBOCAN_Rate
  ) %>%
  dplyr::select(
    Location = location_name,
    `Age group` = age_name,
    `GLOBOCAN case estimate, 2022` = Cases_2022,
    `Matched population denominator` = Population,
    `Rate derived from GLOBOCAN case estimates, per 100,000` =
      GLOBOCAN_Rate,
    `GBD incidence rate, per 100,000` = GBD_Rate,
    `Absolute rate difference` = Absolute_Difference,
    `Relative rate difference (%)` = Relative_Difference_Percent
  ) %>%
  dplyr::arrange(Location, `Age group`) %>%
  dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 2)))

readr::write_csv(
  df_validation_table_v1_1_10,
  file.path(
    supp_tables_dir,
    "Supplementary_Table_S1_GBD_vs_GLOBOCAN_2022.csv"
  )
)

readr::write_csv(
  df_validation_table_v1_1_10,
  file.path(
    figure_source_data_dir,
    "Figure_6_comparison.csv"
  )
)

# ------------------------------------------------------------------------------
# 2. Main Table 2: China-only sequential decomposition pathway
# ------------------------------------------------------------------------------

china_decomposition <- all_decomp_results %>%
  dplyr::filter(Location == "China")

if (nrow(china_decomposition) != 1L) {
  stop(
    "Expected exactly one China decomposition row, but found ",
    nrow(china_decomposition), "."
  )
}

china_total_increase <-
  china_decomposition$Cases_2023 -
  china_decomposition$Cases_1990

if (!is.finite(china_total_increase) || china_total_increase <= 0) {
  stop("The China decomposition total increase is invalid.")
}

# Reconstruct the sequential counterfactual pathway from the unrounded
# decomposition components. This changes only the manuscript table layout;
# the decomposition method and numerical results are unchanged.
china_population_stage <-
  china_decomposition$Cases_1990 +
  china_decomposition$Population_Growth

china_demographic_stage <-
  china_population_stage +
  china_decomposition$Age_Structure

# Preserve unrounded values for internal consistency checks.
table2_pathway_unrounded <- tibble::tibble(
  `Sequential stage` = c(
    "Observed 1990 baseline",
    "After replacing total population size",
    "After additionally replacing age structure",
    "Observed 2023 endpoint"
  ),
  `Component represented` = c(
    NA_character_,
    "Population growth",
    "Age-structure change",
    "Residual epidemiological component"
  ),
  `Estimated incident cases, n` = c(
    china_decomposition$Cases_1990,
    china_population_stage,
    china_demographic_stage,
    china_decomposition$Cases_2023
  ),
  `Incremental contribution, cases` = c(
    NA_real_,
    china_decomposition$Population_Growth,
    china_decomposition$Age_Structure,
    china_decomposition$Epidemiological_Risk
  ),
  `Share of total increase, %` = c(
    NA_real_,
    100 * china_decomposition$Population_Growth /
      china_total_increase,
    100 * china_decomposition$Age_Structure /
      china_total_increase,
    100 * china_decomposition$Epidemiological_Risk /
      china_total_increase
  )
)

# Verify the complete unrounded pathway before rounding for presentation.
if (
  !isTRUE(all.equal(
    china_population_stage,
    china_decomposition$Cases_1990 +
      china_decomposition$Population_Growth,
    tolerance = 1e-10
  )) ||
  !isTRUE(all.equal(
    china_demographic_stage,
    china_decomposition$Cases_1990 +
      china_decomposition$Population_Growth +
      china_decomposition$Age_Structure,
    tolerance = 1e-10
  )) ||
  !isTRUE(all.equal(
    china_decomposition$Cases_2023,
    china_demographic_stage +
      china_decomposition$Epidemiological_Risk,
    tolerance = 1e-8
  ))
) {
  stop(
    "The China sequential decomposition pathway does not reproduce ",
    "the unrounded decomposition results."
  )
}

# Round stage counts, component contributions, and percentages separately
# for manuscript presentation.
table2_manuscript <- table2_pathway_unrounded %>%
  dplyr::mutate(
    `Estimated incident cases, n` =
      round(`Estimated incident cases, n`, 0),
    `Incremental contribution, cases` =
      round(`Incremental contribution, cases`, 0),
    `Share of total increase, %` =
      round(`Share of total increase, %`, 1)
  )

readr::write_csv(
  table2_manuscript,
  file.path(
    main_tables_dir,
    "Table_2_Sequential_decomposition_of_increased_cases.csv"
  ),
  na = ""
)

writeLines(
  c(
    "Table 2 notes",
    "",
    paste0(
      "China incident cases increased from ",
      scales::comma(round(china_decomposition$Cases_1990)),
      " in 1990 to ",
      scales::comma(round(china_decomposition$Cases_2023)),
      " in 2023."
    ),
    paste(
      "The table presents the complete sequential decomposition pathway.",
      "The observed 1990 case count serves as the baseline."
    ),
    paste(
      "Total population size was replaced first, followed by age structure;",
      "the remaining difference between the demographic counterfactual and",
      "the observed 2023 case count was defined as the residual",
      "epidemiological component."
    ),
    paste(
      "The residual epidemiological component is descriptive and should not",
      "be interpreted as a directly measured causal exposure or metabolic",
      "mechanism."
    ),
    paste(
      "Stage-specific case counts, incremental contributions, and percentages",
      "were calculated from unrounded estimates and rounded separately for",
      "presentation. Differences between adjacent displayed stage counts may",
      "therefore not exactly equal the displayed component contributions."
    ),
    paste(
      "Negative component values, if present in another location, indicate",
      "that the corresponding component reduced rather than increased the",
      "overall change."
    ),
    paste(
      "Full decomposition results for China, the global population, and the",
      "five SDI strata are provided in Supplementary Table S2."
    )
  ),
  file.path(
    documentation_dir,
    "Table_2_notes.txt"
  )
)

# ------------------------------------------------------------------------------
# 3. Precise terminology in provenance and supplementary workbook
# ------------------------------------------------------------------------------

replace_globocan_wording <- function(x) {
  x <- stringr::str_replace_all(
    x,
    stringr::fixed("GLOBOCAN registry-based estimates"),
    "rates derived from GLOBOCAN case estimates"
  )
  x <- stringr::str_replace_all(
    x,
    stringr::fixed("GLOBOCAN registry-based estimate"),
    "rate derived from GLOBOCAN case estimates"
  )
  x <- stringr::str_replace_all(
    x,
    stringr::fixed("registry-based comparator"),
    "case-estimate-derived comparator"
  )
  x <- stringr::str_replace_all(
    x,
    stringr::fixed("registry-based"),
    "case-estimate-derived"
  )
  x
}

data_provenance_v1_1_10 <- data_provenance %>%
  dplyr::mutate(
    dplyr::across(
      where(is.character),
      replace_globocan_wording
    )
  )

readr::write_csv(
  data_provenance_v1_1_10,
  file.path(
    supp_tables_dir,
    "Supplementary_Table_S17_Data_provenance.csv"
  )
)

# Rebuild the consolidated supplementary workbook so the corrected S1 and S17
# content is also present in the Excel file.
supplementary_workbook_v1_1_10 <- list(
  S1_GBD_GLOBOCAN = df_validation_table_v1_1_10,
  S2_Decomposition = df_table2,
  S3_PAF_SHAP = df_paf_2023,
  S4_HCA_selected = df_hca_summary %>%
    dplyr::filter(year %in% c(1990, 2000, 2010, 2020, 2023)),
  S5_BAPC_selected = df_bapc_selected,
  S6_SHAP_status = df_shap_importance,
  S7_Completeness = feature_summary,
  S8_BMI_by_age = bmi_selected_by_age,
  S9_Temporal_SHAP = temporal_boot_summary,
  S10_Structure_SHAP = structure_decision_summary,
  S11_Anchor = anchored_vs_unanchored,
  S12_GLOBOCAN_sens = manuscript_ready_baseline_scaled,
  S13_HCA_sens = hca_2023_scenario_totals,
  S14_Burden_UI = burden_selected_ui,
  S15_PAF_trends = risk_paf_selected,
  S16_MIR = mir_selected,
  S17_Provenance = data_provenance_v1_1_10
)

openxlsx::write.xlsx(
  supplementary_workbook_v1_1_10,
  file = file.path(
    supp_tables_dir,
    "Supplementary_Tables_S1_to_S17.xlsx"
  ),
  overwrite = TRUE
)

# ------------------------------------------------------------------------------
# 4. Corrected captions
# ------------------------------------------------------------------------------

main_figure_captions_v1_1_10 <- c(
  paste(
    "Figure 1. Breast cancer burden and temporal trends, 1990-2023.",
    "The upper panels show global all-age age-standardized burden rates in",
    "2023. The lower panels show author-recalculated weighted rates among",
    "women aged 25-49 years and the YLL/YLD composition of DALYs in China."
  ),
  paste(
    "Figure 2. Sequential decomposition of increased incident cases and descriptive",
    "cohort-specific incidence patterns. Panel A shows the sequential",
    "decomposition for China. Panel B displays each signed component divided",
    "by the same total increase; negative values therefore remain negative.",
    "Panel C is descriptive, and later cohorts are shown only for ages",
    "observable within 1990-2023."
  ),
  paste(
    "Figure 3. Risk-attribution patterns and exploratory feature ranking.",
    "Panel A shows broad DALY PAF categories only. Panel B shows the SHAP",
    "summary for the four nonconstant China-specific DALY PAF features.",
    "Panels C and D show signed risk-specific DALY PAFs by age and year.",
    "High BMI was excluded from ranking because its China series was constant",
    "at zero. SHAP values do not establish causality."
  ),
  paste(
    "Figure 4. Mortality-to-incidence ratios and mortality-related",
    "productivity losses. Panel A shows weighted ASDR-to-ASIR ratios for ages",
    "25-49 years; China is displayed as a fixed black line with black points,",
    "without a year-color legend. Panels B and C report Human Capital Approach",
    "estimates in constant 2015 US dollars using YLL-specific PAFs."
  ),
  paste(
    "Figure 5. Status-quo Bayesian age-period-cohort projections of incidence",
    "to 2050. The dotted line marks the 2023 transition. Shaded bands represent",
    "80% and 95% posterior uncertainty intervals after multiplicative anchoring",
    "of posterior means and interval bounds to observed 2023 rates."
  ),
  paste(
    "Figure 6. External comparison of GBD 2022 incidence rates and rates",
    "derived from GLOBOCAN 2022 age-specific case estimates. GLOBOCAN case",
    "estimates were divided by the matched study population denominators to",
    "construct comparison rates. Pearson correlation summarizes age-pattern",
    "concordance and is not formal external validation of the BAPC model."
  )
)

writeLines(
  main_figure_captions_v1_1_10,
  file.path(
    documentation_dir,
    "Main_Figure_Captions_Draft.txt"
  )
)

supplementary_figure_captions_v1_1_10 <- c(
  paste(
    "Supplementary Figure S1. Temporal stability of the exploratory SHAP",
    "ranking. Fold-specific SHAP values were calculated exclusively in",
    "held-out future-period observations. The grouped bootstrap sampled six",
    "non-overlapping 5-year blocks and one final 4-year block with replacement,",
    "retaining all five age groups within each selected year. High BMI was",
    "constant at zero in the selected GBD series and was therefore not rankable;",
    "this does not indicate absent obesity exposure or biological irrelevance."
  ),
  paste(
    "Supplementary Figure S2. Sensitivity of the exploratory SHAP ranking to",
    "target scale, population size, between-age differences, annual changes,",
    "and residual age/time structure."
  ),
  paste(
    "Supplementary Figure S3. Age-specific temporal patterns in signed",
    "China-specific GBD DALY PAFs from 1990 to 2023. The High BMI series is",
    "constant at zero."
  ),
  paste(
    "Supplementary Figure S4. Sensitivity of BAPC projections to",
    "multiplicative anchoring at the observed 2023 incidence rate."
  ),
  paste(
    "Supplementary Figure S5. Sensitivity analysis using rates derived from",
    "GLOBOCAN 2022 case estimates as an external incidence baseline. This",
    "scenario preserves the GBD-derived trajectory and is not an independent",
    "GLOBOCAN forecast. The scaled intervals retain the relative posterior",
    "uncertainty of the main model but do not incorporate uncertainty in the",
    "GLOBOCAN-to-GBD baseline-scaling factors."
  ),
  paste(
    "Supplementary Figure S6. Sensitivity of Human Capital Approach",
    "productivity-loss estimates to illustrative productivity-value and",
    "age-allocation assumptions; the age multipliers are not empirical",
    "age-specific female wage estimates."
  )
)

writeLines(
  supplementary_figure_captions_v1_1_10,
  file.path(
    documentation_dir,
    "Supplementary_Figure_Captions_Draft.txt"
  )
)

# ------------------------------------------------------------------------------
# 5. Output index and run/upload documentation
# ------------------------------------------------------------------------------

output_index_path <- file.path(
  manuscript_ready_dir,
  "MANUSCRIPT_OUTPUT_INDEX.csv"
)

if (!file.exists(output_index_path)) {
  stop("The manuscript output index was not created by the embedded manuscript-output section.")
}

supplementary_table_index_v1_1_10 <- tibble::tribble(
  ~Category, ~Number, ~Title, ~Directory, ~File_stub,
  "Supplementary table", "Table S1", "GBD and GLOBOCAN 2022 external comparison", "Supplementary_Tables", "Supplementary_Table_S1_GBD_vs_GLOBOCAN_2022",
  "Supplementary table", "Table S2", "Sequential decomposition results", "Supplementary_Tables", "Supplementary_Table_S2_Decomposition_results",
  "Supplementary table", "Table S3", "PAF and exploratory SHAP ranking in 2023", "Supplementary_Tables", "Supplementary_Table_S3_PAF_and_SHAP_ranking_2023",
  "Supplementary table", "Table S4", "Mortality-related productivity losses in selected years", "Supplementary_Tables", "Supplementary_Table_S4_Productivity_losses_selected_years",
  "Supplementary table", "Table S5", "BAPC projections in selected years with 95% uncertainty intervals", "Supplementary_Tables", "Supplementary_Table_S5_BAPC_selected_years_95UI",
  "Supplementary table", "Table S6", "SHAP feature status and importance", "Supplementary_Tables", "Supplementary_Table_S6_SHAP_feature_status_and_importance",
  "Supplementary table", "Table S7", "PAF feature completeness", "Supplementary_Tables", "Supplementary_Table_S7_PAF_feature_completeness",
  "Supplementary table", "Table S8", "High-BMI source validation by age", "Supplementary_Tables", "Supplementary_Table_S8_High_BMI_source_validation_by_age",
  "Supplementary table", "Table S9", "Temporal SHAP bootstrap summary", "Supplementary_Tables", "Supplementary_Table_S9_SHAP_temporal_bootstrap_summary",
  "Supplementary table", "Table S10", "SHAP structure-sensitivity summary", "Supplementary_Tables", "Supplementary_Table_S10_SHAP_structure_sensitivity_summary",
  "Supplementary table", "Table S11", "Anchored versus unanchored projection", "Supplementary_Tables", "Supplementary_Table_S11_Anchored_vs_unanchored_projection",
  "Supplementary table", "Table S12", "GLOBOCAN-based baseline-scaled projection sensitivity", "Supplementary_Tables", "Supplementary_Table_S12_GLOBOCAN_baseline_scaled_projection",
  "Supplementary table", "Table S13", "HCA sensitivity scenarios in 2023", "Supplementary_Tables", "Supplementary_Table_S13_HCA_sensitivity_scenarios_2023",
  "Supplementary table", "Table S14", "Burden counts, rates, and 95% uncertainty intervals", "Supplementary_Tables", "Supplementary_Table_S14_Burden_counts_rates_and_95UI",
  "Supplementary table", "Table S15", "Age-specific PAFs in selected years", "Supplementary_Tables", "Supplementary_Table_S15_Age_specific_PAF_selected_years",
  "Supplementary table", "Table S16", "Mortality-to-incidence ratios in selected years", "Supplementary_Tables", "Supplementary_Table_S16_MIR_selected_years",
  "Supplementary table", "Table S17", "Data provenance", "Supplementary_Tables", "Supplementary_Table_S17_Data_provenance",
  "Supplementary workbook", "Tables S1-S17", "Consolidated supplementary tables workbook", "Supplementary_Tables", "Supplementary_Tables_S1_to_S17"
)

output_index_v1_1_10 <- readr::read_csv(
  output_index_path,
  show_col_types = FALSE
) %>%
  dplyr::filter(Number != "Table 2") %>%
  dplyr::bind_rows(
    tibble::tibble(
      Category = "Main table",
      Number = "Table 2",
      Title = "Sequential decomposition pathway of increased incident cases in China",
      Directory = "Main_Tables",
      File_stub = "Table_2_Sequential_decomposition_of_increased_cases"
    ),
    supplementary_table_index_v1_1_10
  ) %>%
  dplyr::mutate(
    Category_order = dplyr::case_when(
      Category == "Main figure" ~ 1L,
      Category == "Main table" ~ 2L,
      Category == "Supplementary figure" ~ 3L,
      Category == "Supplementary table" ~ 4L,
      Category == "Supplementary workbook" ~ 5L,
      TRUE ~ 6L
    ),
    Number_order = readr::parse_number(Number)
  ) %>%
  dplyr::arrange(Category_order, Number_order) %>%
  dplyr::select(-Category_order, -Number_order)

readr::write_csv(output_index_v1_1_10, output_index_path)

writeLines(
  c(
    paste0("GBD EOBC manuscript-ready output package ", analysis_version),
    paste0("Completed: ", Sys.time()),
    "",
    "Main manuscript:",
    "- Six figures in PDF and PNG format",
    "- Two manuscript-ready CSV tables",
    "",
    "Supplementary Information:",
    "- Six supplementary figures in PDF and PNG format",
    "- Seventeen supplementary CSV tables",
    "- One consolidated Excel workbook",
    "",
    "Validated v1.1.8 analytical foundation retained:",
    "- Core data, model specifications, random seeds, formulas, and numerical results are unchanged",
    "",
    "v1.1.10 reporting and release refinements:",
    "- Figure S1 subtitle shortened to prevent clipping; full interpretation retained in the caption",
    "- GLOBOCAN publication-facing and audit outputs use baseline-scaling terminology throughout",
    "- Table S12 uses relative change rather than relative increase",
    "- Figure S5 states that baseline-scaling-factor uncertainty is not propagated",
    "- HCA figure labels multipliers as illustrative assumptions rather than empirical wage estimates",
    "- Manuscript output index includes all figures, main tables, supplementary tables, and the consolidated workbook",
    "- Version metadata and quality-check filenames are standardized to v1.1.10",
    "",
    "Reconcile all numbering and wording with the revised manuscript before submission."
  ),
  file.path(documentation_dir, "README_MANUSCRIPT_OUTPUTS.txt")
)

writeLines(
  c(
    "Files to upload after the final v1.1.10 run",
    "",
    "Preferred method:",
    "1. Create a clean ZIP of the entire Results/Manuscript_Ready folder, excluding __MACOSX, .DS_Store, and ._* files.",
    "2. Add Results/Logs/sessionInfo_final.txt and Results/Logs/sessionInfo.txt.",
    "3. Add Results/Analysis_Tables/HCA_PAF_truncation_audit.csv.",
    "4. Upload that single ZIP file.",
    "",
    "Do not upload the raw GBD, UN, ILO, World Bank, or GLOBOCAN source files unless specifically requested.",
    "",
    "Minimum files when a ZIP cannot be uploaded:",
    "- Main_Figures/Figure_2_Decomposition_and_birth_cohort_patterns.png",
    "- Main_Figures/Figure_3_GBD_risk_attribution_and_exploratory_SHAP_ranking.png",
    "- Main_Figures/Figure_4_MIR_and_mortality_related_productivity_loss.png",
    "- Main_Figures/Figure_6_GBD_and_GLOBOCAN_external_comparison.png",
    "- Main_Tables/Table_2_Sequential_decomposition_of_increased_cases.csv",
    "- Supplementary_Tables/Supplementary_Tables_S1_to_S17.xlsx",
    "- Documentation/V1_1_10_OUTPUT_QUALITY_CHECKS.csv",
    "- Documentation/MANUSCRIPT_OUTPUT_QUALITY_CHECKS.csv",
    "- Logs/sessionInfo_final.txt",
    "- Analysis_Tables/HCA_PAF_truncation_audit.csv",
    "- Analysis_Tables/Computational_Evidence_Audit_Summary_v1.1.10.csv",
    "- Sensitivity_Results/Reporting_Assumptions_v1.1.10.txt"
  ),
  file.path(documentation_dir, "RUN_AND_UPLOAD_CHECKLIST.txt")
)

writeLines(
  c(
    paste0("Analysis version: ", analysis_version),
    paste0("Completed: ", Sys.time()),
    "This v1.1.10 release is a final reporting-and-packaging patch based on the validated v1.1.8 analysis. It standardizes publication-facing terminology, corrects output labels and documentation, completes the manuscript output index, and adds release-hygiene guidance. No input data, model specification, statistical calculation, random seed, or numerical result was changed."
  ),
  file.path(documentation_dir, "ANALYSIS_VERSION.txt")
)

patch_summary <- tibble::tribble(
  ~Item, ~Status, ~Description,
  "Analytical foundation", "UNCHANGED", "Validated v1.1.8 inputs, models, formulas, parameters, seeds, and numerical results retained",
  "Supplementary Figure S1", "LAYOUT FIXED", "Shortened in-figure subtitle prevents clipping; full high-BMI interpretation moved to the caption",
  "GLOBOCAN outputs", "STANDARDIZED", "Baseline-scaling terminology used in publication-facing figures, tables, raw audit exports, and workbook sheets",
  "Supplementary Table S12", "CORRECTED", "Relative change replaces relative increase because some external-baseline factors are below one",
  "Supplementary Figure S5", "CLARIFIED", "Figure states that scaling-factor uncertainty is not propagated",
  "HCA sensitivity figure", "CLARIFIED", "Multipliers described as illustrative assumptions rather than empirical age-specific female wage estimates",
  "Manuscript output index", "COMPLETED", "Index includes six main figures, two main tables, six supplementary figures, seventeen supplementary tables, and the consolidated workbook",
  "Documentation and packaging", "STANDARDIZED", "Version labels, quality-check filenames, release notes, and clean-ZIP guidance updated to v1.1.10"
)

readr::write_csv(
  patch_summary,
  file.path(documentation_dir, "V1_1_10_REPORTING_AND_PACKAGING_SUMMARY.csv")
)

# ------------------------------------------------------------------------------
# 6. Version-specific quality checks
# ------------------------------------------------------------------------------

expected_main_figure_files <- unlist(lapply(
  output_index_v1_1_10 %>%
    dplyr::filter(Category == "Main figure") %>%
    dplyr::pull(File_stub),
  function(file_stub) {
    file.path(
      main_figures_dir,
      paste0(file_stub, c(".pdf", ".png"))
    )
  }
))

expected_supp_figure_files <- unlist(lapply(
  output_index_v1_1_10 %>%
    dplyr::filter(Category == "Supplementary figure") %>%
    dplyr::pull(File_stub),
  function(file_stub) {
    file.path(
      supp_figures_dir,
      paste0(file_stub, c(".pdf", ".png"))
    )
  }
))

main_table_paths <- c(
  file.path(
    main_tables_dir,
    "Table_1_Breast_cancer_burden_and_temporal_trends.csv"
  ),
  file.path(
    main_tables_dir,
    "Table_2_Sequential_decomposition_of_increased_cases.csv"
  )
)

figure3_expected_macro <- df_data3 %>%
  dplyr::filter(
    location_name %in% c("China", "Global"),
    metric_name == "Percent",
    stringr::str_detect(measure_name, "DALY"),
    age_name %in% age_specific,
    rei_name %in% level2_risks
  ) %>%
  dplyr::group_by(year, location_name, rei_name) %>%
  dplyr::summarise(val = mean(val, na.rm = TRUE), .groups = "drop") %>%
  dplyr::arrange(year, location_name, rei_name)

figure3_expected_micro <- df_data3 %>%
  dplyr::filter(
    location_name %in% c("China", "Global"),
    metric_name == "Percent",
    stringr::str_detect(measure_name, "DALY"),
    age_name %in% age_specific,
    rei_name %in% level3_risks
  ) %>%
  dplyr::mutate(
    Risk_Factor = risk_name_clean(rei_name),
    age_name = factor(age_name, levels = age_specific)
  ) %>%
  dplyr::group_by(location_name, year, age_name, Risk_Factor) %>%
  dplyr::summarise(val = mean(val, na.rm = TRUE), .groups = "drop") %>%
  dplyr::arrange(location_name, year, age_name, Risk_Factor)

figure3_actual_macro <- df_macro %>%
  dplyr::arrange(year, location_name, rei_name)

figure3_actual_micro <- df_micro %>%
  dplyr::arrange(location_name, year, age_name, Risk_Factor)

v1_1_10_output_checks <- tibble::tibble(
  Check = c(
    "Analysis version is v1.1.10",
    "Main Table 2 contains four sequential stages",
    "Main Table 2 component shares sum to 100%",
    "Main Table 2 pathway reproduces the decomposition results",
    "Figure 2 relative contributions sum to 100% within location",
    "Figure 3 broad PAF object equals DALY-only reconstruction",
    "Figure 3 specific PAF object equals DALY-only reconstruction",
    "HCA uses YLL-specific PAFs",
    "HCA contains no missing source PAF after joining",
    "GLOBOCAN comparison contains ten matched rows",
    "GLOBOCAN manuscript table uses case-estimate-derived terminology",
    "Six main figures exist as PDF and PNG",
    "Six supplementary figures exist as PDF and PNG",
    "Two main manuscript tables exist",
    "Seventeen supplementary CSV tables exist",
    "Consolidated supplementary workbook exists",
    "Manuscript output index contains all 32 expected entries",
    "Manuscript output index includes 17 supplementary tables and the consolidated workbook",
    "Run-and-upload checklist exists"
  ),
  Passed = c(
    identical(analysis_version, "v1.1.10"),
    nrow(table2_manuscript) == 4L,
    abs(
      sum(
        table2_manuscript[["Share of total increase, %"]],
        na.rm = TRUE
      ) - 100
    ) <= 0.2,
    (
      abs(
        sum(
          table2_manuscript[["Incremental contribution, cases"]],
          na.rm = TRUE
        ) -
          round(china_total_increase)
      ) <= 1
    ) &&
      isTRUE(all.equal(
        table2_manuscript[["Estimated incident cases, n"]],
        round(c(
          china_decomposition$Cases_1990,
          china_population_stage,
          china_demographic_stage,
          china_decomposition$Cases_2023
        )),
        check.attributes = FALSE,
        tolerance = 0
      )),
    all(
      abs(
        df_bar %>%
          dplyr::group_by(Location) %>%
          dplyr::summarise(
            Signed_share_sum = sum(Relative_Contribution),
            .groups = "drop"
          ) %>%
          dplyr::pull(Signed_share_sum) - 1
      ) < 1e-8
    ),
    isTRUE(all.equal(
      figure3_actual_macro,
      figure3_expected_macro,
      check.attributes = FALSE,
      tolerance = 1e-12
    )),
    isTRUE(all.equal(
      figure3_actual_micro,
      figure3_expected_micro,
      check.attributes = FALSE,
      tolerance = 1e-12
    )),
    identical(hca_paf_measure_used, "YLL"),
    !any(is.na(df_econ_master$PAF_Source)),
    nrow(df_scatter) == 10L,
    "Rate derived from GLOBOCAN case estimates, per 100,000" %in%
      names(df_validation_table_v1_1_10),
    all(file.exists(expected_main_figure_files)),
    all(file.exists(expected_supp_figure_files)),
    all(file.exists(main_table_paths)),
    length(list.files(
      supp_tables_dir,
      pattern = "^Supplementary_Table_S[0-9]+.*\\.csv$"
    )) == 17L,
    file.exists(file.path(
      supp_tables_dir,
      "Supplementary_Tables_S1_to_S17.xlsx"
    )),
    nrow(output_index_v1_1_10) == 32L,
    sum(output_index_v1_1_10$Category == "Supplementary table") == 17L &&
      sum(output_index_v1_1_10$Category == "Supplementary workbook") == 1L,
    file.exists(file.path(
      documentation_dir,
      "RUN_AND_UPLOAD_CHECKLIST.txt"
    ))
  )
) %>%
  dplyr::mutate(Status = dplyr::if_else(Passed, "PASS", "FAIL"))

readr::write_csv(
  v1_1_10_output_checks,
  file.path(
    documentation_dir,
    "V1_1_10_OUTPUT_QUALITY_CHECKS.csv"
  )
)

if (any(!v1_1_10_output_checks$Passed)) {
  failed_v1_1_10_checks <- v1_1_10_output_checks %>%
    dplyr::filter(!Passed) %>%
    dplyr::pull(Check)

  stop(
    "v1.1.10 output check(s) failed: ",
    paste(failed_v1_1_10_checks, collapse = "; ")
  )
}

message("✓ v1.1.10 output and consistency checks completed successfully.")


# ------------------------------------------------------------------------------
# 11. Optional Word table export
# Disabled by default to avoid package conflicts. CSV files are already exported.
# ------------------------------------------------------------------------------

make_word_tables <- FALSE

if (make_word_tables) {
  if (!requireNamespace("flextable", quietly = TRUE)) install.packages("flextable")
  if (!requireNamespace("officer", quietly = TRUE)) install.packages("officer")

  format_sci_table <- function(ft_obj, table_caption) {
    ft_obj <- flextable::theme_booktabs(ft_obj)
    ft_obj <- flextable::bold(ft_obj, part = "header")
    ft_obj <- flextable::align(ft_obj, align = "center", part = "all")
    ft_obj <- flextable::align(ft_obj, j = 1, align = "left", part = "all")
    ft_obj <- flextable::fontsize(ft_obj, size = 8, part = "all")
    ft_obj <- flextable::font(ft_obj, fontname = "Times New Roman", part = "all")
    ft_obj <- flextable::set_caption(ft_obj, caption = table_caption)
    ft_obj <- flextable::autofit(ft_obj)
    return(ft_obj)
  }

  doc <- officer::read_docx()
  doc <- officer::body_add_par(doc, "Table 1", style = "heading 1")
  doc <- flextable::body_add_flextable(doc, format_sci_table(flextable::flextable(df_table1_final), "Table 1. Breast cancer burden and temporal trends"))
  doc <- officer::body_add_break(doc)
  doc <- officer::body_add_par(doc, "Supplementary Tables", style = "heading 1")
  doc <- flextable::body_add_flextable(doc, format_sci_table(flextable::flextable(df_table2), "Supplementary Table S2. Decomposition results"))
  print(doc, target = file.path(tables_dir, "All_Manuscript_Tables_Final.docx"))
}

writeLines(capture.output(sessionInfo()), file.path(logs_dir, "sessionInfo.txt"))


# ------------------------------------------------------------------------------
# 10C. v1.1.10 reporting-only evidence audit metadata
# ------------------------------------------------------------------------------

reporting_patch_summary <- tibble::tibble(
  Audit_scope = "Computational evidence only",
  Audit_item = c(
    "XGBoost temporal hyperparameter leakage",
    "Fold-specific SHAP evaluation set",
    "Calendar-block bootstrap structure",
    "GLOBOCAN external-baseline sensitivity",
    "HCA age multipliers",
    "High-BMI constant-zero feature"
  ),
  Evidence_status = c(
    "No held-out tuning, early stopping, or model selection in the pipeline",
    "Confirmed: SHAP calculated in held-out future-period observations",
    "Confirmed: six non-overlapping 5-year blocks plus one final 4-year block; sampled with replacement; model refitted",
    "Confirmed: fixed baseline-scaling factors; factor uncertainty not propagated",
    "Confirmed: illustrative raw multipliers; normalized scenario preserves 2023 total monetary scale",
    "Confirmed: 170 observed zeros; not rankable; no inference about obesity prevalence or biological importance"
  ),
  Analytical_result_changed = FALSE
)

readr::write_csv(
  reporting_patch_summary,
  file.path(analysis_tables_dir, "Computational_Evidence_Audit_Summary_v1.1.10.csv")
)

reporting_assumptions_v1_1_10 <- c(
  paste0("Analysis version: ", analysis_version),
  "Patch type: final reporting, documentation, and packaging update.",
  "No input data, model formula, XGBoost parameter, random seed, projection model, HCA formula, or numerical result was intentionally changed.",
  "Temporal folds: hyperparameters fixed in the pipeline; held-out periods not used for tuning, early stopping, or model selection.",
  "Fold SHAP: calculated exclusively on held-out future-period observations.",
  "Bootstrap: six complete non-overlapping 5-year blocks and one final 4-year block, sampled with replacement; all five age groups retained; model refitted in every replicate; SHAP evaluated on a fixed full-panel support.",
  "GLOBOCAN analysis: external-baseline scaling sensitivity, not external validation or independent forecasting; scaling-factor uncertainty was not propagated.",
  "HCA multipliers: illustrative scenario values, not observed age-specific female wage estimates; normalized age-graded scenario preserves the 2023 total monetary scale.",
  "High BMI: selected China-specific breast-cancer DALY PAF series is constant zero across 170 observations; not rankable and not evidence of absent obesity exposure or biological relevance."
)
writeLines(
  reporting_assumptions_v1_1_10,
  con = file.path(sensitivity_results_dir, "Reporting_Assumptions_v1.1.10.txt")
)

writeLines(
  c(
    "v1.1.10 release-hygiene requirements",
    "",
    "Before uploading Manuscript_Ready, Data S1, GitHub Release assets, or Zenodo files:",
    "1. Remove __MACOSX directories, .DS_Store files, and AppleDouble files beginning with ._.",
    "2. Create the archive from the project root using the supplied create_clean_release_zip.py utility or another exclusion-aware archiver.",
    "3. Confirm that MANUSCRIPT_OUTPUT_INDEX.csv contains 32 entries.",
    "4. Confirm that V1_1_10_OUTPUT_QUALITY_CHECKS.csv and MANUSCRIPT_OUTPUT_QUALITY_CHECKS.csv contain only PASS results.",
    "5. Do not include third-party raw source files in the public release unless redistribution is permitted."
  ),
  con = file.path(documentation_dir, "RELEASE_HYGIENE_v1.1.10.txt")
)

message("✓ v1.1.10 reporting-only evidence audit metadata exported.")

message("🎉 Full pipeline completed successfully: ", analysis_version)
message("Figures saved in: ", fig_pdf_dir, " and ", fig_png_dir)
message("Tables saved in: ", tables_dir)
