# ============================================================
# run_all.R
#
# Run the complete reproducible epidemiology workflow.
# ============================================================


# 01. Generate synthetic epidemiologic data
source("scripts/01_generate_synthetic_data.R")


# 02. Produce descriptive statistics and Table 1
source("scripts/02_descriptive_analysis.R")


# 03. Fit multivariable regression models
source("scripts/03_regression_models.R")


# 04. Conduct interaction and spline analyses
source("scripts/04_interaction_rcs.R")


# 05. Conduct subgroup and sensitivity analyses
source("scripts/05_subgroup_sensitivity.R")


# 06. Generate publication-style figures
source("scripts/06_figures.R")


# ------------------------------------------------------------
# Save R session information for reproducibility
# ------------------------------------------------------------

if (!dir.exists("output")) {
  dir.create("output")
}

capture.output(
  sessionInfo(),
  file = "output/session_info.txt"
)


cat(
  "\n============================================\n",
  "Complete analysis workflow finished successfully.\n",
  "All generated tables and figures are available in output/.\n",
  "============================================\n"
)