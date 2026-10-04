# ============================================================
# 02_descriptive_analysis.R
#
# Purpose:
# Produce descriptive statistics from the synthetic
# epidemiology dataset generated in Script 01.
# ============================================================


# ------------------------------------------------------------
# 1. Read synthetic dataset
# ------------------------------------------------------------

data <- read.csv(
  "data/synthetic_epidemiology_data.csv",
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------
# 2. Check dataset structure
# ------------------------------------------------------------

dim(data)
names(data)
head(data)


# ------------------------------------------------------------
# 3. Helper functions
# ------------------------------------------------------------

mean_sd <- function(x) {
  sprintf(
    "%.1f (%.1f)",
    mean(x, na.rm = TRUE),
    sd(x, na.rm = TRUE)
  )
}


n_percent <- function(x, level) {
  
  denominator <- sum(!is.na(x))
  numerator <- sum(x == level, na.rm = TRUE)
  
  sprintf(
    "%d (%.1f%%)",
    numerator,
    100 * numerator / denominator
  )
}


# ------------------------------------------------------------
# 4. Create descriptive Table 1
#
# Continuous variables are presented as mean (SD).
# Categorical variables are presented as n (%).
# ------------------------------------------------------------

table1 <- data.frame(
  
  Characteristic = c(
    "Age, years",
    "Female sex",
    "BMI, kg/m²",
    "Waist circumference, cm",
    "Current smoker",
    "Physical activity",
    "Sleep duration, hours",
    "Short sleep (<6 h)",
    "Hypertension",
    "Biomarker",
    "Health outcome"
  ),
  
  Overall = c(
    mean_sd(data$age),
    n_percent(data$sex, "Female"),
    mean_sd(data$bmi),
    mean_sd(data$waist_cm),
    n_percent(data$smoking, "Current smoker"),
    mean_sd(data$physical_activity),
    mean_sd(data$sleep_duration),
    n_percent(data$short_sleep, 1),
    n_percent(data$hypertension, "Yes"),
    mean_sd(data$biomarker),
    n_percent(data$health_outcome, "Yes")
  ),
  
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------
# 5. Label sample size
# ------------------------------------------------------------

names(table1)[2] <- paste0(
  "Overall (N = ",
  nrow(data),
  ")"
)


# ------------------------------------------------------------
# 6. Display Table 1
# ------------------------------------------------------------

print(
  table1,
  row.names = FALSE
)


# ------------------------------------------------------------
# 7. Check binary outcome prevalence separately
# ------------------------------------------------------------

outcome_prevalence <- mean(
  data$health_outcome == "Yes",
  na.rm = TRUE
)

cat(
  "\nHealth outcome prevalence:",
  round(outcome_prevalence * 100, 1),
  "%\n"
)


# ------------------------------------------------------------
# 8. Create output directory if needed
# ------------------------------------------------------------

if (!dir.exists("output")) {
  dir.create("output")
}


# ------------------------------------------------------------
# 9. Save Table 1
# ------------------------------------------------------------

write.csv(
  table1,
  file = "output/table1_descriptive.csv",
  row.names = FALSE
)


cat(
  "\nDescriptive Table 1 successfully saved to:\n",
  "output/table1_descriptive.csv\n"
)