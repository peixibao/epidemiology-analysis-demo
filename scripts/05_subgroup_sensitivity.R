# ============================================================
# 05_subgroup_sensitivity.R
#
# Purpose:
# Demonstrate subgroup and sensitivity analyses.
# ============================================================


# ------------------------------------------------------------
# 1. Load data
# ------------------------------------------------------------

data <- read.csv(
  "data/synthetic_epidemiology_data.csv",
  stringsAsFactors = FALSE
)

data$sex <- factor(
  data$sex,
  levels = c("Female", "Male")
)

data$smoking <- factor(
  data$smoking,
  levels = c("Non-smoker", "Current smoker")
)

data$hypertension <- factor(
  data$hypertension,
  levels = c("No", "Yes")
)

data$health_outcome_binary <- ifelse(
  data$health_outcome == "Yes",
  1,
  0
)


if (!requireNamespace("sandwich", quietly = TRUE)) {
  install.packages("sandwich")
}


# ------------------------------------------------------------
# 2. Helper function for modified Poisson regression
# ------------------------------------------------------------

fit_modified_poisson <- function(formula, dataset) {
  
  model <- glm(
    formula,
    family = poisson(link = "log"),
    data = dataset
  )
  
  robust_vcov <- sandwich::vcovHC(
    model,
    type = "HC0"
  )
  
  robust_se <- sqrt(
    diag(robust_vcov)
  )
  
  beta <- coef(model)
  
  p_value <- 2 * pnorm(
    abs(beta / robust_se),
    lower.tail = FALSE
  )
  
  data.frame(
    Term = names(beta),
    RR = exp(beta),
    Lower_95CI = exp(beta - 1.96 * robust_se),
    Upper_95CI = exp(beta + 1.96 * robust_se),
    P_value = p_value,
    row.names = NULL
  )
}


# ------------------------------------------------------------
# 3. Primary model
# ------------------------------------------------------------

primary_results <- fit_modified_poisson(
  health_outcome_binary ~
    age +
    sex +
    bmi +
    smoking +
    physical_activity +
    short_sleep +
    hypertension,
  data
)


# ------------------------------------------------------------
# 4. Sensitivity analysis 1
#
# Alternative short-sleep definition:
# <= 6 hours rather than < 6 hours
# ------------------------------------------------------------

data$short_sleep_alt <- ifelse(
  data$sleep_duration <= 6,
  1,
  0
)

sensitivity_sleep <- fit_modified_poisson(
  health_outcome_binary ~
    age +
    sex +
    bmi +
    smoking +
    physical_activity +
    short_sleep_alt +
    hypertension,
  data
)


# ------------------------------------------------------------
# 5. Sensitivity analysis 2
#
# Restrict analysis to BMI 18.5–35 kg/m²
# ------------------------------------------------------------

restricted_data <- subset(
  data,
  bmi >= 18.5 & bmi <= 35
)

sensitivity_bmi <- fit_modified_poisson(
  health_outcome_binary ~
    age +
    sex +
    bmi +
    smoking +
    physical_activity +
    short_sleep +
    hypertension,
  restricted_data
)


# ------------------------------------------------------------
# 6. Sex-stratified subgroup analyses
# ------------------------------------------------------------

female_data <- subset(
  data,
  sex == "Female"
)

male_data <- subset(
  data,
  sex == "Male"
)

female_results <- fit_modified_poisson(
  health_outcome_binary ~
    age +
    bmi +
    smoking +
    physical_activity +
    short_sleep +
    hypertension,
  female_data
)

male_results <- fit_modified_poisson(
  health_outcome_binary ~
    age +
    bmi +
    smoking +
    physical_activity +
    short_sleep +
    hypertension,
  male_data
)


# ------------------------------------------------------------
# 7. Extract BMI estimates for subgroup comparison
# ------------------------------------------------------------

female_bmi <- subset(
  female_results,
  Term == "bmi"
)

male_bmi <- subset(
  male_results,
  Term == "bmi"
)

subgroup_bmi <- rbind(
  data.frame(
    Subgroup = "Female",
    female_bmi[, c(
      "RR",
      "Lower_95CI",
      "Upper_95CI",
      "P_value"
    )]
  ),
  
  data.frame(
    Subgroup = "Male",
    male_bmi[, c(
      "RR",
      "Lower_95CI",
      "Upper_95CI",
      "P_value"
    )]
  )
)


# ------------------------------------------------------------
# 8. Save results
# ------------------------------------------------------------

if (!dir.exists("output")) {
  dir.create("output")
}

write.csv(
  primary_results,
  "output/sensitivity_primary_model.csv",
  row.names = FALSE
)

write.csv(
  sensitivity_sleep,
  "output/sensitivity_alternative_sleep_definition.csv",
  row.names = FALSE
)

write.csv(
  sensitivity_bmi,
  "output/sensitivity_restricted_bmi.csv",
  row.names = FALSE
)

write.csv(
  subgroup_bmi,
  "output/subgroup_bmi_by_sex.csv",
  row.names = FALSE
)

cat(
  "\nSubgroup and sensitivity analyses completed successfully.\n"
)