# ============================================================
# 03_regression_models.R
#
# Purpose:
# Demonstrate multivariable regression models using the
# synthetic epidemiology dataset.
#
# Models included:
# 1. Multivariable linear regression
# 2. Multivariable logistic regression
# 3. Modified Poisson regression with robust standard errors
# ============================================================


# ------------------------------------------------------------
# 1. Load data
# ------------------------------------------------------------

data <- read.csv(
  "data/synthetic_epidemiology_data.csv",
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------
# 2. Prepare variables
# ------------------------------------------------------------

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


# ------------------------------------------------------------
# 3. Multivariable linear regression
#
# Outcome:
# biomarker (continuous)
#
# Interpretation:
# Each beta coefficient represents the adjusted mean
# difference in biomarker associated with the predictor.
# ------------------------------------------------------------

linear_model <- lm(
  biomarker ~
    age +
    sex +
    bmi +
    smoking +
    physical_activity +
    sleep_duration +
    hypertension,
  data = data
)

summary(linear_model)


# ------------------------------------------------------------
# 4. Extract linear regression results
# ------------------------------------------------------------

linear_coef <- summary(linear_model)$coefficients

linear_results <- data.frame(
  Term = rownames(linear_coef),
  Beta = linear_coef[, "Estimate"],
  SE = linear_coef[, "Std. Error"],
  P_value = linear_coef[, "Pr(>|t|)"],
  row.names = NULL
)

linear_results$Lower_95CI <-
  linear_results$Beta - 1.96 * linear_results$SE

linear_results$Upper_95CI <-
  linear_results$Beta + 1.96 * linear_results$SE


# ------------------------------------------------------------
# 5. Multivariable logistic regression
#
# Outcome:
# health_outcome_binary
#
# Interpretation:
# Exponentiated coefficients are odds ratios (ORs).
# ------------------------------------------------------------

logistic_model <- glm(
  health_outcome_binary ~
    age +
    sex +
    bmi +
    smoking +
    physical_activity +
    short_sleep +
    hypertension,
  family = binomial(link = "logit"),
  data = data
)

summary(logistic_model)


# ------------------------------------------------------------
# 6. Extract logistic regression results
# ------------------------------------------------------------

logistic_coef <- summary(logistic_model)$coefficients

logistic_results <- data.frame(
  Term = rownames(logistic_coef),
  OR = exp(logistic_coef[, "Estimate"]),
  Lower_95CI = exp(
    logistic_coef[, "Estimate"] -
      1.96 * logistic_coef[, "Std. Error"]
  ),
  Upper_95CI = exp(
    logistic_coef[, "Estimate"] +
      1.96 * logistic_coef[, "Std. Error"]
  ),
  P_value = logistic_coef[, "Pr(>|z|)"],
  row.names = NULL
)


# ------------------------------------------------------------
# 7. Install/load sandwich package
#
# sandwich is used to calculate robust standard errors
# for modified Poisson regression.
# ------------------------------------------------------------

if (!requireNamespace("sandwich", quietly = TRUE)) {
  install.packages("sandwich")
}


# ------------------------------------------------------------
# 8. Modified Poisson regression
#
# Outcome:
# health_outcome_binary
#
# Interpretation:
# Exponentiated coefficients are prevalence ratios /
# risk ratios rather than odds ratios.
#
# Robust standard errors are used because standard
# Poisson variance assumptions do not hold for binary data.
# ------------------------------------------------------------

poisson_model <- glm(
  health_outcome_binary ~
    age +
    sex +
    bmi +
    smoking +
    physical_activity +
    short_sleep +
    hypertension,
  family = poisson(link = "log"),
  data = data
)


# ------------------------------------------------------------
# 9. Robust variance estimation
# ------------------------------------------------------------

robust_vcov <- sandwich::vcovHC(
  poisson_model,
  type = "HC0"
)

robust_se <- sqrt(
  diag(robust_vcov)
)

poisson_beta <- coef(poisson_model)

poisson_z <- poisson_beta / robust_se

poisson_p <- 2 * pnorm(
  abs(poisson_z),
  lower.tail = FALSE
)


# ------------------------------------------------------------
# 10. Extract modified Poisson results
# ------------------------------------------------------------

poisson_results <- data.frame(
  Term = names(poisson_beta),
  
  RR = exp(poisson_beta),
  
  Lower_95CI = exp(
    poisson_beta -
      1.96 * robust_se
  ),
  
  Upper_95CI = exp(
    poisson_beta +
      1.96 * robust_se
  ),
  
  P_value = poisson_p,
  
  row.names = NULL
)


# ------------------------------------------------------------
# 11. Display results
# ------------------------------------------------------------

cat("\n--- LINEAR REGRESSION ---\n")
print(linear_results)

cat("\n--- LOGISTIC REGRESSION ---\n")
print(logistic_results)

cat("\n--- MODIFIED POISSON REGRESSION ---\n")
print(poisson_results)


# ------------------------------------------------------------
# 12. Create output folder if needed
# ------------------------------------------------------------

if (!dir.exists("output")) {
  dir.create("output")
}


# ------------------------------------------------------------
# 13. Save regression tables
# ------------------------------------------------------------

write.csv(
  linear_results,
  "output/linear_regression_results.csv",
  row.names = FALSE
)

write.csv(
  logistic_results,
  "output/logistic_regression_results.csv",
  row.names = FALSE
)

write.csv(
  poisson_results,
  "output/modified_poisson_results.csv",
  row.names = FALSE
)


cat(
  "\nRegression results successfully saved to output/.\n"
)