# ============================================================
# 04_interaction_rcs.R
#
# Purpose:
# Demonstrate interaction analysis and restricted cubic
# spline modeling using synthetic epidemiologic data.
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


# ------------------------------------------------------------
# 2. Interaction analysis
#
# Question:
# Does the association between BMI and the health outcome
# differ according to short-sleep status?
# ------------------------------------------------------------

interaction_model <- glm(
  health_outcome_binary ~
    age +
    sex +
    bmi * short_sleep +
    smoking +
    physical_activity +
    hypertension,
  family = binomial(link = "logit"),
  data = data
)

summary(interaction_model)


# ------------------------------------------------------------
# 3. Compare model with and without interaction
# ------------------------------------------------------------

no_interaction_model <- glm(
  health_outcome_binary ~
    age +
    sex +
    bmi +
    short_sleep +
    smoking +
    physical_activity +
    hypertension,
  family = binomial(link = "logit"),
  data = data
)

interaction_test <- anova(
  no_interaction_model,
  interaction_model,
  test = "LRT"
)

print(interaction_test)


# ------------------------------------------------------------
# 4. Extract interaction coefficient
# ------------------------------------------------------------

interaction_coef <- summary(
  interaction_model
)$coefficients

interaction_results <- data.frame(
  Term = rownames(interaction_coef),
  Estimate = interaction_coef[, "Estimate"],
  SE = interaction_coef[, "Std. Error"],
  OR = exp(interaction_coef[, "Estimate"]),
  Lower_95CI = exp(
    interaction_coef[, "Estimate"] -
      1.96 * interaction_coef[, "Std. Error"]
  ),
  Upper_95CI = exp(
    interaction_coef[, "Estimate"] +
      1.96 * interaction_coef[, "Std. Error"]
  ),
  P_value = interaction_coef[, "Pr(>|z|)"],
  row.names = NULL
)


# ------------------------------------------------------------
# 5. Restricted / natural cubic spline model
#
# Natural cubic splines constrain the function to be linear
# beyond the boundary knots and are commonly used to model
# nonlinear exposure-outcome relationships.
# ------------------------------------------------------------

spline_model <- glm(
  health_outcome_binary ~
    splines::ns(bmi, df = 4) +
    age +
    sex +
    smoking +
    physical_activity +
    short_sleep +
    hypertension,
  family = binomial(link = "logit"),
  data = data
)

linear_bmi_model <- glm(
  health_outcome_binary ~
    bmi +
    age +
    sex +
    smoking +
    physical_activity +
    short_sleep +
    hypertension,
  family = binomial(link = "logit"),
  data = data
)


# ------------------------------------------------------------
# 6. Test for nonlinearity
# ------------------------------------------------------------

nonlinearity_test <- anova(
  linear_bmi_model,
  spline_model,
  test = "LRT"
)

print(nonlinearity_test)


# ------------------------------------------------------------
# 7. Generate spline predictions
# ------------------------------------------------------------

bmi_grid <- seq(
  quantile(data$bmi, 0.02, na.rm = TRUE),
  quantile(data$bmi, 0.98, na.rm = TRUE),
  length.out = 200
)

prediction_data <- data.frame(
  bmi = bmi_grid,
  age = mean(data$age, na.rm = TRUE),
  sex = factor(
    "Female",
    levels = levels(data$sex)
  ),
  smoking = factor(
    "Non-smoker",
    levels = levels(data$smoking)
  ),
  physical_activity = mean(
    data$physical_activity,
    na.rm = TRUE
  ),
  short_sleep = 0,
  hypertension = factor(
    "No",
    levels = levels(data$hypertension)
  )
)

pred <- predict(
  spline_model,
  newdata = prediction_data,
  type = "link",
  se.fit = TRUE
)

prediction_data$Predicted_probability <-
  plogis(pred$fit)

prediction_data$Lower_95CI <-
  plogis(pred$fit - 1.96 * pred$se.fit)

prediction_data$Upper_95CI <-
  plogis(pred$fit + 1.96 * pred$se.fit)


# ------------------------------------------------------------
# 8. Save outputs
# ------------------------------------------------------------

if (!dir.exists("output")) {
  dir.create("output")
}

write.csv(
  interaction_results,
  "output/interaction_results.csv",
  row.names = FALSE
)

write.csv(
  prediction_data,
  "output/bmi_spline_predictions.csv",
  row.names = FALSE
)

capture.output(
  interaction_test,
  file = "output/interaction_lrt.txt"
)

capture.output(
  nonlinearity_test,
  file = "output/spline_nonlinearity_lrt.txt"
)

cat(
  "\nInteraction and spline analyses completed successfully.\n"
)