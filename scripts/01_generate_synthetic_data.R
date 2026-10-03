# ============================================================
# 01_generate_synthetic_data.R
#
# Purpose:
# Generate a synthetic dataset for demonstrating a
# reproducible epidemiologic analysis workflow.
#
# IMPORTANT:
# This dataset is entirely synthetic.
# It contains no real participant or patient information.
# ============================================================


# ------------------------------------------------------------
# 1. Make results reproducible
# ------------------------------------------------------------

set.seed(2026)


# ------------------------------------------------------------
# 2. Define sample size
# ------------------------------------------------------------

n <- 800


# ------------------------------------------------------------
# 3. Generate basic participant characteristics
# ------------------------------------------------------------

id <- 1:n

age <- round(
  pmin(
    pmax(rnorm(n, mean = 48, sd = 12), 18),
    75
  )
)

sex <- sample(
  c("Female", "Male"),
  size = n,
  replace = TRUE,
  prob = c(0.52, 0.48)
)

bmi <- round(
  pmin(
    pmax(rnorm(n, mean = 24.5, sd = 4.0), 16),
    40
  ),
  1
)

waist_cm <- round(
  45 +
    1.65 * bmi +
    0.12 * age +
    ifelse(sex == "Male", 5, 0) +
    rnorm(n, mean = 0, sd = 5),
  1
)


# ------------------------------------------------------------
# 4. Generate lifestyle variables
# ------------------------------------------------------------

smoking_probability <- plogis(
  -2.0 +
    0.025 * age +
    ifelse(sex == "Male", 1.1, 0)
)

smoking <- ifelse(
  rbinom(n, size = 1, prob = smoking_probability) == 1,
  "Current smoker",
  "Non-smoker"
)

physical_activity <- round(
  pmax(
    rnorm(
      n,
      mean = 6.0 -
        0.04 * (age - 45) -
        0.08 * (bmi - 24),
      sd = 2.0
    ),
    0
  ),
  1
)

sleep_duration <- round(
  pmin(
    pmax(
      rnorm(
        n,
        mean = 7.1 -
          0.015 * (age - 45) -
          0.10 * (bmi - 24),
        sd = 0.9
      ),
      4
    ),
    10
  ),
  1
)

short_sleep <- ifelse(
  sleep_duration < 6,
  1,
  0
)


# ------------------------------------------------------------
# 5. Generate hypertension
# ------------------------------------------------------------

hypertension_probability <- plogis(
  -6.0 +
    0.055 * age +
    0.10 * bmi +
    0.65 * (smoking == "Current smoker") +
    0.35 * (sex == "Male")
)

hypertension <- ifelse(
  rbinom(n, size = 1, prob = hypertension_probability) == 1,
  "Yes",
  "No"
)


# ------------------------------------------------------------
# 6. Generate a continuous biomarker
#
# This variable is constructed so that older age,
# higher BMI, smoking, hypertension, and shorter sleep
# are associated with higher biomarker values.
# ------------------------------------------------------------

biomarker <- round(
  70 +
    0.35 * age +
    1.15 * bmi +
    4.0 * (smoking == "Current smoker") +
    6.0 * (hypertension == "Yes") -
    1.8 * sleep_duration -
    0.6 * physical_activity +
    rnorm(n, mean = 0, sd = 8),
  1
)


# ------------------------------------------------------------
# 7. Generate a binary health outcome
#
# The model includes:
# - age
# - BMI
# - smoking
# - hypertension
# - physical activity
# - short sleep
# - BMI × short-sleep interaction
#
# This gives us useful structure for later demonstrations
# of logistic regression and effect modification.
# ------------------------------------------------------------

linear_predictor <- (
  -7.0 +
    0.040 * age +
    0.095 * bmi +
    0.55 * (smoking == "Current smoker") +
    0.75 * (hypertension == "Yes") -
    0.10 * physical_activity +
    0.45 * short_sleep +
    0.045 * bmi * short_sleep
)

outcome_probability <- plogis(linear_predictor)

health_outcome <- ifelse(
  rbinom(n, size = 1, prob = outcome_probability) == 1,
  "Yes",
  "No"
)


# ------------------------------------------------------------
# 8. Combine variables into one dataset
# ------------------------------------------------------------

synthetic_data <- data.frame(
  id = id,
  age = age,
  sex = sex,
  bmi = bmi,
  waist_cm = waist_cm,
  smoking = smoking,
  physical_activity = physical_activity,
  sleep_duration = sleep_duration,
  short_sleep = short_sleep,
  hypertension = hypertension,
  biomarker = biomarker,
  health_outcome = health_outcome
)


# ------------------------------------------------------------
# 9. Inspect the generated dataset
# ------------------------------------------------------------

head(synthetic_data)

summary(synthetic_data)


# ------------------------------------------------------------
# 10. Create the data folder if it does not exist
# ------------------------------------------------------------

if (!dir.exists("data")) {
  dir.create("data")
}


# ------------------------------------------------------------
# 11. Save synthetic dataset
# ------------------------------------------------------------

write.csv(
  synthetic_data,
  file = "data/synthetic_epidemiology_data.csv",
  row.names = FALSE
)


# ------------------------------------------------------------
# 12. Confirmation message
# ------------------------------------------------------------

cat(
  "\nSynthetic dataset successfully generated and saved to:\n",
  "data/synthetic_epidemiology_data.csv\n"
)
