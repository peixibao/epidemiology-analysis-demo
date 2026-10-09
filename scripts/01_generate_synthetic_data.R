# Seed-controlled, entirely synthetic adult epidemiology dataset.
set.seed(2026)
n <- 800L
clamp <- function(x, lo, hi) pmin(pmax(x, lo), hi)

id <- seq_len(n)
age <- round(clamp(rnorm(n, mean = 48, sd = 12), 18, 75))
sex <- sample(c("Female", "Male"), size = n, replace = TRUE,
              prob = c(.52, .48))
bmi <- round(clamp(rnorm(n, mean = 24.5, sd = 4), 16, 40), 1)
waist_cm <- round(45 + 1.65 * bmi + .12 * age +
                    ifelse(sex == "Male", 5, 0) + rnorm(n, 0, 5), 1)

smoking_probability <- plogis(-2 + .025 * age +
                                ifelse(sex == "Male", 1.1, 0))
smoking <- ifelse(rbinom(n, 1, smoking_probability) == 1,
                  "Current smoker", "Non-smoker")
physical_activity <- round(pmax(rnorm(
  n, mean = 6 - .04 * (age - 45) - .08 * (bmi - 24), sd = 2
), 0), 1)
sleep_duration <- round(clamp(rnorm(
  n, mean = 7.1 - .015 * (age - 45) - .10 * (bmi - 24), sd = .9
), 4, 10), 1)
short_sleep <- as.integer(sleep_duration < 6)

hypertension_probability <- plogis(
  -6 + .055 * age + .10 * bmi +
    .65 * (smoking == "Current smoker") + .35 * (sex == "Male")
)
hypertension <- ifelse(rbinom(n, 1, hypertension_probability) == 1,
                       "Yes", "No")
biomarker <- round(
  70 + .35 * age + 1.15 * bmi + 4 * (smoking == "Current smoker") +
    6 * (hypertension == "Yes") - 1.8 * sleep_duration -
    .6 * physical_activity + rnorm(n, 0, 8), 1
)

# The generating mechanism deliberately includes a BMI × short-sleep term.
linear_predictor <- -7 + .040 * age + .095 * bmi +
  .55 * (smoking == "Current smoker") +
  .75 * (hypertension == "Yes") - .10 * physical_activity +
  .45 * short_sleep + .045 * bmi * short_sleep
health_outcome <- ifelse(rbinom(n, 1, plogis(linear_predictor)) == 1,
                         "Yes", "No")

synthetic_data <- data.frame(
  id, age, sex, bmi, waist_cm, smoking, physical_activity,
  sleep_duration, short_sleep, hypertension, biomarker, health_outcome
)
stopifnot(nrow(synthetic_data) == 800L, ncol(synthetic_data) == 12L,
          !anyDuplicated(synthetic_data$id),
          all(synthetic_data$health_outcome %in% c("No", "Yes")),
          all(synthetic_data$short_sleep %in% c(0, 1)))
save_csv(synthetic_data, "data/synthetic_epidemiology_data.csv")
message(sprintf("Synthetic cohort generated: N=%d, variables=%d.",
                nrow(synthetic_data), ncol(synthetic_data)))
