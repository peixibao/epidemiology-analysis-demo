# Primary multivariable models for the synthetic cross-sectional sample.
dat <- read_analysis_data()

adjustment <- c("age", "sex", "bmi", "smoking", "physical_activity",
                "short_sleep", "hypertension")
linear_formula <- reformulate(
  c("age", "sex", "bmi", "smoking", "physical_activity",
    "sleep_duration", "hypertension"), response = "biomarker"
)
binary_formula <- reformulate(adjustment, response = "health_outcome_binary")

linear_fit <- lm(linear_formula, data = dat)
logistic_fit <- glm(binary_formula, family = binomial(link = "logit"), data = dat)

linear_results <- coefficient_table(linear_fit, transform = "linear")
logistic_results <- coefficient_table(logistic_fit, transform = "odds")
poisson_results <- modified_poisson(binary_formula, dat)

save_csv(linear_results, "output/linear_regression_results.csv")
save_csv(logistic_results, "output/logistic_regression_results.csv")
save_csv(poisson_results, "output/modified_poisson_results.csv")

message("Linear, logistic, and robust modified Poisson models complete.")
