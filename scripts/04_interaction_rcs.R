# BMI × short-sleep interaction and natural cubic spline nonlinearity.
dat <- read_analysis_data()

interaction_formula <- health_outcome_binary ~
  age + sex + bmi * short_sleep + smoking + physical_activity + hypertension
main_effects_formula <- health_outcome_binary ~
  age + sex + bmi + short_sleep + smoking + physical_activity + hypertension

interaction_fit <- glm(interaction_formula, data = dat, family = binomial())
no_interaction_fit <- glm(main_effects_formula, data = dat, family = binomial())
interaction_test <- anova(no_interaction_fit, interaction_fit, test = "LRT")
interaction_results <- coefficient_table(
  interaction_fit, transform = "odds", include_se = TRUE
)

spline_formula <- health_outcome_binary ~
  splines::ns(bmi, df = 4) + age + sex + smoking +
  physical_activity + short_sleep + hypertension
spline_fit <- glm(spline_formula, data = dat, family = binomial())
linear_bmi_fit <- glm(main_effects_formula, data = dat, family = binomial())
nonlinearity_test <- anova(linear_bmi_fit, spline_fit, test = "LRT")

# Fixed reference profile: descriptive predictions, not population-standardized risks.
grid <- seq(quantile(dat$bmi, .02), quantile(dat$bmi, .98), length.out = 200)
prediction_data <- data.frame(
  bmi = grid,
  age = mean(dat$age),
  sex = factor("Female", levels = levels(dat$sex)),
  smoking = factor("Non-smoker", levels = levels(dat$smoking)),
  physical_activity = mean(dat$physical_activity),
  short_sleep = 0,
  hypertension = factor("No", levels = levels(dat$hypertension))
)
pred <- predict(spline_fit, newdata = prediction_data,
                type = "link", se.fit = TRUE)
prediction_data$Predicted_probability <- plogis(pred$fit)
prediction_data$Lower_95CI <- plogis(pred$fit - 1.96 * pred$se.fit)
prediction_data$Upper_95CI <- plogis(pred$fit + 1.96 * pred$se.fit)

save_csv(interaction_results, "output/interaction_results.csv")
save_csv(prediction_data, "output/bmi_spline_predictions.csv")
capture.output(interaction_test, file = "output/interaction_lrt.txt")
capture.output(nonlinearity_test, file = "output/spline_nonlinearity_lrt.txt")

message("Interaction, spline, and likelihood-ratio tests complete.")
