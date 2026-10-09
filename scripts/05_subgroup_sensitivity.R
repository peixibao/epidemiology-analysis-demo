# Alternative sleep definition, BMI restriction, and sex-stratified models.
dat <- read_analysis_data()

primary_formula <- health_outcome_binary ~
  age + sex + bmi + smoking + physical_activity + short_sleep + hypertension
primary <- modified_poisson(primary_formula, dat)

dat$short_sleep_alt <- as.integer(dat$sleep_duration <= 6)
alternative_formula <- update(primary_formula, . ~ . - short_sleep + short_sleep_alt)
alternative <- modified_poisson(alternative_formula, dat)

restricted <- subset(dat, bmi >= 18.5 & bmi <= 35)
restricted_results <- modified_poisson(primary_formula, restricted)

subgroup_formula <- update(primary_formula, . ~ . - sex)
subgroup_results <- do.call(rbind, lapply(c("Female", "Male"), function(group) {
  fit <- modified_poisson(subgroup_formula, subset(dat, sex == group))
  row <- fit[fit$Term == "bmi",
             c("RR", "Lower_95CI", "Upper_95CI", "P_value"),
             drop = FALSE]
  data.frame(Subgroup = group, row, row.names = NULL)
}))

save_csv(primary, "output/sensitivity_primary_model.csv")
save_csv(alternative, "output/sensitivity_alternative_sleep_definition.csv")
save_csv(restricted_results, "output/sensitivity_restricted_bmi.csv")
save_csv(subgroup_results, "output/subgroup_bmi_by_sex.csv")

message("Subgroup and sensitivity analyses complete.")
