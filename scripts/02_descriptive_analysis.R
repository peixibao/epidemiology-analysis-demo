# Descriptive Table 1 for the synthetic sample.
dat <- read_analysis_data()
mean_sd <- function(x) sprintf("%.1f (%.1f)", mean(x, na.rm = TRUE),
                               sd(x, na.rm = TRUE))
n_percent <- function(x, value) {
  n <- sum(!is.na(x))
  sprintf("%d (%.1f%%)", sum(x == value, na.rm = TRUE),
          100 * mean(x == value, na.rm = TRUE))
}
table1 <- data.frame(
  Characteristic = c(
    "Age, years", "Female sex", "BMI, kg/m²",
    "Waist circumference, cm", "Current smoker", "Physical activity",
    "Sleep duration, hours", "Short sleep (<6 h)",
    "Hypertension", "Biomarker", "Health outcome"
  ),
  Overall = c(
    mean_sd(dat$age), n_percent(dat$sex, "Female"),
    mean_sd(dat$bmi), mean_sd(dat$waist_cm),
    n_percent(dat$smoking, "Current smoker"),
    mean_sd(dat$physical_activity), mean_sd(dat$sleep_duration),
    n_percent(dat$short_sleep, 1), n_percent(dat$hypertension, "Yes"),
    mean_sd(dat$biomarker), n_percent(dat$health_outcome, "Yes")
  ), stringsAsFactors = FALSE
)
names(table1)[2] <- sprintf("Overall (N = %d)", nrow(dat))
save_csv(table1, "output/table1_descriptive.csv")
message(sprintf("Table 1 complete: health outcome prevalence %.1f%%.",
                100 * mean(dat$health_outcome == "Yes")))
