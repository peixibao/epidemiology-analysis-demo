# Shared I/O, model preparation, and inference for the synthetic workflow.

read_analysis_data <- function(path = "data/synthetic_epidemiology_data.csv") {
  if (!file.exists(path)) stop("Dataset not found: ", path, call. = FALSE)
  dat <- read.csv(path, stringsAsFactors = FALSE)
  needed <- c("id", "age", "sex", "bmi", "waist_cm", "smoking",
              "physical_activity", "sleep_duration", "short_sleep",
              "hypertension", "biomarker", "health_outcome")
  stopifnot(all(needed %in% names(dat)), !anyDuplicated(dat$id))
  stopifnot(all(dat$health_outcome %in% c("No", "Yes")),
            all(dat$short_sleep %in% c(0, 1)))
  dat$sex <- factor(dat$sex, levels = c("Female", "Male"))
  dat$smoking <- factor(dat$smoking,
                        levels = c("Non-smoker", "Current smoker"))
  dat$hypertension <- factor(dat$hypertension, levels = c("No", "Yes"))
  dat$health_outcome_binary <- as.integer(dat$health_outcome == "Yes")
  dat
}

save_csv <- function(x, path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  write.csv(x, path, row.names = FALSE)
  invisible(path)
}

coefficient_table <- function(fit, transform = c("linear", "odds"),
                              include_se = FALSE) {
  transform <- match.arg(transform)
  cf <- summary(fit)$coefficients
  beta <- unname(cf[, "Estimate"])
  se <- unname(cf[, "Std. Error"])
  result <- data.frame(Term = rownames(cf), row.names = NULL)
  if (transform == "linear") {
    result$Beta <- beta
    result$SE <- se
    result$P_value <- unname(cf[, "Pr(>|t|)"])
    result$Lower_95CI <- beta - 1.96 * se
    result$Upper_95CI <- beta + 1.96 * se
  } else {
    if (include_se) {
      result$Estimate <- beta
      result$SE <- se
    }
    result$OR <- exp(beta)
    result$Lower_95CI <- exp(beta - 1.96 * se)
    result$Upper_95CI <- exp(beta + 1.96 * se)
    result$P_value <- unname(cf[, "Pr(>|z|)"])
  }
  result
}

modified_poisson <- function(formula, dat) {
  fit <- glm(formula, data = dat, family = poisson(link = "log"))
  if (!isTRUE(fit$converged)) stop("Modified Poisson model did not converge")
  beta <- coef(fit)
  robust_se <- sqrt(diag(sandwich::vcovHC(fit, type = "HC0")))
  stopifnot(all(is.finite(beta)), all(is.finite(robust_se)),
            all(robust_se > 0))
  data.frame(
    Term = names(beta), RR = unname(exp(beta)),
    Lower_95CI = unname(exp(beta - 1.96 * robust_se)),
    Upper_95CI = unname(exp(beta + 1.96 * robust_se)),
    P_value = unname(2 * pnorm(abs(beta / robust_se), lower.tail = FALSE)),
    row.names = NULL
  )
}
