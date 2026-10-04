# ============================================================
# 06_figures.R
#
# Purpose:
# Generate publication-style figures from model outputs.
# ============================================================


# ------------------------------------------------------------
# 1. Install/load ggplot2
# ------------------------------------------------------------

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

library(ggplot2)


# ------------------------------------------------------------
# 2. Forest plot for modified Poisson model
# ------------------------------------------------------------

poisson_results <- read.csv(
  "output/modified_poisson_results.csv"
)

forest_data <- subset(
  poisson_results,
  Term != "(Intercept)"
)

forest_data$Term <- factor(
  forest_data$Term,
  levels = rev(forest_data$Term)
)

forest_plot <- ggplot(
  forest_data,
  aes(
    x = RR,
    y = Term
  )
) +
  geom_point(
    size = 2.5
  ) +
  geom_errorbarh(
    aes(
      xmin = Lower_95CI,
      xmax = Upper_95CI
    ),
    height = 0.2
  ) +
  geom_vline(
    xintercept = 1,
    linetype = "dashed"
  ) +
  labs(
    x = "Adjusted risk ratio (95% CI)",
    y = NULL,
    title = "Multivariable Modified Poisson Regression"
  ) +
  theme_minimal(
    base_size = 12
  )

print(forest_plot)

ggsave(
  filename = "output/figure_forest_plot.png",
  plot = forest_plot,
  width = 7,
  height = 5,
  dpi = 300
)


# ------------------------------------------------------------
# 3. BMI spline figure
# ------------------------------------------------------------

spline_data <- read.csv(
  "output/bmi_spline_predictions.csv"
)

spline_plot <- ggplot(
  spline_data,
  aes(
    x = bmi,
    y = Predicted_probability
  )
) +
  geom_ribbon(
    aes(
      ymin = Lower_95CI,
      ymax = Upper_95CI
    ),
    alpha = 0.2
  ) +
  geom_line(
    linewidth = 1
  ) +
  labs(
    x = "BMI (kg/m²)",
    y = "Predicted probability",
    title = "BMI and Predicted Probability of the Health Outcome"
  ) +
  theme_minimal(
    base_size = 12
  )

print(spline_plot)

ggsave(
  filename = "output/figure_bmi_spline.png",
  plot = spline_plot,
  width = 7,
  height = 5,
  dpi = 300
)


cat(
  "\nFigures successfully saved to output/.\n"
)