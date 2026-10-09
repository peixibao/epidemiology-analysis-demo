# Reproducible plots from the saved model outputs.
poisson <- read.csv("output/modified_poisson_results.csv")
spline <- read.csv("output/bmi_spline_predictions.csv")
forest <- subset(poisson, Term != "(Intercept)")
forest$Term <- factor(forest$Term, levels = rev(forest$Term))

p_forest <- ggplot2::ggplot(forest, ggplot2::aes(x = RR, y = Term)) +
  ggplot2::geom_point(size = 2.5) +
  ggplot2::geom_errorbarh(
    ggplot2::aes(xmin = Lower_95CI, xmax = Upper_95CI), height = .2
  ) +
  ggplot2::geom_vline(xintercept = 1, linetype = "dashed") +
  ggplot2::labs(
    x = "Adjusted prevalence ratio (95% CI)", y = NULL,
    title = "Multivariable Modified Poisson Regression"
  ) +
  ggplot2::theme_minimal(base_size = 12)

p_spline <- ggplot2::ggplot(
  spline, ggplot2::aes(x = bmi, y = Predicted_probability)
) +
  ggplot2::geom_ribbon(ggplot2::aes(
    ymin = Lower_95CI, ymax = Upper_95CI
  ), alpha = .2) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::labs(
    x = "BMI (kg/m²)", y = "Predicted probability",
    title = "BMI and Predicted Probability of the Health Outcome"
  ) +
  ggplot2::theme_minimal(base_size = 12)

ggplot2::ggsave("output/figure_forest_plot.png", p_forest,
                width = 7, height = 5, dpi = 300)
ggplot2::ggsave("output/figure_bmi_spline.png", p_spline,
                width = 7, height = 5, dpi = 300)
message("Forest plot and spline plot saved.")
