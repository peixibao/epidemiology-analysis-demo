# Fail early if required packages are unavailable; do not install during analysis.
required <- c("sandwich", "ggplot2")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop("Missing R packages: ", paste(missing, collapse = ", "),
       ". Run install.packages(c(", paste(sprintf('"%s"', missing), collapse = ", "),
       ")) first.", call. = FALSE)
}
message("R dependencies available.")
