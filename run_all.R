# Reproduce the complete synthetic epidemiology analysis.
source_path <- sys.frame(1)$ofile
if (is.null(source_path) || !nzchar(source_path)) {
  stop("Start the workflow with source('run_all.R').", call. = FALSE)
}
project_root <- dirname(normalizePath(source_path, mustWork = TRUE))

run_pipeline <- function(root) {
  previous_wd <- setwd(root)
  on.exit(setwd(previous_wd), add = TRUE)

  scripts <- c(
    "scripts/00_check_packages.R",
    "scripts/00_analysis_helpers.R",
    "scripts/01_generate_synthetic_data.R",
    "scripts/02_descriptive_analysis.R",
    "scripts/03_regression_models.R",
    "scripts/04_interaction_rcs.R",
    "scripts/05_subgroup_sensitivity.R",
    "scripts/06_figures.R"
  )
  absent <- scripts[!file.exists(scripts)]
  if (length(absent)) {
    stop("Missing workflow files: ", paste(absent, collapse = ", "), call. = FALSE)
  }
  dir.create("data", showWarnings = FALSE, recursive = TRUE)
  dir.create("output", showWarnings = FALSE, recursive = TRUE)

  for (script in scripts) {
    message("\n--- ", script, " ---")
    source(script, local = TRUE, echo = FALSE)
  }

  writeLines(capture.output(sessionInfo()), "output/session_info.txt")
  message("\nComplete epidemiology workflow finished successfully.")
  invisible(TRUE)
}

run_pipeline(project_root)
rm(run_pipeline, project_root, source_path)
