# Epidemiology Analysis Demo

This repository demonstrates a reproducible epidemiologic analysis workflow using synthetic data in R.

The workflow is designed to reflect common tasks in clinical and population-based epidemiology, including data cleaning, descriptive analysis, regression modeling, effect-modification analysis, restricted cubic splines, and sensitivity analyses.

## Methods

- Descriptive statistics
- Multivariable linear regression
- Logistic regression
- Modified Poisson regression with robust standard errors
- Interaction and effect-modification analysis
- Restricted cubic splines
- Subgroup and sensitivity analyses
- Publication-style tables and figures

## Repository Structure

- `data/` — synthetic datasets used in the demonstration
- `scripts/` — R scripts for data processing and statistical analysis
- `output/` — generated tables and figures

## Data

All data in this repository are synthetic and contain no real participant or patient information.

## Software

R

## Quick Start

Run the complete analysis workflow from the project root:

```r
source("run_all.R")

## Workflow

1. Generate a synthetic epidemiologic dataset
2. Produce descriptive statistics and Table 1
3. Fit multivariable linear, logistic, and modified Poisson regression models
4. Evaluate effect modification and nonlinear associations
5. Conduct subgroup and sensitivity analyses
6. Generate publication-style figures
