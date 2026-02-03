#!/usr/bin/env Rscript

# This script works in tandem with
# rstudio/shiny-workflows/.github/workflows/R-CMD-check.yaml@v1 GHA job

# Remove tests/testthat/_snaps from .Rbuildignore
# This ensures snapshot tests are included in R CMD check but not in the package build for a CRAN submission

rbuildignore_path <- ".Rbuildignore"

if (!file.exists(rbuildignore_path)) {
  stop(".Rbuildignore file not found")
}

# Read the file
lines <- readLines(rbuildignore_path)

# Remove the line containing "tests/testthat/_snaps"
lines_filtered <- lines[!grepl("^tests/testthat/_snaps\\s*$", lines)]

# Write back only if changes were made
if (length(lines) != length(lines_filtered)) {
  writeLines(lines_filtered, rbuildignore_path)
  message("Removed 'tests/testthat/_snaps' from .Rbuildignore")
} else {
  message("'tests/testthat/_snaps' not found in .Rbuildignore")
}
