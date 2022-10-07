# `devtools::spell_check()`

# only check spelling if on CI and spelling is available
if (requireNamespace("spelling", quietly = TRUE)) {
  if (isTRUE(as.logical(Sys.getenv("CI")))) {
    spelling::spell_check_test(vignettes = TRUE, error = FALSE,
                              skip_on_cran = TRUE)
  }
}
