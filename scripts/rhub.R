rhub_output <- devtools::check_rhub(
  env_vars = c("_R_CHECK_FORCE_SUGGESTS_" = "0"),
  interactive = FALSE
)
rhub_output$web()
