
context("utils")
test_that("require_pkgs", {

  detach("package:survival")
  detach("package:scales")

  expect_false("package:survival" %in% search())
  expect_false("package:scales" %in% search())

  suppressMessages(require_pkgs(c("survival", "scales")))

  expect_true("package:survival" %in% search())
  expect_true("package:scales" %in% search())

  expect_error(suppressWarnings(suppressMessages(require_pkgs("DOES_NOT_EXIST_asdfasdfasfd"))))
})
