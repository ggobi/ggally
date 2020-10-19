
context("utils")
test_that("require_namespaces", {

  if ("Hmisc" %in% loadedNamespaces()) unloadNamespace("Hmisc")
  #NB: survival is required by Hmisc, so Hmisc must be unloaded before
  if ("survival" %in% loadedNamespaces()) unloadNamespace("survival")

  expect_false("package:survival" %in% search())

  suppressMessages(require_namespaces(c("survival")))

  expect_false("package:survival" %in% search())

  expect_false(is.null(getNamespace("survival")))

  expect_error(
    suppressWarnings(suppressMessages(
      require_namespaces("DOES_NOT_EXIST_qweqweqweqwe")
    ))
  )
})
