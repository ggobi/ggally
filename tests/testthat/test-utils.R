
context("utils")
test_that("require_namespaces", {

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
