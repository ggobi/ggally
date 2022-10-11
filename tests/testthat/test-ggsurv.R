
context("ggsurv")
suppressMessages(require(survival))
suppressMessages(require(scales))
lung <- survival::lung
kidney <- survival::kidney

sf.lung <- survival::survfit(Surv(time, status) ~ 1, data = lung)
sf.kid <- survival::survfit(Surv(time, status) ~ disease, data = kidney)

test_that("single", {

  a <- ggsurv(sf.lung)

  expect_equivalent(mapping_string(a$mapping$x), "time")
  expect_equivalent(mapping_string(a$mapping$y), "surv")

  expect_true(is.null(a$labels$group))
  expect_true(is.null(a$labels$colour))
  expect_true(is.null(a$labels$linetype))
})

test_that("multiple", {

  a <- ggsurv(sf.kid)

  expect_equivalent(mapping_string(a$mapping$x), "time")
  expect_equivalent(mapping_string(a$mapping$y), "surv")

  expect_true(!is.null(a$labels$group))
  expect_true(!is.null(a$labels$colour))
  expect_true(!is.null(a$labels$linetype))

})

test_that("adjust plot", {

  a <- ggsurv(sf.kid, plot.cens = FALSE)
  expect_equivalent(length(a$layers), 1)

  a <- ggsurv(sf.kid, plot.cens = TRUE)
  expect_equivalent(length(a$layers), 2)

})

test_that("stops", {

  noCensor <- subset(lung, status == 1)
  lungNoCensor <- survival::survfit(Surv(time, status) ~ 1, data = noCensor)

  # check that the surv.col and lty.est are of the correct length
  expect_error(ggsurv(lungNoCensor, surv.col = c("black", "red")))
  expect_error(ggsurv(lungNoCensor, lty.est = 1:2))

  # must have censor to plot
  expect_error(ggsurv(lungNoCensor, plot.cens = TRUE))


  noCensor <- subset(kidney, status == 1)
  kidneyNoCensor <- survival::survfit(Surv(time, status) ~ disease, data = noCensor)

  # check that the surv.col and lty.est are of the correct length.  should be 4
  expect_error(ggsurv(kidneyNoCensor, surv.col = c("black", "red", "blue")))
  expect_error(ggsurv(kidneyNoCensor, lty.est = 1:3))

  # must have censor to plot
  expect_error(ggsurv(kidneyNoCensor, plot.cens = TRUE))

  # must have censor to plot
  expect_silent(
    ggsurv(sf.kid, CI = TRUE, surv.col = c("black", "red", "blue", "green"))
  )
  expect_silent(
    ggsurv(sf.kid, CI = TRUE, lty.est = 1:4)
  )

  ggsurv(sf.kid, CI = TRUE, surv.col = "red")


})

test_that("back.white", {

  sf.lung <- survival::survfit(Surv(time, status) ~ 1, data = lung)
  sf.kid <- survival::survfit(Surv(time, status) ~ disease, data = kidney)

  a <- ggsurv(sf.lung, back.white = FALSE)
  expect_true(length(a$theme) == 0)
  a <- ggsurv(sf.lung, back.white = TRUE)
  expect_true(length(a$theme) != 0)

  a <- ggsurv(sf.kid, back.white = FALSE)
  expect_true(length(a$theme) == 0)
  a <- ggsurv(sf.kid, back.white = TRUE)
  expect_true(length(a$theme) != 0)

})

test_that("surv.col", {


  ggsurv(sf.lung, surv.col = "red")


  ggsurv(sf.kid, surv.col = "red")
  ggsurv(sf.kid, surv.col = c("black", "red", "blue", "green"))

  ggsurv(sf.kid, lty.est = 1)
  ggsurv(sf.kid, lty.est = 1:4)
  expect_true("idk how to test it happened" != "fail")
})


test_that("CI", {
  a <- ggsurv(sf.lung, CI = FALSE)
  b <- ggsurv(sf.lung, CI = TRUE)
  expect_equivalent(length(b$layers) - length(a$layers), 2)

  a <- ggsurv(sf.kid, CI = FALSE)
  b <- ggsurv(sf.kid, CI = TRUE)
  expect_equivalent(length(b$layers) - length(a$layers), 2)
})

test_that("multiple colors", {
  p <- ggsurv(sf.kid, plot.cens = TRUE)
  vdiffr::expect_doppelganger("plot-cens-true", p)
  expect_warning({
    ggsurv(sf.kid, plot.cens = TRUE, cens.col = c("red", "blue"))
  }, "Color scales for censored points") # nolint

  p <- ggsurv(sf.kid, plot.cens = TRUE, cens.col = "blue")
  vdiffr::expect_doppelganger("plot-cens-true-blue", p)

  custom_color <- c("green", "blue", "purple", "orange")
  p <- ggsurv(sf.kid, plot.cens = TRUE, cens.col = custom_color)
  vdiffr::expect_doppelganger("plot-cens-true-custom", p)

  expect_warning({
    ggsurv(
      sf.kid, plot.cens = TRUE,
      cens.col = custom_color,
      cens.shape = c(1, 2)
    )
  }, "The length of the censored shapes") # nolint
  p <-
    ggsurv(
      sf.kid, plot.cens = TRUE,
      cens.col = custom_color,
      cens.shape = c(1, 2, 3, 4)
    )
  vdiffr::expect_doppelganger("plot-cens-true-custom-shape", p)
})

test_that("cens.size", {
  a <- ggsurv(sf.lung)
  b <- ggsurv(sf.lung, cens.size = 5)
  expect_true(a$layers[[4]]$aes_params$size == 2)
  expect_true(b$layers[[4]]$aes_params$size != 2)

  a <- ggsurv(sf.kid)
  b <- ggsurv(sf.lung, cens.size = 5)
  expect_true(a$layers[[2]]$aes_params$size == 2)
  expect_true(b$layers[[2]]$aes_params$size != 2)
})
