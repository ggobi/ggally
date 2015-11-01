
context("ggsurv")
require(survival)
require(scales)
data(lung, package = "survival")
data(kidney, package = "survival")

sf.lung <- survival::survfit(Surv(time, status) ~ 1, data = lung)
sf.kid <- survival::survfit(Surv(time, status) ~ disease, data = kidney)

test_that("single", {

  a <- ggsurv(sf.lung)

  expect_equivalent(as.character(a$mapping$x), "time")
  expect_equivalent(as.character(a$mapping$y), "surv")

  expect_true(is.null(a$labels$group))
  expect_true(is.null(a$labels$colour))
  expect_true(is.null(a$labels$linetype))
})

test_that("multiple", {

  a <- ggsurv(sf.kid)

  expect_equivalent(as.character(a$mapping$x), "time")
  expect_equivalent(as.character(a$mapping$y), "surv")

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
  expect_error(
    ggsurv(sf.kid, CI = TRUE, surv.col = c("black", "red", "blue", "green")),
    "Either surv.col or lty.est should be"
  )
  expect_error(ggsurv(sf.kid, CI = TRUE, lty.est = 1:4), "Either surv.col or lty.est should be")

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






# 881            R/ggsurv.r        231       231     0
# 883            R/ggsurv.r        242       242     0

# 884            R/ggsurv.r        247       249     0
# 885            R/ggsurv.r        248       248     0
# 886            R/ggsurv.r        251       255     0
# 887            R/ggsurv.r        252       252     0
# 888            R/ggsurv.r        254       254     0
# 889            R/ggsurv.r        256       258     0
# 890            R/ggsurv.r        263       263     0
# 891            R/ggsurv.r        274       274     0
