context("ggnostic")

expect_print <- function(p) {
  testthat::expect_silent(print(p, progress = FALSE))
}


test_that("ggnostic mtcars", {

  mtc <- mtcars;
  mtc$am <- c("0" = "automatic", "1" = "manual")[as.character(mtc$am)];

  mod <- lm(mpg ~ wt + qsec + am, data = mtc);

  pm <- ggnostic(mod, mapping = ggplot2::aes(), columnsY = c("mpg", ".fitted", ".se.fit", ".resid", ".std.resid", ".sigma", ".hat", ".cooksd"))
  expect_print(pm)

  pm <- ggnostic(mod, mapping = ggplot2::aes(color = am), legend = c(1, 3))
  expect_print(pm)
})



test_that("error checking", {
  data(tips, package = "reshape2")
  mod <- lm(tip ~ total_bill + size + day, data = tips)
  # mod <- lm(tip ~ total_bill + size, data = tips)
  broom_augment_rows <- broomify(mod)
  ggnostic(mod)

})
