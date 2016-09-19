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

  get_cols <- function(cols) {
    match_nostic_columns(
      cols,
      c("mpg", broom_columns()),
      "columnsY"
    )
  }

  expect_equivalent(get_cols(c(".resid", ".sig", ".hat", ".c")), c(".resid", ".sigma", ".hat", ".cooksd"))

  expect_error(
    get_cols(c(
      "not_there", ".fitted", ".se.fit", ".resid",
      ".std.resid", ".sigma", ".hat", ".cooksd"
    )),
    "Could not match 'columnsY'"
  )

})
