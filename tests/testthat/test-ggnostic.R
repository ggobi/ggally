context("ggnostic")

expect_print <- function(p) {
  testthat::expect_silent({
    print(p)
  })
}


test_that("fn_switch", {
  fn1 <- function(data, mapping, ...) {
    return(1)
  }
  fn2 <- function(data, mapping, ...) {
    return(2)
  }
  fn3 <- function(data, mapping, ...) {
    return(3)
  }
  fn5 <- function(data, mapping, ...) {
    return(5)
  }

  fn <- fn_switch(list(A = fn1, B = fn2, C = fn3), "value")

  dummy_dt <- data.frame(A = rnorm(100), B = rnorm(100), C = rnorm(100))

  chars <- c("A", "B", "C")
  for (i in 1:3) {
    mapping <- ggplot2::aes_string(value = chars[i])
    expect_equal(fn(dummy_dt, mapping), i)
  }


  fn <- fn_switch(list(A = fn1, default = fn5), "value")
  expect_equal(fn(dummy_dt, ggplot2::aes_string(value = "A")), 1)
  expect_equal(fn(dummy_dt, ggplot2::aes_string(value = "B")), 5)
  expect_equal(fn(dummy_dt, ggplot2::aes_string(value = "C")), 5)

  fn <- fn_switch(list(A = fn1), "value")
  expect_equal(fn(dummy_dt, ggplot2::aes_string(value = "A")), 1)
  expect_error(fn(dummy_dt, ggplot2::aes_string(value = "B")), "function could not be found")

})

test_that("model_beta_label", {
  mod <- lm(mpg ~ wt + qsec + am, mtcars)

  expect_equal(model_beta_label(mod), c("wt***", "qsec***", "am*"))
  expect_equal(model_beta_label(mod, lmStars = FALSE), c("wt", "qsec", "am"))
})

test_that("ggnostic mtcars", {

  mtc <- mtcars;
  mtc$am <- c("0" = "automatic", "1" = "manual")[as.character(mtc$am)];

  mod <- lm(mpg ~ wt + qsec + am, data = mtc);
  continuous_type <- list(
    .resid = wrap(ggally_nostic_resid, method = "loess"),
    .std.resid = wrap(ggally_nostic_std_resid, method = "loess")
  )

  pm <- ggnostic(
    mod,
    mapping = ggplot2::aes(),
    columnsY = c("mpg", ".fitted", ".se.fit", ".resid", ".std.resid", ".sigma", ".hat", ".cooksd"),
    continuous = continuous_type,
    progress = FALSE
  )
  expect_print(pm)

  pm <- ggnostic(
    mod,
    mapping = ggplot2::aes(color = am),
    legend = c(1, 3),
    continuous = continuous_type,
    progress = FALSE
  )
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

  expect_equivalent(
    get_cols(c(".resid", ".sig", ".hat", ".c")),
    c(".resid", ".sigma", ".hat", ".cooksd")
  )

  expect_error(
    get_cols(c(
      "not_there", ".fitted", ".se.fit", ".resid",
      ".std.resid", ".sigma", ".hat", ".cooksd"
    )),
    "Could not match 'columnsY'"
  )

})
