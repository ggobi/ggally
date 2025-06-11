
suppressMessages(require(broom))

test_that("example", {
  reg <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data = iris)
  p <- ggcoef(reg)
  vdiffr::expect_doppelganger("lm", p)

  skip_if_not_installed("MASS")
  d <- as.data.frame(Titanic)
  reg2 <- glm(Survived ~ Sex + Age + Class, family = binomial, data = d, weights = d$Freq)
  p <- ggcoef(reg2, exponentiate = TRUE)
  vdiffr::expect_doppelganger("lm-expo", p)
  p <- ggcoef(
    reg2,
    exponentiate = TRUE,
    exclude_intercept = TRUE,
    errorbar_height = .2,
    color = "blue"
  )
  vdiffr::expect_doppelganger("lm-expo-blue", p)
})
