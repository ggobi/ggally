context("ggcoef_model")

test_that("example of ggcoef_model", {
  suppressMessages(require(broom))
  suppressMessages(require(broom.helpers))

  expect_print <- function(x) {
    expect_silent(print(x))
  }
  skip_if_not_installed("reshape")

  data(tips, package = "reshape")
  mod_simple <- lm(tip ~ day + time + total_bill, data = tips)
  expect_print(ggcoef_model(mod_simple))

  # custom variable labels
  # you can use to define variable labels before computing model
  if (require(labelled)) {
    tips_labelled <- tips %>%
      set_variable_labels(
        day = "Day of the week",
        time = "Lunch or Dinner",
        total_bill = "Bill's total"
      )
    mod_labelled <- lm(tip ~ day + time + total_bill, data = tips_labelled)
    expect_print(ggcoef_model(mod_labelled))
  }

  expect_print(ggcoef_model(
    mod_simple,
    variable_labels = c(
      day = "Week day",
      time = "Time (lunch or dinner ?)",
      total_bill = "Total of the bill"
    )
  ))
  # if labels are too long, you can use 'facet_labeller' to wrap them
  expect_print(ggcoef_model(
    mod_simple,
    variable_labels = c(
      day = "Week day",
      time = "Time (lunch or dinner ?)",
      total_bill = "Total of the bill"
    ),
    facet_labeller = label_wrap_gen(10)
  ))

  # do not display variable facets but add colour guide
  expect_print(ggcoef_model(mod_simple, facet_row = NULL, colour_guide = TRUE))

  # groupe variables displayed on one row
  expect_print(ggcoef_model(mod_simple, no_reference_row = "time", group_one_row_variables = TRUE))

  # a logistic regression example
  d_titanic <- as.data.frame(Titanic)
  d_titanic$Survived <- factor(d_titanic$Survived, c("No", "Yes"))
  mod_titanic <- glm(
    Survived ~ Sex * Age + Class,
    weights = Freq,
    data = d_titanic,
    family = binomial
  )

  # use 'exponentiate = TRUE' to get the Odds Ratio
  expect_print(ggcoef_model(mod_titanic, exponentiate = TRUE))

  # display intercepts
  expect_print(ggcoef_model(mod_titanic, exponentiate = TRUE, intercept = TRUE))

  # display only a subset of terms
  expect_print(ggcoef_model(mod_titanic, exponentiate = TRUE, include = c("Age", "Class")))

  # do not change points' shape based on significance
  expect_print(ggcoef_model(mod_titanic, exponentiate = TRUE, significance = NULL))

  # a black and white version
  expect_print(ggcoef_model(
    mod_titanic, exponentiate = TRUE,
    colour = NULL, stripped_rows = FALSE
  ))

  # show dichotomous terms on one row
  expect_print(ggcoef_model(
    mod_titanic,
    exponentiate = TRUE,
    no_reference_row = broom.helpers::all_dichotomous(),
    categorical_terms_pattern = "{ifelse(dichotomous, paste0(level, ' / ', reference_level), level)}",
    show_p_values = FALSE
  ))

  # works also with with polynomial terms
  mod_poly <- lm(
    tip ~ poly(total_bill, 3) + day,
    data = tips,
  )
  expect_print(ggcoef_model(mod_poly))

  # or with different type of contrasts
  # for sum contrasts, the value of the reference term is computed
  mod2 <- lm(
    tip ~ day + time + sex,
    data = tips,
    contrasts = list(time = contr.sum, day = contr.treatment(4, base = 3))
  )
  expect_print(ggcoef_model(mod2))

  # Use ggcoef_compare() for comparing several models on the same plot
  mod1 <- lm(Fertility ~ ., data = swiss)
  mod2 <- step(mod1, trace = 0)
  mod3 <- lm(Fertility ~ Agriculture + Education * Catholic, data = swiss)
  models <- list("Full model" = mod1, "Simplified model" = mod2, "With interaction" = mod3)

  expect_print(ggcoef_compare(models))
  expect_print(ggcoef_compare(models, type = "faceted"))

  # specific function for nnet::multinom models
  skip_if_not_installed("nnet")
  library(nnet)
  data(happy)
  mod <- multinom(happy ~ age + degree + sex, data = happy)
  expect_print(ggcoef_multinom(mod, exponentiate = TRUE))
  expect_print(ggcoef_multinom(mod, type = "faceted"))
  expect_print(ggcoef_multinom(
    mod, type = "faceted",
    y.level = c(
      "pretty happy" = "pretty happy\n(ref: very happy)"
    )
  ))


})
