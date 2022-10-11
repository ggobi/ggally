context("ggcoef_model")

test_that("example of ggcoef_model", {
  skip_if_not_installed("broom.helpers")
  skip_if_not_installed("reshape")

  data(tips, package = "reshape")
  mod_simple <- lm(tip ~ day + time + total_bill, data = tips)
  p <- ggcoef_model(mod_simple)
  vdiffr::expect_doppelganger("lm", p)

  p <- ggcoef_model(mod_simple, shape_guide = FALSE, colour_guide = FALSE)
  vdiffr::expect_doppelganger("lm-noguide", p)

  # custom variable labels
  # you can use to define variable labels before computing model
  if (require(labelled)) {
    tips_labelled <- tips %>%
      labelled::set_variable_labels(
        day = "Day of the week",
        time = "Lunch or Dinner",
        total_bill = "Bill's total"
      )
    mod_labelled <- lm(tip ~ day + time + total_bill, data = tips_labelled)
    p <- ggcoef_model(mod_labelled)
    vdiffr::expect_doppelganger("lm-labelled", p)
  }

  p <- ggcoef_model(
    mod_simple,
    variable_labels = c(
      day = "Week day",
      time = "Time (lunch or dinner ?)",
      total_bill = "Total of the bill"
    )
  )
  vdiffr::expect_doppelganger("lm-variable-labels", p)
  # if labels are too long, you can use 'facet_labeller' to wrap them
  p <- ggcoef_model(
    mod_simple,
    variable_labels = c(
      day = "Week day",
      time = "Time (lunch or dinner ?)",
      total_bill = "Total of the bill"
    ),
    facet_labeller = label_wrap_gen(10)
  )
  vdiffr::expect_doppelganger("lm-variable-labels-facet", p)

  # do not display variable facets but add colour guide
  p <- ggcoef_model(mod_simple, facet_row = NULL, colour_guide = TRUE)
  vdiffr::expect_doppelganger("lm-color-nofacet", p)


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
  p <- ggcoef_model(mod_titanic, exponentiate = TRUE)
  vdiffr::expect_doppelganger("glm-expo", p)


  # display intercepts
  p <- ggcoef_model(mod_titanic, exponentiate = TRUE, intercept = TRUE)
  vdiffr::expect_doppelganger("glm-expo-intercept", p)


  # display only a subset of terms
  p <- ggcoef_model(mod_titanic, exponentiate = TRUE, include = c("Age", "Class"))
  vdiffr::expect_doppelganger("glm-expo-include", p)

  # do not change points' shape based on significance
  p <- ggcoef_model(mod_titanic, exponentiate = TRUE, significance = NULL)
  vdiffr::expect_doppelganger("glm-expo-significance", p)

  # a black and white version
  p <- ggcoef_model(
    mod_titanic, exponentiate = TRUE,
    colour = NULL, stripped_rows = FALSE
  )
  vdiffr::expect_doppelganger("glm-expo-no-color-rowstrips", p)

  # show dichotomous terms on one row
  p <- ggcoef_model(
    mod_titanic,
    exponentiate = TRUE,
    no_reference_row = broom.helpers::all_dichotomous(),
    categorical_terms_pattern = "{ifelse(dichotomous, paste0(level, ' / ', reference_level), level)}",
    show_p_values = FALSE
  )
  vdiffr::expect_doppelganger("glm-expo-no-ref", p)

  # works also with with polynomial terms
  mod_poly <- lm(
    tip ~ poly(total_bill, 3) + day,
    data = tips,
  )
  p <- ggcoef_model(mod_poly)
  vdiffr::expect_doppelganger("poly", p)

  # or with different type of contrasts
  # for sum contrasts, the value of the reference term is computed
  emmeans_is_installed <- (system.file(package = "emmeans") != "")
  if (emmeans_is_installed) {
    mod2 <- lm(
      tip ~ day + time + sex,
      data = tips,
      contrasts = list(time = contr.sum, day = contr.treatment(4, base = 3))
    )
    p <- ggcoef_model(mod2)
    vdiffr::expect_doppelganger("contrasts", p)
  }

  # Use ggcoef_compare() for comparing several models on the same plot
  mod1 <- lm(Fertility ~ ., data = swiss)
  mod2 <- step(mod1, trace = 0)
  mod3 <- lm(Fertility ~ Agriculture + Education * Catholic, data = swiss)
  models <- list("Full model" = mod1, "Simplified model" = mod2, "With interaction" = mod3)

  p <- ggcoef_compare(models)
  vdiffr::expect_doppelganger("models", p)
  p <- ggcoef_compare(models, type = "faceted")
  vdiffr::expect_doppelganger("models-faceted", p)

  # specific function for nnet::multinom models
  skip_if_not_installed("nnet")
  library(nnet)
  data(happy)
  mod <- multinom(happy ~ age + degree + sex, data = happy)
  p <- ggcoef_multinom(mod, exponentiate = TRUE)
  vdiffr::expect_doppelganger("multinom-expo", p)
  p <- ggcoef_multinom(mod, type = "faceted")
  vdiffr::expect_doppelganger("multinom-faceted", p)
  p <- ggcoef_multinom(
    mod, type = "faceted",
    y.level_label = c(
      "pretty happy" = "pretty happy\n(ref: very happy)"
    )
  )
  vdiffr::expect_doppelganger("multinom-faceted-ylabel", p)


})

test_that("ggcoef_model works with tieders not returning p-values", {
  skip_if_not_installed("broom.helpers")
  skip_if_not_installed("scagnostics")

  mod <- lm(Sepal.Width ~ Species, iris)
  my_tidier <- function(x, ...) {
    x %>%
      broom::tidy(...) %>%
      dplyr::select(-.data$p.value)
  }
  expect_error(
    mod %>% ggcoef_model(tidy_fun = my_tidier),
    NA
  )

})

test_that("ggcoef_compare complete missing data by respecting the order if variables", {
  m1 <- lm(Fertility ~ Education + Catholic, data = swiss)
  m2 <- lm(Fertility ~ Education + Catholic + Agriculture, data = swiss)
  m3 <- lm(Fertility ~ Education + Catholic + Agriculture + Infant.Mortality, data = swiss)
  res <- ggcoef_compare(models = list(m1, m2, m3), return_data = TRUE)
  expect_equal(
    res$variable[1:4],
    structure(1:4, .Label = c("Education", "Catholic", "Agriculture",
                              "Infant.Mortality"), class = "factor")
  )
})

test_that("ggcoef_compare() does not produce an error with an include", {
  skip_if_not_installed("survival")
  skip_if_not_installed("broom.helpers")
  m1 <- survival::coxph(survival::Surv(time, status) ~ prior + age, data = survival::veteran)
  m2 <- survival::coxph(survival::Surv(time, status) ~ prior + celltype, data = survival::veteran)
  models <- list("Model 1" = m1, "Model 2" = m2)

  expect_error(
    ggcoef_compare(models, include = broom.helpers::starts_with("p")),
    NA
  )

})
