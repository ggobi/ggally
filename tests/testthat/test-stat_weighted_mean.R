context("stat_weighted_mean")

test_that("example", {
  expect_print <- function(x) {
    expect_silent(print(x))
  }

  skip_if_not_installed("reshape")
  data(tips, package = "reshape")

  expect_print(ggplot(tips) +
       aes(x = day, y = total_bill) +
       geom_point())

  expect_print(ggplot(tips) +
       aes(x = day, y = total_bill) +
       stat_weighted_mean())

  expect_print(ggplot(tips) +
       aes(x = day, y = total_bill, group = 1) +
       stat_weighted_mean(geom = "line"))

  expect_print(ggplot(tips) +
       aes(x = day, y = total_bill, colour = sex, group = sex) +
       stat_weighted_mean(geom = "line"))

  expect_print(ggplot(tips) +
       aes(x = day, y = total_bill, fill = sex) +
       stat_weighted_mean(geom = "bar", position = "dodge"))

  # computing a proportion on the fly
  expect_print(ggplot(tips) +
       aes(x = day, y = as.integer(smoker == "Yes"), fill = sex) +
       stat_weighted_mean(geom = "bar", position = "dodge") +
       scale_y_continuous(labels = scales::percent))

  # taking into account some weights
  d <- as.data.frame(Titanic)
  expect_print(ggplot(d) +
       aes(x = Class, y = as.integer(Survived == "Yes"), weight = Freq, fill = Sex) +
       geom_bar(stat = "weighted_mean", position = "dodge") +
       scale_y_continuous(labels = scales::percent) +
       labs(y = "Survived"))


  tips_f <- tips
  tips_f$day <- factor(tips$day, c("Thur", "Fri", "Sat", "Sun"))

  # Numeric variable
  expect_print(ggally_trends(tips_f, mapping = aes(x = day, y = total_bill)))
  expect_print(ggally_trends(tips_f, mapping = aes(x = day, y = total_bill, colour = time)))

  # Binary variable
  expect_print(ggally_trends(tips_f, mapping = aes(x = day, y = smoker)))
  expect_print(ggally_trends(tips_f, mapping = aes(x = day, y = smoker, colour = sex)))

  # Discrete variable with 3 or more categories
  expect_print(ggally_trends(tips_f, mapping = aes(x = smoker, y = day)))
  expect_print(ggally_trends(tips_f, mapping = aes(x = smoker, y = day, color = sex)))

  # Include zero on Y axis
  expect_print(ggally_trends(tips_f, mapping = aes(x = day, y = total_bill), include_zero = TRUE))
  expect_print(ggally_trends(tips_f, mapping = aes(x = day, y = smoker), include_zero = TRUE))

  # Change line size
  expect_print(ggally_trends(tips_f, mapping = aes(x = day, y = smoker, colour = sex), size = 3))

  # Define weights with the appropriate aesthetic
  d <- as.data.frame(Titanic)
  expect_print(ggally_trends(
    d,
    mapping = aes(x = Class, y = Survived, weight = Freq, color = Sex),
    include_zero = TRUE
  ))

})
