


context("crosstalk")

test_that("crosstalk works with ggduo and ggpairs", {

  skip_if_not_installed("crosstalk")

  sd <- crosstalk::SharedData$new(iris[1:4])

  expect_silent({
    pm <- ggpairs(sd)
  })
  expect_error({
      pm <- ggpairs(sd, 3:5)
    },
    "Make sure your numeric"
  )
  expect_error({
      pm <- ggpairs(sd, c("Petal.Length", "Petal.Width", crosstalk_key()))
    },
    "Columns in 'columns' not"
  )

  expect_silent({
    pm <- ggduo(sd)
  })
  expect_error({
      pm <- ggduo(sd, c(1:2, 5), 3:5)
    },
    "Make sure your numeric 'columnsX'"
  )
  expect_error({
      pm <- ggduo(
        sd,
        c("Sepal.Length", "Sepal.Width", crosstalk_key()),
        c("Petal.Length", "Petal.Width")
      )
    },
    "Columns in 'columnsX' not"
  )
  expect_error({
      pm <- ggduo(
        sd,
        c("Sepal.Length", "Sepal.Width"),
        c("Petal.Length", "Petal.Width", crosstalk_key())
      )
    },
    "Columns in 'columnsY' not"
  )

})
