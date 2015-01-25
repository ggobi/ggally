
# context("ggpairs-setup")
data(tips, package = "reshape")

context("ggpairs")
test_that("structure", {
  a <- ggpairs(tips)
  expect_is(a$data, "data.frame")
  expect_is(a$columns, "integer")
  expect_is(a$plots, "list")
  expect_equivalent(length(a$plots), ncol(tips)^2)
  expect_is(a$title, "character")
  expect_is(a$verbose, "logical")
  expect_is(a$printInfo, "logical")
  expect_is(a$axisLabels, "character")
  expect_is(a$columnLabels, "character")
  expect_is(a$legends, "logical")
  expect_true(is.null(a$gg))
  expect_true("gg" %in% names(a))
})

test_that("columns", {
  columnsUsed <- 1:3
  a <- ggpairs(tips, columns = columnsUsed)
  expect_equivalent(length(a$plots), length(columnsUsed)^2)
  expect_equivalent(a$columnLabels, names(tips)[columnsUsed])
})

test_that("column labels", {
  columnsUsed <- 1:3
  columnTitles <- c("A", "B", "C")
  a <- ggpairs(tips, columnsUsed, columnLabels = columnTitles)
  expect_equivalent(a$columnLabels, columnTitles)


  columnTitles <- c("Total Bill %", "Tip 123456", "Sex ( /a asdf)")
  a <- ggpairs(tips, columnsUsed, columnLabels = columnTitles)
  expect_equivalent(a$columnLabels, columnTitles)
})

test_that("blank plots", {
  columnsUsed <- 1:3
  au <- ggpairs(tips, columnsUsed, upper = "blank")
  ad <- ggpairs(tips, columnsUsed, diag = "blank")
  al <- ggpairs(tips, columnsUsed, lower = "blank")
  for (i in 1:3) {
    for (j in 1:3) {
      if(i < j) {
        expect_true(is_blank_plot(getPlot(au, i, j)))
        expect_false(is_blank_plot(getPlot(ad, i, j)))
        expect_false(is_blank_plot(getPlot(al, i, j)))
      }
      if(i > j) {
        expect_false(is_blank_plot(getPlot(au, i, j)))
        expect_false(is_blank_plot(getPlot(ad, i, j)))
        expect_true(is_blank_plot(getPlot(al, i, j)))
      }
      if(i == j) {
        expect_false(is_blank_plot(getPlot(au, i, j)))
        expect_true(is_blank_plot(getPlot(ad, i, j)))
        expect_false(is_blank_plot(getPlot(al, i, j)))
      }
    }
  }

})

test_that("stops", {
  expect_warning(ggpairs(tips, axisLabels = "not_a_chosen"), "'axisLabels' not in ")

  expect_error(ggpairs(tips, columns = 1:10), "Make sure your 'columns' values are less than or equal to")
  expect_error(ggpairs(tips, columns = -5:5), "Make sure your 'columns' values are positive")
  expect_error(ggpairs(tips, columns = (2:10)/2), "Make sure your 'columns' values are integers")
  expect_error(ggpairs(tips, columns = 1:3, columnLabels = c("A", "B", "C", "Extra")), "The length of the 'columnLabels' does not match the length of")

  dt <- tips
  colnames(dt)[3] <- "1"
  expect_warning(ggpairs(dt), "Column name is numeric")

  expect_error(ggpairs(tips, upper = c("not_a_list")), "'upper' is not a list")
  expect_error(ggpairs(tips, diag = c("not_a_list")), "'diag' is not a list")
  expect_error(ggpairs(tips, lower = c("not_a_list")), "'lower' is not a list")

  # couldn't get correct error message
  #  variables: 'colour' have non standard format: 'total_bill + tip'.
  expect_error(ggpairs(tips, color = "total_bill + tip"))

})

test_that("print", {
  columnsUsed <- 1:3
  au <- ggpairs(tips, columnsUsed, upper = "blank")
  ad <- ggpairs(tips, columnsUsed, diag = "blank")
  al <- ggpairs(tips, columnsUsed, lower = "blank")
  print(au); print(ad); print(al)

  fn <- function(axisLabels) {
    a <- ggpairs(
      tips, 1:4, upper = "blank", diag = "blank",
      axisLabels = axisLabels
    )
    a
  }
  for (axisLabels in c("show", "internal", "none")) {
    a <- fn(axisLabels)
    print(a)
  }

  expect_true(TRUE)
})




# pm <- ggpairs(tips, upper = "blank")
# # pm


#  # Custom Example
#  pm <- ggpairs(
#    tips[,c(1,3,4,2)],
#    upper = list(continuous = "density", combo = "box"),
#    lower = list(continuous = "points", combo = "dot")
#  )
#  # pm

#  # Use sample of the diamonds data
#  data(diamonds, package="ggplot2")
#  diamonds.samp <- diamonds[sample(1:dim(diamonds)[1],200),]

#  # Custom Example
#  pm <- ggpairs(
#   diamonds.samp[,1:5],
#   upper = list(continuous = "density", combo = "box"),
#   lower = list(continuous = "points", combo = "dot"),
#   color = "cut",
#   alpha = 0.4,
#   title = "Diamonds"
#  )
#  # pm

#  # Will plot four "Incorrect Plots"
#  bad_plots <- ggpairs(
#    tips[,1:3],
#    upper = list(continuous = "wrongType1", combo = "wrongType2"),
#    lower = list(continuous = "IDK1", combo = "IDK2", discrete = "mosaic"),
#  )
#  # bad_plots

#  # Only Variable Labels on the diagonal (no axis labels)
#  pm <- ggpairs(tips[,1:3], axisLabels="internal")
#  # pm
#  # Only Variable Labels on the outside (no axis labels)
#  pm <- ggpairs(tips[,1:3], axisLabels="none")
#  # pm

#  # Custom Examples
#  custom_car <- ggpairs(mtcars[,c("mpg","wt","cyl")], upper = "blank", title = "Custom Example")
# #' # ggplot example taken from example(geom_text)
# #'   plot <- ggplot2::ggplot(mtcars, ggplot2::aes(x=wt, y=mpg, label=rownames(mtcars)))
# #'   plot <- plot +
# #'     ggplot2::geom_text(ggplot2::aes(colour=factor(cyl)), size = 3) +
# #'     ggplot2::scale_colour_discrete(l=40)
# #' custom_car <- putPlot(custom_car, plot, 1, 2)
# #' personal_plot <- ggally_text(
# #'   "ggpairs allows you\nto put in your\nown plot.\nLike that one.\n <---"
# #' )
# #' custom_car <- putPlot(custom_car, personal_plot, 1, 3)
# #' # custom_car



