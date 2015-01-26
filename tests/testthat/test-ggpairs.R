
context("ggpairs")
data(tips, package = "reshape")

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
  a <- ggpairs(tips, columns = columnsUsed, params = c(binwidth = 1))
  expect_equivalent(length(a$plots), length(columnsUsed)^2)
  expect_equivalent(a$columnLabels, names(tips)[columnsUsed])
})

test_that("column labels", {
  columnsUsed <- 1:3
  columnTitles <- c("A", "B", "C")
  a <- ggpairs(tips, columnsUsed, columnLabels = columnTitles, params = c(binwidth = 1))
  expect_equivalent(a$columnLabels, columnTitles)


  columnTitles <- c("Total Bill %", "Tip 123456", "Sex ( /a asdf)")
  a <- ggpairs(tips, columnsUsed, columnLabels = columnTitles, params = c(binwidth = 1))
  expect_equivalent(a$columnLabels, columnTitles)
})

test_that("character", {
  tips2 <- tips
  tips2$sex <- as.character(tips2$sex)
  tips2$smoker <- as.character(tips2$smoker)
  a <- ggpairs(tips2, params = c(binwidth = 1))
  expect_true(is.factor(a$data$sex))
  expect_true(is.factor(a$data$smoker))
})

test_that("printInfo", {
  txt <- capture.output({
    a <- ggpairs(tips, printInfo = TRUE, params = c(binwidth = 1))
  })
  expect_true(length(txt) > 0)
  expect_false(is.list(a$plots[[2]]))
  expect_false(is.list(a$plots[[8]]))

  txt <- capture.output({
    p1 <- getPlot(a, 1, 2)
    p2 <- getPlot(a, 2, 1)
    a <- putPlot(a, p1, 2, 1)
    a <- putPlot(a, "blank", 1, 2)
    print(a)
  })
  expect_true(length(txt) > 0)
  expect_true(is.character(a$plots[[2]]))
  expect_true(is.list(a$plots[[8]]))
})

test_that("blank plots", {
  columnsUsed <- 1:3
  au <- ggpairs(tips, columnsUsed, upper = "blank", params = c(binwidth = 1))
  ad <- ggpairs(tips, columnsUsed, diag = "blank", params = c(binwidth = 1))
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
  au <- ggpairs(tips, columnsUsed, upper = "blank", params = c(binwidth = 1))
  ad <- ggpairs(tips, columnsUsed, diag = "blank", params = c(binwidth = 1))
  al <- ggpairs(tips, columnsUsed, lower = "blank")
  print(au); print(ad); print(al)

  fn <- function(axisLabels) {
    a <- ggpairs(
      tips, 1:4,
      axisLabels = axisLabels,
      params = c(binwidth = 1)
    )
    a
  }
  for (axisLabels in c("show", "internal", "none")) {
    a <- fn(axisLabels)
    print(a)
  }

  expect_true(TRUE)
})

test_that("subtypes", {

# list of the different plot types to check
# continuous
#    points
#    smooth
#    density
#    cor
#   blank

# combo
#   box
#   dot plot
#   facethist
#   facetdensity
#   denstrip
#   blank

# discrete
#   ratio
#   facetbar
#   blank

  fn1 <- function(title, upper, diag, ...) {
      params <- c(binwidth = 1)
    ggpairs(
      tips, 1:4,
      axisLabels = "show",
      title = title,
      upper = upper,
      lower = upper,
      diag = diag,
      params = params,
      ...
    ) + ggplot2::theme(plot.title = ggplot2::element_text(size = 9))
  }

  fn2 <- function(...) {
    fn1(..., color = "day")
  }

  conSubs = c("points", "smooth", "density", "cor", "blank")
  comSubs = c("box", "dot", "facethist", "facetdensity", "denstrip", "blank")
  disSubs = c("ratio", "facetbar", "blank")

  conDiagSubs = c("density", "bar", "blank")
  disDiagSubs = c("bar", "blank")

  printShowStrips = c(TRUE, FALSE)

  for(fn in list(fn1, fn2)){
    for (i in 1:6) {
      conSub <- ifelse(i <= length(conSubs), conSubs[i], "blank")
      comSub <- ifelse(i <= length(comSubs), comSubs[i], "blank")
      disSub <- ifelse(i <= length(disSubs), disSubs[i], "blank")

      diagConSub <- ifelse(i <= length(conDiagSubs), conDiagSubs[i], "blank")
      diagDisSub <- ifelse(i <= length(disDiagSubs), disDiagSubs[i], "blank")

      if (i <= length(printShowStrips)) {
        printShowStrip <- printShowStrips[i]
      } else {
        printShowStrip <- NULL
      }

      a <- fn(
        title = paste(
          "upper_lower = c(cont = ", conSub,
            ", combo = ", comSub,
            ", discrete = ", disSub,
          "); diag = c(cont = ", diagConSub,
            ", discrete = ", diagDisSub,
          ")", sep = ""),
        upper = list(
          continuous = conSub,
          combo = comSub,
          discrete = disSub
        ),
        diag = list(
          continuous = diagConSub,
          discrete = diagDisSub
        )
      )

      print(a, showStrips = printShowStrip)
    }
  }

  a <- ggpairs(tips, 1:2, lower = "blank", diag = "blank", upper = list(continuous = "BAD_TYPE"), params = c(binwidth = 1))
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



