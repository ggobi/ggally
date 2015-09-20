
context("ggpairs")
data(tips, package = "reshape")

facethistBindwidth1 = list(combo = wrap("facethist", binwidth = 1))

test_that("structure", {
  a <- ggpairs(tips)
  expect_is(a$data, "data.frame")
  expect_is(a$plots, "list")
  expect_equivalent(length(a$plots), ncol(tips) ^ 2)
  expect_is(a$title, "character")
  expect_is(a$verbose, "logical")
  expect_is(a$printInfo, "logical")
  expect_is(a$axisLabels, "character")
  expect_is(a$xAxisLabels, "character")
  expect_is(a$yAxisLabels, "character")
  expect_is(a$legends, "logical")
  expect_is(a$byrow, "logical")
  expect_true(is.null(a$gg))
  expect_true("gg" %in% names(a))
})

test_that("columns", {
  columnsUsed <- c("total_bill", "tip", "sex")
  a <- ggpairs(tips, columns = columnsUsed)
  expect_equivalent(length(a$plots), length(columnsUsed) ^ 2)
  expect_equivalent(a$xAxisLabels, columnsUsed)
  expect_equivalent(a$yAxisLabels, columnsUsed)
})

test_that("column labels", {
  columnsUsed <- 1:3
  columnTitles <- c("A", "B", "C")
  a <- ggpairs(tips, columnsUsed, columnLabels = columnTitles)
  expect_equivalent(a$xAxisLabels, columnTitles)
  expect_equivalent(a$yAxisLabels, columnTitles)


  columnTitles <- c("Total Bill %", "Tip 123456", "Sex ( /a asdf)")
  a <- ggpairs(tips, columnsUsed, columnLabels = columnTitles)
  expect_equivalent(a$xAxisLabels, columnTitles)
  expect_equivalent(a$yAxisLabels, columnTitles)
})

test_that("character", {
  tips2 <- tips
  tips2$sex <- as.character(tips2$sex)
  tips2$smoker <- as.character(tips2$smoker)
  a <- ggpairs(tips2)
  expect_true(is.factor(a$data$sex))
  expect_true(is.factor(a$data$smoker))
})

test_that("printInfo", {
  txt <- capture.output({
    a <- ggpairs(tips, 1:4, lower = facethistBindwidth1, printInfo = TRUE)
  })
  expect_true(length(txt) > 0)
  expect_true(inherits(a$plots[[2]], "ggmatrix_plot_obj"))
  expect_true(inherits(a$plots[[3]], "ggmatrix_plot_obj"))

  txt <- capture.output({
    p1 <- a[1, 2]
    p2 <- a[2, 1]
    a[2,1] <- p1
    a[1,2] <- "blank"

    print(a)
  })
  expect_true(length(txt) > 0)
  expect_true(is_blank_plot(a$plots[[2]]))
  expect_true(is.list(a$plots[[4 + 1]]))
})

test_that("blank plots", {
  columnsUsed <- 1:3
  au <- ggpairs(tips, columnsUsed, upper = "blank")
  ad <- ggpairs(tips, columnsUsed, diag = "blank")
  al <- ggpairs(tips, columnsUsed, lower = "blank")
  for (i in 1:3) {
    for (j in 1:3) {
      if(i < j) {
        expect_true(  is_blank_plot(au[i, j]))
        expect_false( is_blank_plot(ad[i, j]))
        expect_false( is_blank_plot(al[i, j]))
      }
      if(i > j) {
        expect_false( is_blank_plot(au[i, j]))
        expect_false( is_blank_plot(ad[i, j]))
        expect_true(  is_blank_plot(al[i, j]))
      }
      if(i == j) {
        expect_false( is_blank_plot(au[i, j]))
        expect_true(  is_blank_plot(ad[i, j]))
        expect_false( is_blank_plot(al[i, j]))
      }
    }
  }

  a <- ggpairs(tips, columnsUsed)
  a[1,1] <- ggplot2::qplot(total_bill, data = tips)
  expect_false(is_blank_plot(a[1,1]))

})

test_that("stops", {
  expect_warning(ggpairs(tips, axisLabels = "not_a_chosen", lower = facethistBindwidth1), "'axisLabels' not in ")

  expect_error(ggpairs(tips, columns = 1:10), "Make sure your 'columns' values are less than or equal to")
  expect_error(ggpairs(tips, columns = -5:5), "Make sure your 'columns' values are positive")
  expect_error(ggpairs(tips, columns = (2:10) / 2), "Make sure your 'columns' values are integers")
  expect_error(ggpairs(tips, columns = 1:3, columnLabels = c("A", "B", "C", "Extra")), "The length of the 'columnLabels' does not match the length of")

  dt <- tips
  colnames(dt)[3] <- "1"
  expect_warning(ggpairs(dt, lower = facethistBindwidth1), "Column name is numeric")

  expect_error(ggpairs(tips, upper = c("not_a_list")), "'upper' is not a list")
  expect_error(ggpairs(tips, diag = c("not_a_list")), "'diag' is not a list")
  expect_error(ggpairs(tips, lower = c("not_a_list")), "'lower' is not a list")

  # couldn't get correct error message
  #  variables: 'colour' have non standard format: 'total_bill + tip'.
  expect_error(ggpairs(tips, mapping = aes(color = total_bill + tip)))

})

test_that("print", {
  columnsUsed <- 1:3
  au <- ggpairs(tips, columnsUsed, upper = "blank", lower = facethistBindwidth1)
  ad <- ggpairs(tips, columnsUsed, diag = "blank", lower = facethistBindwidth1)
  al <- ggpairs(tips, columnsUsed, lower = "blank")
  print(au); print(ad); print(al)

  fn <- function(axisLabels) {
    a <- ggpairs(
      tips, 1:4, lower = facethistBindwidth1,
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
    ggpairs(
      tips, 1:4,
      axisLabels = "show",
      title = title,
      upper = upper,
      lower = upper,
      diag = diag,
      ...
    ) + ggplot2::theme(plot.title = ggplot2::element_text(size = 9))
  }

  fn2 <- function(...) {
    fn1(..., mapping = aes(color = day))
  }

  # re ordered the subs so that density can have no binwidth param
  conSubs = list("density", "points", "smooth", "cor", "blank")
  comSubs = list("box", "dot", wrap("facethist", binwidth = 1), "facetdensity", wrap("denstrip", binwidth = 1), "blank")
  disSubs = list("ratio", "facetbar", "blank")

  conDiagSubs = c("densityDiag", wrap("barDiag", binwidth = 1), "blankDiag")
  disDiagSubs = c("barDiag", "blankDiag")

  printShowStrips = c(TRUE, FALSE)

  gn <- function(x) {
    fnName <- attr(x, "fnName")
    if (is.null(fnName)) {
      x
    } else {
      fnName
    }
  }

  for(fn in list(fn1, fn2)){
    for (i in 1:6) {
      conSub <- if(i <= length(conSubs)) conSubs[[i]] else "blank"
      comSub <- if(i <= length(comSubs)) comSubs[[i]] else "blank"
      disSub <- if(i <= length(disSubs)) disSubs[[i]] else "blank"

      diagConSub <- if(i <= length(conDiagSubs)) conDiagSubs[[i]] else "blank"
      diagDisSub <- if(i <= length(disDiagSubs)) disDiagSubs[[i]] else "blank"

      if (i <= length(printShowStrips)) {
        printShowStrip <- printShowStrips[i]
      } else {
        printShowStrip <- NULL
      }

      a <- fn(
        title = paste(
          "upper_lower = c(cont = ", gn(conSub),
            ", combo = ", gn(comSub),
            ", discrete = ", gn(disSub),
          "); diag = c(cont = ", gn(diagConSub),
            ", discrete = ", gn(diagDisSub),
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

  expect_error({
    ggpairs(tips, 1:2, lower = "blank", diag = "blank", upper = list(continuous = "BAD_TYPE"))
  })
  expect_true(TRUE)

})

test_that("dates", {
  startDt <- as.POSIXct("2000-01-01", tz = "UTC")
  endDt   <- as.POSIXct("2000-04-01", tz = "UTC")

  dts <- seq(startDt, endDt, 86400) # 86400 = as.numeric(ddays(1))
  x <- data.frame(
    date = dts,
    x1 = rnorm(length(dts)),
    x2 = rnorm(length(dts)),
    cat = sample(c("a", "b", "c"), length(dts), replace = TRUE)
  )

  class(x) <- c("NOT_data.frame", "data.frame")

  a <- ggpairs(x, c(2,1,4,3), mapping = aes(color = cat), lower = "blank", upper = list(continuous = "cor"))
  p <- a[1, 2]
  expect_equal(p$type, "continuous")
  expect_equal(p$subType, "cor")

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
