#' Evaluate a GGally Function
#'
#' Evaluate and GGally function with data, mapping, and parameters.
#'
#' @param txt text that should be evaluated to create a plot
#' @param ggally_data data that should be used when evaluating the text
#' @keywords internal
eval_ggpair <- function(txt, ggally_data) {
  con <- textConnection(txt)
  on.exit(close(con))
  output <- eval(parse(con), envir = list(ggally_data = ggally_data))
  output
}


#' Put Plot
#'
#' Function to place your own plot in the layout.
#'
#' @param x ggally object to be altered
#' @param value ggplot object to be placed
#' @param i row from the top
#' @param j column from the left
#' @keywords hplot
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @export
#' @examples
#' custom_car <- ggpairs(mtcars[, c("mpg", "wt", "cyl")], upper = "blank", title = "Custom Example")
#' # ggplot example taken from example(geom_text)
#'   plot <- ggplot2::ggplot(mtcars, ggplot2::aes(x=wt, y=mpg, label=rownames(mtcars)))
#'   plot <- plot +
#'     ggplot2::geom_text(ggplot2::aes(colour=factor(cyl)), size = 3) +
#'     ggplot2::scale_colour_discrete(l=40)
#' custom_car[1, 2] <- plot
#' personal_plot <- ggally_text(
#'   "ggpairs allows you\nto put in your\nown plot.\nLike that one.\n <---"
#' )
#' custom_car[1, 3] <- personal_plot
#' # custom_car
putPlot <- function(x, value, i, j){
  pos <- get_pos(x, i, j)
  if (mode(value) == "character") {
    if (value == "blank") {
      x$plots[[pos]] <- ggally_blank()
    } else {
      stop("character values (besides 'blank') are not allowed to be stored as plot values.")
    }
  } else {
    x$plots[[pos]] <- value
  }

  if (x$printInfo) {
    cat("\n\nDone placing plot: ", pos, "\n")
  }

  x
}

#' getPlot
#'
#' Retrieves the ggplot object at the desired location.
#'
#' @param x ggpair object to select from
#' @param i row from the top
#' @param j column from the left
#' @keywords hplot
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @importFrom utils capture.output
#' @export
#' @examples
#'  data(tips, package = "reshape")
#'  plotMatrix2 <- ggpairs(tips[, 3:2], upper = list(combo = "denstrip"))
#'  plotMatrix2[1, 2]
getPlot <- function(x, i, j){
  if (x$printInfo) {
    cat("i: ", i, " j: ", j, "\n")
  }

  pos <- get_pos(x, i, j)

  if (x$printInfo) {
    cat("Plot List Spot: ", pos, "\n")
  }

  # if (pos > length(x$plots)) {
  #   plot_text <- "blank"
  # } else {
  #   plot_text <- x$plots[[pos]]
  # }
  if (pos > length(x$plots)) {
    plotObj <- NULL
  } else {
    plotObj <- x$plots[[pos]]
  }

  if (is.null(plotObj) || identical(plotObj, "blank")) {
    p <- ggally_blank()
  } else {
    if (ggplot2::is.ggplot(plotObj)) {
      p <- plotObj
    } else if (inherits(plotObj, "ggmatrix_plot_obj")) {

      fn <- plotObj$fn
      p <- fn(x$data, plotObj$mapping)

    } else {
      firstNote <- str_c("Position: i = ", i, ", j = ", j, "\nstr(plotObj):\n", sep = "")
      strObj <- capture.output({
        print(str(plotObj))
      })
      stop(str_c("unknown plot object type.\n", firstNote, strObj))
    }

    if (!is.null(x$gg)) {
      # print("adding custom gg")
      p <- p + x$gg
    }
  }
  # stop("fix this")
  # if (is.character(plot_text)) {
  #   if (plot_text != "blank") {
  #     p <- eval_ggpair(plot_text, x$data)
  #     if (! is.null(x$gg)) {
  #       p <- p + x$gg
  #     }
  #     # attributes( p)$class <- "ggplot"
  #   } else {
  #     p <- ggally_blank()
  #   }
  # } else {
  #   p <- plot_text
  # }

  if (x$printInfo || x$verbose) {
    cat("Plot #", pos)
    if (is_blank_plot(p)) {
      cat(" - Blank")
    }
    cat("\n")
  }

  p
}


get_pos <- function(x, i, j) {
  if (!identical(x$byrow, TRUE)) {
    pos <- i + (x$nrow * (j - 1))
  } else {
    pos <- j + (x$ncol * (i - 1))
  }
  pos
}


check_i_j <- function(i, j) {
  if ( (length(i) > 1) || (mode(i) != "numeric")) {
    stop("'i' may only be a single numeric value")
  }
  if ( (length(j) > 1) || (mode(j) != "numeric")) {
    stop("'j' may only be a single numeric value")
  }
  NULL
}

#' @rdname getPlot
#' @usage \method{[}{ggmatrix}(x, i, j, ...)
#' @param ... ignored
#' @export
`[.ggmatrix` <- function(x, i, j, ...) {
  check_i_j(i, j)

  getPlot(x, i, j)
}

#' @rdname putPlot
#' @usage \method{[}{ggmatrix}(x, i, j, ...) <- value
#' @param ... ignored
#' @export
`[<-.ggmatrix` <- function(x, i, j, ..., value) {
  # x = matrix
  # i = first subset
  # j = second subset
  # y = value
  check_i_j(i, j)

  xNew <- putPlot(x, value, i, j)
  xNew
}
