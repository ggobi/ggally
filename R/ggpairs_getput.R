#' Put Plot
#'
#' Function to place your own plot in the layout.
#'
#' @param pm ggally object to be altered
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
#'
#' # remove plots after creating a plot matrix
#' custom_car[2,1] <- NULL
#' custom_car[3,1] <- "blank" # the same as storing null
#' custom_car[3,2] <- NULL
#' custom_car
putPlot <- function(pm, value, i, j){
  pos <- get_pos(pm, i, j)
  if (is.null(value)) {
    pm$plots[[pos]] <- make_ggmatrix_plot_obj(wrap("blank", funcArgName = "ggally_blank"))
  } else if (mode(value) == "character") {
    if (value == "blank") {
      pm$plots[[pos]] <- make_ggmatrix_plot_obj(wrap("blank", funcArgName = "ggally_blank"))
    } else {
      stop("character values (besides 'blank') are not allowed to be stored as plot values.")
    }
  } else {
    pm$plots[[pos]] <- value
  }

  pm
}

#' getPlot
#'
#' Retrieves the ggplot object at the desired location.
#'
#' @param pm ggmatrix object to select from
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
getPlot <- function(pm, i, j){
  if (FALSE) {
    cat("i: ", i, " j: ", j, "\n")
  }

  pos <- get_pos(pm, i, j)

  if (pos > length(pm$plots)) {
    plotObj <- NULL
  } else {
    plotObj <- pm$plots[[pos]]
  }

  if (is.null(plotObj)) {
    p <- ggally_blank()
  } else {
    if (ggplot2::is.ggplot(plotObj)) {
      p <- plotObj

    } else if (inherits(plotObj, "ggmatrix_plot_obj")) {
      fn <- plotObj$fn
      p <- fn(pm$data, plotObj$mapping)

    } else if (inherits(plotObj, "legend_guide_box")) {
      p <- plotObj

    } else {
      firstNote <- str_c("Position: i = ", i, ", j = ", j, "\nstr(plotObj):\n", sep = "")
      strObj <- capture.output({
        str(plotObj)
      })
      stop(str_c("unknown plot object type.\n", firstNote, strObj))
    }

    p <- add_gg_info(p, pm$gg)
  }

  p
}


get_pos <- function(pm, i, j) {
  if (isTRUE(pm$byrow)) {
    pos <- j + (pm$ncol * (i - 1))
  } else {
    pos <- i + (pm$nrow * (j - 1))
  }
  pos
}

get_pos_rev <- function(pm, pos) {
  if (isTRUE(pm$byrow)) {
    i <- ceiling(pos / pm$ncol)
    j <- (pos - 1) %% pm$ncol + 1
  } else {
    i <- (pos - 1) %% pm$nrow + 1
    j <- ceiling(pos / pm$nrow)
  }
  c(i, j)
}


check_i_j <- function(pm, i, j) {
  if ( (length(i) > 1) || (mode(i) != "numeric")) {
    stop("'i' may only be a single numeric value")
  }
  if ( (length(j) > 1) || (mode(j) != "numeric")) {
    stop("'j' may only be a single numeric value")
  }

  if (i > pm$nrow || i < 1) {
    stop("'i' may only be in the range from 1:", pm$nrow)
  }
  if (j > pm$ncol || j < 1) {
    stop("'j' may only be in the range from 1:", pm$ncol)
  }

  invisible()
}

#' @rdname getPlot
#' @usage \method{[}{ggmatrix}(pm, i, j, ...)
#' @param ... ignored
#' @export
`[.ggmatrix` <- function(pm, i, j, ...) {
  # print(list(x = i, y = j))
  check_i_j(pm, i, j)

  getPlot(pm, i, j)
}

#' @rdname putPlot
#' @usage \method{[}{ggmatrix}(pm, i, j, ...) <- value
#' @param ... ignored
#' @export
`[<-.ggmatrix` <- function(pm, i, j, ..., value) {
  # x = matrix
  # i = first subset
  # j = second subset
  # y = value
  check_i_j(pm, i, j)

  putPlot(pm, value, i, j)
}
