#' Plots the Histogram Plots by Using Diagonal
#'
#' Plots the Histogram by using Diagonal.
#'
#' @param data data set using
#' @param mapping aesthetics being used.
#' @param ... other arguments sent to geom_histogram
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords hplot
#' @export
#' @examples
#'  ggally_histogramDiag(tips, mapping = ggplot2::aes(x = total_bill))
#'  #data(movies)
#'  #ggally_histogramDiag(movies, mapping = ggplot2::aes_string(x="rating"))
#'  #ggally_histogramDiag(movies, mapping = ggplot2::aes_string(x="rating", color = "mpaa"))
ggally_histogramDiag = function (data, mapping, ...) {
  p <- ggplot(data, mapping) + 
    scale_x_continuous() + 
    scale_y_continuous() + 
    geom_histogram(
      aes(y = ..density.. / max(..density..) * 
            diff(range(x, na.rm = TRUE))), 
      ...)
  p$type <- "diag"
  p$subType <- "density"
  p
}
