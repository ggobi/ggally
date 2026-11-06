make_ggmatrix_plot_obj <- function(
  fn,
  mapping = ggplot2::aes(),
  dataPos = 1,
  gg = NULL
) {
  # nonCallVals <- which(lapply(mapping, mode) == "call")
  # if (length(nonCallVals) > 0) {
  #   nonCallNames <- names(mapping)[nonCallVals]
  #   browser()
  #   stop(
  #     paste(
  #       "variables: ",
  #       paste(shQuote(nonCallNames, type = "cmd"), sep = ", "),
  #       " have non standard format: ",
  #       paste(shQuote(unlist(mapping[nonCallVals]), type = "cmd"), collapse = ", "),
  #       ".  Please rename the columns or make a new column.",
  #       sep = ""
  #     )
  #   )
  # }

  ret <- list(
    fn = fn,
    mapping = mapping,
    dataPos = dataPos,
    gg = gg
  )
  class(ret) <- "ggmatrix_plot_obj"
  ret
}


blank_plot_string <- function() {
  "PM; (blank)"
}

mapping_as_string <- function(mapping) {
  str_c(
    "c(",
    str_c(names(mapping), as.character(mapping), sep = " = ", collapse = ", "),
    ")"
  )
}

#' @export
as.character.ggmatrix_plot_obj <- function(x, ...) {
  hasGg <- (!is.null(x$gg))
  mappingTxt <- mapping_as_string(x$mapping)
  fnTxt <- ifelse(
    inherits(x$fn, "ggmatrix_fn_with_params"),
    as.character(x$fn),
    "custom_function"
  )
  if (inherits(x$fn, "ggmatrix_fn_with_params")) {
    if (attr(x$fn, "name") %in% c("ggally_blank", "ggally_blankDiag")) {
      return(blank_plot_string())
    }
  }
  str_c(
    "PM",
    "; aes: ",
    mappingTxt,
    "; fn: {",
    fnTxt,
    "}",
    # "; dataPos: ", x$dataPos,
    "; gg: ",
    as.character(hasGg)
  )
}


#' \code{\link{ggmatrix}} structure
#'
#' View the condensed version of the \code{\link{ggmatrix}} object. The attribute "class" is ALWAYS altered to "_class" to avoid recursion.
#'
#' @details
#' Arguments:
#' \describe{
#'   \item{\code{object}}{\code{\link{ggmatrix}} object to be viewed}
#'   \item{\code{...}}{passed on to the default \code{str} method}
#'   \item{\code{raw}}{boolean to determine if the plots should be converted to text or kept as original objects}
#' }
#' @importFrom utils str
#' @name str.ggmatrix
#' @usage NULL
method(str, ggmatrix) <- function(object, ..., raw = FALSE) {
  if (isTRUE(raw)) {
    # S7's str method
    NextMethod()
    return()
  }

  matched_call <- rlang::call_match(
    sys.call(),
    function(object, ..., raw = FALSE) {}
  )
  objName <- rlang::call_args(matched_call)$object

  obj <- convert(object, class_list)
  cat(str_c(
    "\nCustom str.ggmatrix output: \nTo view original object use ",
    "'str(",
    objName,
    ", raw = TRUE)'\n\n"
  ))
  obj$plots <- lapply(obj$plots, function(plotObj) {
    if (ggplot2::is_ggplot(plotObj)) {
      str_c(
        "PM; ggplot2 object; mapping: ",
        mapping_as_string(plotObj$mapping)
      )
    } else if (inherits(plotObj, "ggmatrix_plot_obj")) {
      as.character(plotObj)
    } else {
      plotObj
    }
  })
  attr(obj, "_class") <- attr(obj, "class")
  class(obj) <- NULL
  str(obj, ...)
}
