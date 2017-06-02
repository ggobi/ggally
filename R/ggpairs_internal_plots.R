
#' Wrap a function with different parameter values
#'
#' Wraps a function with the supplied parameters to force different default behavior.  This is useful for functions that are supplied to ggpairs.  It allows you to change the behavior of one function, rather than creating multiple functions with different parameter settings.
#'
#' \code{wrap} is identical to \code{wrap_fn_with_params}.  These function take the new parameters as arguments.
#'
#' \code{wrapp} is identical to \code{wrap_fn_with_param_arg}.  These functions take the new parameters as a single list.
#'
#' The \code{params} and \code{fn} attributes are there for debugging purposes.  If either attribute is altered, the function must be re-wrapped to have the changes take effect.
#'
#' @param funcVal function that the \code{params} will be applied to.  The function should follow the api of \code{function(data, mapping, ...)\{\}}. \code{funcVal} is allowed to be a string of one of the \code{ggally_NAME} functions, such as \code{"points"} for \code{ggally_points} or \code{"facetdensity"} for \code{ggally_facetdensity}.
#' @param ... named parameters to be supplied to \code{wrap_fn_with_param_arg}
#' @param params named vector or list of parameters to be applied to the \code{funcVal}
#' @param funcArgName name of function to be displayed
#' @return a \code{function(data, mapping, ...)\{\}} that will wrap the original function with the parameters applied as arguments
#' @export
#' @rdname wrap
#' @examples
#'  # small function to display plots only if it's interactive
#'  p_ <- GGally::print_if_interactive
#'
#' # example function that prints 'val'
#' fn <- function(data, mapping, val = 2) {
#'   print(val)
#' }
#' fn(data = NULL, mapping = NULL) # 2
#'
#' # wrap function to change default value 'val' to 5 instead of 2
#' wrapped_fn1 <- wrap(fn, val = 5)
#' wrapped_fn1(data = NULL, mapping = NULL) # 5
#' # you may still supply regular values
#' wrapped_fn1(data = NULL, mapping = NULL, val = 3) # 3
#'
#' # wrap function to change 'val' to 5 using the arg list
#' wrapped_fn2 <- wrap_fn_with_param_arg(fn, params = list(val = 5))
#' wrapped_fn2(data = NULL, mapping = NULL) # 5
#'
#' # change parameter settings in ggpairs for a particular function
#' ## Goal output:
#' regularPlot <- ggally_points(
#'   iris,
#'   ggplot2::aes(Sepal.Length, Sepal.Width),
#'   size = 5, color = "red"
#' )
#' p_(regularPlot)
#'
#' # Wrap ggally_points to have parameter values size = 5 and color = 'red'
#' w_ggally_points <- wrap(ggally_points, size = 5, color = "red")
#' wrappedPlot <- w_ggally_points(
#'   iris,
#'   ggplot2::aes(Sepal.Length, Sepal.Width)
#' )
#' p_(wrappedPlot)
#'
#' # Double check the aes parameters are the same for the geom_point layer
#' identical(regularPlot$layers[[1]]$aes_params, wrappedPlot$layers[[1]]$aes_params)
#'
#' # Use a wrapped function in ggpairs
#' pm <- ggpairs(iris, 1:3, lower = list(continuous = wrap(ggally_points, size = 5, color = "red")))
#' p_(pm)
#' pm <- ggpairs(iris, 1:3, lower = list(continuous = w_ggally_points))
#' p_(pm)
wrap_fn_with_param_arg <- function(
  funcVal,
  params = NULL,
  funcArgName = deparse(substitute(funcVal))
) {

  if (missing(funcArgName)) {
    fnName <- attr(funcVal, "name")
    if (!is.null(fnName)) {
      funcArgName <- fnName
    }
  }

  if (!is.null(params)) {
    if (is.vector(params)) {
      params <- as.list(params)
    }

    if (length(params) > 0) {
      if (!is.list(params)) {
        stop("'params' must be a named list, named vector, or NULL")
      }
      if (is.null(names(params))) {
        stop("'params' must be a named list, named vector, or NULL")
      }
      if (any(nchar(names(params)) == 0)) {
        stop("'params' must be a named list, named vector, or NULL")
      }
    }
  }

  if (mode(funcVal) == "character") {

    if (missing(funcArgName)) {
      funcArgName <- str_c("ggally_", funcVal)
    }

    tryCatch({
        funcVal <- get(
          str_c("ggally_", funcVal),
          mode = "function",
          envir = loadNamespace("GGally")
        )
      },
      error = function(e) {
        stop(str_c(
"The following ggpairs plot functions are readily available: \n",
"\tcontinuous: c('points', 'smooth', 'smooth_loess', 'density', 'cor', 'blank')\n",
"\tcombo: c('box', 'box_no_facet', 'dot', 'dot_no_facet', 'facethist',",
  " 'facetdensity', 'denstrip', 'blank')\n",
"\tdiscrete: c('ratio', 'facetbar', 'blank')\n",
"\tna: c('na', 'blank')\n",
"\n",
"\tdiag continuous: c('densityDiag', 'barDiag', 'blankDiag')\n",
"\tdiag discrete: c('barDiag', 'blankDiag')\n",
"\tdiag na: c('naDiag', 'blankDiag')\n",
"\n",
"You may also provide your own function that follows the api of ",
  "function(data, mapping, ...){ . . . }\nand returns a ggplot2 plot object\n",
  "\tEx:\n",
  "\tmy_fn <- function(data, mapping, ...){\n",
  "\t  p <- ggplot(data = data, mapping = mapping) + \n",
  "\t    geom_point(...)\n",
  "\t  p\n",
  "\t}\n",
  "\tggpairs(data, lower = list(continuous = my_fn))\n",
"\n",
"Function provided: ", funcVal
        ))
      }
    )
  }


  allParams <- ifnull(attr(funcVal, "params"), list())
  allParams[names(params)] <- params

  original_fn <- funcVal

  ret_fn <- function(data, mapping, ...) {
    allParams$data <- data
    allParams$mapping <- mapping
    argsList <- list(...)
    allParams[names(argsList)] <- argsList
    do.call(original_fn, allParams)
  }

  class(ret_fn) <- "ggmatrix_fn_with_params"
  attr(ret_fn, "name") <- as.character(funcArgName)
  attr(ret_fn, "params") <- allParams
  attr(ret_fn, "fn") <- original_fn
  ret_fn
}

#' @export
#' @rdname wrap
wrapp <- wrap_fn_with_param_arg

#' @export
#' @rdname wrap
wrap  <- function(funcVal, ..., funcArgName = deparse(substitute(funcVal))) {
  if (missing(funcArgName)) {
    fnName <- attr(funcVal, "name")
    if (!is.null(fnName)) {
      funcArgName <- fnName
    } else if (is.character(funcVal)) {
      funcArgName <- str_c("ggally_", funcVal)
    }
  }

  params <- list(...)
  if (length(params) > 0) {
    if (is.null(names(params))) {
      stop("all parameters must be named arguments")
    }
    if (any(nchar(names(params)) == 0)) {
      stop("all parameters must be named arguments")
    }
  }
  wrap_fn_with_param_arg(funcVal, params = params, funcArgName = funcArgName)
}
#' @export
#' @rdname wrap
wrap_fn_with_params <- wrap


as.character.ggmatrix_fn_with_params <- function(x, ...) {
  params <- attr(x, "params")
  fnName <- attr(x, "name")

  if (length(params) == 0) {
    txt <- str_c("wrap: '", fnName, "'")
  } else {
    txt <- str_c("wrap: '", attr(x, "name"), "'; params: ", mapping_as_string(params))
  }

  txt
}






make_ggmatrix_plot_obj <- function(fn, mapping = ggplot2::aes(), dataPos = 1, gg = NULL) {
  nonCallVals <- which(lapply(mapping, mode) == "call")
  if (length(nonCallVals) > 0) {
    nonCallNames <- names(mapping)[nonCallVals]
    stop(
      paste(
        "variables: ",
        paste(shQuote(nonCallNames, type = "cmd"), sep = ", "),
        " have non standard format: ",
        paste(shQuote(unlist(mapping[nonCallVals]), type = "cmd"), collapse = ", "),
        ".  Please rename the columns or make a new column.",
        sep = ""
      )
    )
  }

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
  str_c("c(", str_c(names(mapping), as.character(mapping), sep = " = ", collapse = ", "), ")")
}

as.character.ggmatrix_plot_obj <- function(x, ...) {
  hasGg <- (!is.null(x$gg))
  mappingTxt <- mapping_as_string(x$mapping)
  fnTxt <- ifelse(inherits(x$fn, "ggmatrix_fn_with_params"), as.character(x$fn), "custom_function")
  if (inherits(x$fn, "ggmatrix_fn_with_params")) {
    if (attr(x$fn, "name") %in% c("ggally_blank", "ggally_blankDiag")) {
      return(blank_plot_string())
    }
  }
  str_c(
    "PM",
    "; aes: ", mappingTxt,
    "; fn: {", fnTxt, "}",
    # "; dataPos: ", x$dataPos,
    "; gg: ", as.character(hasGg)
  )
}



#' ggmatrix structure
#'
#' View the condensed version of the ggmatrix object. The attribute "class" is ALWAYS altered to "_class" to avoid recursion.
#'
#' @param object ggmatrix object to be viewed
#' @param ... passed on to the default str method
#' @param raw boolean to determine if the plots should be converted to text or kept as original objects
#' @method str ggmatrix
#' @importFrom utils str
#' @export
str.ggmatrix <- function(object, ..., raw = FALSE) {
  objName <- deparse(substitute(object))
  obj <- object
  if (identical(raw, FALSE)) {
    cat(str_c(
      "\nCustom str.ggmatrix output: \nTo view original object use ",
      "'str(", objName, ", raw = TRUE)'\n\n"
    ))
    obj$plots <- lapply(obj$plots, function(plotObj) {
      if (ggplot2::is.ggplot(plotObj)) {
        str_c("PM; ggplot2 object; mapping: ", mapping_as_string(plotObj$mapping))
      } else if (inherits(plotObj, "ggmatrix_plot_obj")) {
        as.character(plotObj)
      } else {
        plotObj
      }
    })
  }
  attr(obj, "_class") <- attr(obj, "class")
  class(obj) <- NULL
  str(obj, ...)
}
