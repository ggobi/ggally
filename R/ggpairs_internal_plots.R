
#' Wrap a function with different parameter values
#'
#' Wraps a function with the supplied parameters to force different behavior.  This is useful for functions that are supplied to ggpairs.  It allows you to change the behavior of one function, rather than creating multiple functions with different parameter settings.
#'
#' \code{wrap} is identical to \code{wrap_fn_with_params}.  These function take the new parameters as arguements.
#'
#' \code{wrapp} is identical to \code{wrap_fn_with_param_arg}.  These functions take the new parameters as a single list.
#'
#' @param funcVal function that the \code{params} will be applied to.  The function should follow the api of \code{function(data, mapping, ...)\{\}}
#' @param ... named parameters to be supplied to \code{wrap_fn_with_param_arg}
#' @param params named vector of parameters to be applied to the \code{funcVal}
#' @param funcArgName name of function to be displayed
#' @return a \code{function(data, mapping, ...)\{\}} that will wrap the original function with the parameters applied as arguements
#' @export
#' @rdname wrap
#' @examples
#' # example function that prints 'val'
#' fn <- function(data, mapping, val = 2) {
#'   print(val)
#' }
#' fn(NULL, NULL) # 2
#'
#' # wrap function to change 'val' to 5 instead of 2
#' wrapped_fn1 <- wrap(fn, val = 5)
#' wrapped_fn1(NULL, NULL) # 5
#'
#' # wrap function to change 'val' to 5 using the arg list
#' wrapped_fn2 <- wrap_fn_with_param_arg(fn, params = list(val = 5))
#' wrapped_fn2(NULL, NULL) # 5
#'
#' # change parameter settings in ggpairs for a particular function
#' ## Goal output:
#' (regularPlot <- ggally_points(
#'   iris,
#'   ggplot2::aes(Sepal.Length, Sepal.Width),
#'   size = 5, color = "red"
#' ))
#' # Wrap ggally_points to have parameter values size = 5 and color = 'red'
#' w_ggally_points <- wrap(ggally_points, size = 5, color = "red")
#' (wrappedPlot <- w_ggally_points(
#'   iris,
#'   ggplot2::aes(Sepal.Length, Sepal.Width)
#' ))
#'
#' # Double check the aes parameters are the same for the geom_point layer
#' identical(regularPlot$layers[[1]]$aes_params, wrappedPlot$layers[[1]]$aes_params)
#'
#' # Use a wrapped function in ggpairs
#' ggpairs(iris, 1:3, lower = list(continuous = wrap(ggally_points, size = 5, color = "red")))
#' ggpairs(iris, 1:3, lower = list(continuous = w_ggally_points))
wrap_fn_with_param_arg <- function(
  funcVal,
  params = NULL,
  funcArgName = substitute(funcVal)
) {

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

  if (inherits(funcVal, "ggmatrix_fn_with_params")) {
    fnName <- attr(funcVal, "fnName")
    oParams <- params
    params <- attr(funcVal, "params")

    if (length(oParams) > 0) {
      params[names(oParams)] <- oParams
    }
    fn <- attr(funcVal, "original_fn")

  } else if (mode(funcVal) == "character") {

    fnName <- str_c("ggally_", funcVal)
    tryCatch({
        fn <- get(fnName, mode = "function")
      },
      error = function(e) {
        stop(str_c(
          "The following ggpair plot functions are readily available: \n",
          "\tcontinuous: c('points', 'smooth', 'density', 'cor', 'blank')\n",
          "\tcombo: c('box', 'dot', 'facethist', 'facetdensity', 'denstrip', 'blank')\n",
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


  } else if (mode(funcVal) == "function") {
    fnName <- as.character(funcArgName)
    if (identical(fnName, "subType")) {
      fnName <- "custom_function"
    }
    fn <- funcVal
  }

  if (length(params) > 0) {
    ret_fn <- function(data, mapping, ...) {
      argsList <- list(...)

      argsList$data <- data
      argsList$mapping <- mapping

      for (pName in names(params)) {
        argsList[[pName]] <- params[[pName]]
      }
      # print(params);
      # print("")
      # print(argsList)
      do.call(fn, argsList)
    }
  } else {
    ret_fn <- fn
  }

  attr(ret_fn, "params") <- params
  attr(ret_fn, "original_fn") <- fn
  attr(ret_fn, "fnName") <- fnName
  class(ret_fn) <- "ggmatrix_fn_with_params"
  ret_fn
}

#' @export
#' @rdname wrap
wrapp <- wrap_fn_with_param_arg

#' @export
#' @rdname wrap
wrap  <- function(funcVal, ..., funcArgName = substitute(funcVal)) {
  params <- list(...)
  if (length(params) > 0) {
    if (is.null(names(params))) {
      stop("all parameters must be named arguements")
    }
    if (any(nchar(names(params)) == 0)) {
      stop("all parameters must be named arguements")
    }
  }
  wrap_fn_with_param_arg(funcVal, params = params, funcArgName = funcArgName)
}
#' @export
#' @rdname wrap
wrap_fn_with_params <- wrap


as.character.ggmatrix_fn_with_params <- function(x, ...) {
  params <- attr(x, "params")
  paramTxt <- mapping_as_string(params)
  txt <- str_c("wrap; fn: '", attr(x, "fnName"), "'; with params: ", paramTxt)
  txt
}













ggpairs_ggplot2_internal_plot <- function(p) {
  class(p) <- unique(c("ggmatrix_ggplot2", class(p)))
  p
}
as.character.ggmatrix_ggplot2 <- function(x, ...) {
  "PM; ggplot2 object"
}




make_ggmatrix_plot_obj <- function(fn, mapping, dataPos = 1, gg = NULL) {
  nonCallVals <- which(lapply(mapping, mode) == "call")
  if (length(nonCallVals) > 0) {
    nonCallNames <- names(mapping)[nonCallVals]
    stop(
      paste(
        "variables: ",
        paste(shQuote(nonCallNames), sep = ", "),
        " have non standard format: ",
        paste(shQuote(unlist(mapping[nonCallVals])), collapse = ", "),
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


mapping_as_string <- function(mapping) {
  str_c("c(", str_c(names(mapping), as.character(mapping), sep = " = ", collapse = ", "), ")")
}

as.character.ggmatrix_plot_obj <- function(x, ...) {
  hasGg <- (!is.null(x$gg))
  mappingTxt <- mapping_as_string(x$mapping)
  fnTxt <- ifelse(inherits(x$fn, "ggmatrix_fn_with_params"), as.character(x$fn), "custom_function")
  if (inherits(x$fn, "ggmatrix_fn_with_params")) {
    if (attr(x$fn, "fnName") %in% c("ggally_blank", "ggally_blankDiag")) {
      return("(blank)")
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
  objName <- as.character(substitute(object))
  obj <- object
  if (identical(raw, FALSE)) {
    cat(str_c(
      "\nCustom str.ggmatrix output: \nTo view original object use ",
      "'str(", objName, ", raw = TRUE)'\n\n"
    ))
    obj$plots <- lapply(obj$plots, function(plotObj) {
      if (is_blank_plot(plotObj)) {
        "(blank)"
      } else if (ggplot2::is.ggplot(plotObj)) {
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
