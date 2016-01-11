#' @export
#' @rdname wrap_fn_with_param_arg
wrap  <- function(funcVal, ..., funcArgName = substitute(funcVal)) {
  wrap_fn_with_param_arg(funcVal, params = list(...), funcArgName = funcArgName)
}

#' @export
#' @rdname wrap_fn_with_param_arg
#' @param ... named parameters to be supplied to \code{wrap_fn_with_param_arg}
wrap_fn_with_params <- function(funcVal, ..., funcArgName = substitute(funcVal)) {
  wrap_fn_with_param_arg(funcVal, params = list(...), funcArgName = funcArgName)
}


#' Wrap a function with parameters
#'
#' Wraps a function with the given parameters.  This allows for very specific parameter arguements to be applied to each specific function.
#'
#' \code{wrap == wrap_fn_with_params}
#'
#' \code{wrapp == wrap_fn_with_param_arg}
#'
#' @param funcVal function that the \code{params} will be applied to.  The function should follow the api of \code{function(data, mapping, ...)\{\}}
#' @param params named vector of parameters to be applied to the \code{funcVal}
#' @param funcArgName name of function to be displayed
#' @return a \code{function(data, mapping, ...)\{\}} that will wrap the original function with the parameters applied as arguements
#' @export
#' @rdname wrap_fn_with_param_arg
#' @examples
#' fn <- function(data, mapping, val = 2) {
#'   print(val)
#' }
#' fn(NULL, NULL) # 2
#' wrapped_fn <- wrap_fn_with_param_arg(fn, params = c(val = 5))
#' wrapped_fn(NULL, NULL) # 5
wrap_fn_with_param_arg <- function(funcVal, params = NULL, funcArgName = substitute(funcVal)) {

  if (inherits(funcVal, "ggmatrix_fn_with_params")) {
    fnName <- attr(funcVal, "fnName")
    oParams <- params
    params <- attr(funcVal, "params")
    for (paramName in names(oParams)) {
      params[paramName] <- oParams[paramName]
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

  original_fn <- fn
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
  attr(ret_fn, "original_fn") <- original_fn
  attr(ret_fn, "fnName") <- fnName
  class(ret_fn) <- "ggmatrix_fn_with_params"
  ret_fn
}

#' @export
#' @rdname wrap_fn_with_param_arg
wrapp <- wrap_fn_with_param_arg


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
