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
wrap_fn_with_param_arg <- function(funcVal, params = NULL) {
  funcArgName <- substitute(funcVal)

  if (mode(funcVal) == "character") {
    fnName <- str_c("ggally_", funcVal)
    fn <- get(fnName, mode = "function")

  } else if (mode(funcVal) == "function") {
    fnName <- as.character(funcArgName)
    fn <- funcVal
  }

  original_fn <- fn
  if (length(params) > 0) {
    ret_fn <- function(data, mapping, ...) {
      argsList <- list(...)

      argsList$data = data
      argsList$mapping = mapping

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
#' @param ... named parameters to be supplied to \code{wrap_fn_with_param_arg}
wrap_fn_with_params <- function(funcVal, ...) {
  wrap_fn_with_param_arg(funcVal, params = list(...))
}

#' @export
wrapp <- wrap_fn_with_param_arg
#' @export
wrap <- wrap_fn_with_params


as.character.ggmatrix_fn_with_params <- function(x, ...) {
  params <- attr(x, "params")
  paramTxt <- mapping_as_string(params)
  txt <- stringr::str_c("wrapper fn; fn: ", attr(x, "fnName"), "; with params: ", paramTxt)
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
  str_c("c(",str_c(names(mapping), as.character(mapping), sep = " = ", collapse = ", "), ")")
}

as.character.ggmatrix_plot_obj <- function(x, ...) {
  hasGg <- (!is.null(x$gg))
  mappingTxt = mapping_as_string(x$mapping)
  fnTxt <- ifelse(inherits(x$fn, "ggmatrix_fn_with_params"), as.character(x$fn), "custom_function")
  str_c(
    "PM",
    "; aes: ", mappingTxt,
    "; fn: {", fnTxt, "}",
    # "; dataPos: ", x$dataPos,
    "; gg: ", as.character(hasGg)
  )
}



#' @export
str.ggmatrix <- function(object, ...) {
  obj <- object
  obj$plots <- lapply(obj$plots, function(plotObj) {
    if (ggplot2::is.ggplot(plotObj)) {
      str_c("ggmatrix plot; ggplot2 object; mapping: ", mapping_as_string(plotObj$mapping))
    } else if (inherits(plotObj, "ggmatrix_plot_obj")) {
      as.character(plotObj)
    } else {
      plotObj
    }
  })
  attr(obj, "_class") <- attr(obj, "class")
  class(obj) <- NULL
  str(obj)
}
