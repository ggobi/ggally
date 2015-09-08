#' @export
wrap_fn_with_param_arg <- function(funcVal, params = NULL) {
  ret <- list()
  funcArgName <- substitute(funcVal)

  if (mode(funcVal) == "character") {
    ret$fnName <- str_c("ggally_", funcVal)
    fn <- get(ret$fnName, mode = "function")

  } else if (mode(funcVal) == "function") {
    ret$fnName <- as.character(funcArgName)
    fn <- funcVal

  }

  ret$original_fn <- fn
  if (length(params) > 0) {
    ret$fn <- function(data, mapping, ...) {
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
    ret$fn <- fn
  }

  ret$params <- params
  class(ret) <- "ggmatrix_fn_with_params"
  ret

}
#' @export
wrap_fn_with_params <- function(funcVal, ...) {
  wrap_fn_with_param_arg(funcVal, params = list(...))
}

as.character.ggmatrix_fn_with_params <- function(x, ...) {
  params <- x$params
  paramTxt <- mapping_as_string(params)
  txt <- stringr::str_c("wrapper fn; fn: ", x$fnName, "; with params: ", paramTxt)
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
