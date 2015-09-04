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

print.ggmatrix_fn_with_params <- function(x, ...) {
  params <- x$params
  if (length(params) > 0) {
    paramTxt <- str_c("params = c(",
      stringr::str_c(names(params), unlist(params), sep = " = ", collapse = ", "),
    ")")
  } else {
    paramTxt <- "(no params)"
  }
  txt <- stringr::str_c("wrapper fn; fn: ", x$fnName, "; with params: ", paramTxt)
  print(txt)
}













ggpairs_ggplot2_internal_plot <- function(p) {
  class(p) <- unique(c("ggmatrix_ggplot2", class(p)))
  p
}
print.ggmatrix_ggplot2 <- function(x, ...) {
  print("ggmatrix plot; ggplot2 object")
}








make_ggpair_plot_obj <- function(fn, mapping, dataPos = 1, gg = NULL) {
  ret <- list(
    fn = fn,
    mapping = mapping,
    dataPos = dataPos,
    gg = gg
  )
  class(ret) <- "ggpair_plot_obj"
  ret
}

print.ggpair_plot_obj <- function(x, ...) {
  hasGg <- (!is.null(x$gg))
  mappingTxt = str_c(names(x$mapping), as.character(x$mapping), sep = " = ", collapse = ", ")
  print(str_c(
    "ggmatrix plot; dataPos: ", x$dataPos,
    "; mapping: ", mappingTxt,
    "; gg: ", as.character(hasGg)
    ))
}
