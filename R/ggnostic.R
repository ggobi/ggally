
## .hat: Diagonal of the hat matrix
## .sigma: Estimate of residual standard deviation when corresponding observation is dropped from model
## .fitted: Fitted values of model
## .cooksd: Cooks distance, 'cooks.distance'
## .se.fit: Standard errors of fitted values
## .resid: Residuals
## .std.resid: Standardised residuals (Some unusual "lm" objects, such as "rlm" from MASS, may omit '.cooksd' and '.std.resid'. "gam" from mgcv omits '.sigma')

#' Broomify a model
#'
#' broom::augment a model and add broom::glance and broom::tidy output as attributes. X and Y variables are also added.
#'
#' @param model model to be sent to \code{broom::\link[broom]{augment}}, \code{broom::\link[broom]{glance}}, and \code{broom::\link[broom]{tidy}}
#' @return broom::augmented data frame with the broom::glance data.frame and broom::tidy data.frame as 'broom_glance' and 'broom_tidy' attributes respectively.  \code{var_x} and \code{var_y} variables are also added as attributes
#' @export
broomify <- function(model) {
  require_pkgs("broom")

  broom_glance_info  <- broom::glance(model)
  broom_tidy_coef    <- broom::tidy(model)
  broom_augment_rows <- broom::augment(model)
  attr(broom_augment_rows, "broom_glance") <- broom_glance_info
  attr(broom_augment_rows, "broom_tidy") <- broom_tidy_coef
  attr(broom_augment_rows, "var_x") <- model_beta_variables(model)
  attr(broom_augment_rows, "var_y") <- model_response_variables(model)
  attr(broom_augment_rows, "var_x_label") <- model_beta_label(model)

  return(broom_augment_rows)
}

#' Model term names
#'
#' Retrieve either the response variable names, the beta variable names, or beta variable names with signifigance stars.
#'
#' @param model model in question
#' @return character vector of names
#' @rdname model_terms
#' @export
#' @importFrom stats terms
model_response_variables <- function(model) {
  model_terms <- terms(model)

  model_variables <- attr(model_terms, "variables")
  # convert to character vector
  model_variables <- as.character(model_variables)[-1]

  model_response_pos <- attr(model_terms, "response")

  model_variables[model_response_pos]
}
#' @rdname model_terms
#' @export
model_beta_variables <- function(model) {
  model_terms <- terms(model)

  model_variables <- attr(model_terms, "variables")
  # convert to character vector
  model_variables <- as.character(model_variables)[-1]

  model_response_pos <- attr(model_terms, "response")

  model_variables[- model_response_pos]
}


#' @importFrom stats symnum
beta_stars <- function(p_val) {
  unclass(symnum(
    p_val,
    corr = FALSE,
    na = FALSE,
    cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
    symbols = c("***", "**", "*", ".", " ")
  ))
}

#' @export
#' @rdname model_terms
#' @importFrom stats anova
model_beta_label <- function(model) {
  beta_vars <- model_beta_variables(model)

  if (! inherits(model, "lm")) {
    return(beta_vars)
  }

  # for lm models only
  tidy_anova <- broom::tidy(anova(model))
  tidy_anova <- tidy_anova[tidy_anova$term %in% beta_vars, ]
  p_vals <- tidy_anova$p.value
  names(p_vals) <- tidy_anova$term
  p_vals <- p_vals[beta_vars]

  x_labs <- paste(beta_vars, beta_stars(p_vals), sep = "")
  gsub("\\s+$", "", x_labs)
}

broom_columns <- function() {
  c(".fitted", ".se.fit", ".resid", ".hat", ".sigma", ".cooksd", ".std.resid")
}

#' RColorBrewer Set1 colors
#'
#' @param col standard color name used to retrieve hex color value
#' @import RColorBrewer
#' @export
brew_colors <- function(col) {
  brew_cols <- RColorBrewer::brewer.pal(n = 9, "Set1")
  names(brew_cols) <- c("red", "blue", "green", "purple", "orange", "yellow", "brown", "pink", "grey")
  brew_cols <- as.list(brew_cols)
  ret <- brew_cols[[col]]
  if (is.null(ret)) {
    stop(paste("color '", col, "' not found in: c(", paste(names(brew_cols), collapse = ", "), ")", sep = ""))
  }
  ret
}



#' ggnostic plot function to add a background line to a geom
#'
#' If a non-null \code{linePosition} value is given, a line will be drawn before the given \code{continuous_geom} or \code{combo_geom} is added to the plot.
#'
#' Functions with a color in their name have different default color behavior.
#'
#' @param data,mapping supplied directly to \code{ggplot2::\link[ggplot2]{ggplot(data, mapping)}}
#' @param ... parameters supplied to \code{continuous_geom} or \code{combo_geom}
#' @param linePosition,lineColor,lineSize,lineAlpha,lineType parameters supplied to \code{ggplot2::\link[ggplot2]{geom_line}}
#' @param continuous_geom ggplot2 geom that is executed after the line is (possibly) added and if the x data is continous
#' @param combo_geom ggplot2 geom that is executed after the line is (possibly) added and if the x data is discrete
#' @param mapColorToFill boolean to determine if combo plots should cut the color mapping to the fill mapping
#' @return ggplot2 plot object
#' @rdname ggally_nostic_line
ggally_nostic_line <- function(
  data, mapping, ...,
  linePosition = NULL,
  lineColor = "red",
  lineSize = 0.5, lineAlpha = 1,
  lineType = 1,
  continuous_geom = ggplot2::geom_point,
  combo_geom = ggplot2::geom_boxplot,
  mapColorToFill = TRUE
) {

  x_is_character <- is_character_column(data, mapping, "x")

  if (x_is_character & isTRUE(mapColorToFill)) {
    mapping <- mapping_color_to_fill(mapping)
  }

  p <- ggplot(data = data, mapping = mapping)

  if (!is.null(linePosition)) {
    p <- p +
      geom_hline(
        yintercept = linePosition,
        color = lineColor,
        size = lineSize,
        alpha = lineAlpha,
        linetype = lineType
      )
  }

  if (x_is_character) {
    p <- p + combo_geom(...)
  } else {
    p <- p + continuous_geom(...)
  }

  p
}


#' ggnostic function to display residuals
#'
#' If non-null \code{pVal} and \code{sigma} values are given, confidence interval lines will be added to the plot at the specified \code{pVal} percentiles of a N(0, sigma) distribution.
#'
#' @param data,mapping,... parameters supplied to \code{\link{ggally_nostic_line}}
#' @param linePosition,lineColor,lineSize,lineAlpha,lineType parameters supplied to \code{ggplot2::\link[ggplot2]{geom_line}}
#' @param lineConfColor,lineConfSize,lineConfAlpha,lineConfType parameters supplied to the confidence interval lines
#' @param pVal percentiles of a N(0, sigma) distribution to be drawn
#' @param sigma sigma value for the \code{pVal} percentiles
#' @param se boolen to determine if the confidence intervals should be displayed
#' @return ggplot2 plot object
#' @seealso \code{stats::\link[stats]{residuals}}
#' @export
#' @importFrom stats qnorm
ggally_nostic_resid <- function(
  data, mapping, ...,
  linePosition = 0,
  lineColor = brew_colors("red"),
  lineSize = 0.5, lineAlpha = 1,
  lineType = 1,
  lineConfColor = lineColor,
  lineConfSize = lineSize, lineConfAlpha = lineAlpha,
  lineConfType = 2,
  pVal = c(0.025, 0.975),
  sigma = attr(data, "broom_glance")$sigma,
  se = TRUE
) {

  if (!is.null(linePosition) & !is.null(pVal) & !is.null(sigma)) {
    scaled_sigmas <- qnorm(pVal, lower.tail = TRUE, sd = sigma)

    linePosition <- c(linePosition, linePosition + scaled_sigmas)
    lineColor <- c(lineColor, lineConfColor, lineConfColor)
    lineType <- c(lineType, lineConfType, lineConfType)
    lineSize <- c(lineSize, lineConfSize, lineConfSize)
    lineAlpha <- c(lineAlpha, lineConfAlpha, lineConfAlpha)
  }

  ggally_nostic_line(
    data, mapping, ...,
    linePosition = linePosition,
    lineColor = lineColor,
    lineType = lineType,
    lineSize = lineSize,
    lineAlpha = lineAlpha
  ) +
    geom_smooth(se = se) +
    coord_cartesian(
      ylim = range(
        c(linePosition, eval_data_col(data, mapping$y)),
        na.rm = TRUE
      )
    )

}



#' ggnostic function to display standardized residuals
#'
#' If non-null \code{pVal} and \code{sigma} values are given, confidence interval lines will be added to the plot at the specified \code{pVal} locations of a N(0, 1) distribution.
#'
#' @param data,mapping,... parameters supplied to \code{\link{ggally_nostic_resid}}
#' @param sigma sigma value for the \code{pVal} percentiles. Set to 1 for standardized residuals
#' @seealso \code{stats::\link[stats]{rstandard}}
#' @return ggplot2 plot object
#' @rdname ggally_nostic_std_resid
#' @export
ggally_nostic_std_resid <- function(
  data, mapping, ...,
  sigma = 1
) {
  ggally_nostic_resid(
    data, mapping, ...,
    sigma = sigma
  )
}


#' ggnostic function to display stats::predict's standard errors
#'
#' As stated in \code{stats::\link[stats]{predict}} documentation:
#'
#'  If the logical 'se.fit' is 'TRUE', standard errors of the predictions are calculated.  If the numeric argument 'scale' is set (with optional ''df'), it is used as the residual standard deviation in the computation of the standard errors, otherwise this is extracted from the model fit.
#'
#' Since the se.fit is \code{TRUE} and scale is unset by default, the standard errors are extracted from the model fit.
#'
#' A base line of 0 is added to give reference to a perfect fit.
#'
#' @param data,mapping,...,lineColor parameters supplied to \code{\link{ggally_nostic_line}}
#' @param linePosition base comparison for a perfect fit
#' @seealso \code{stats::\link[stats]{influence}}
#' @return ggplot2 plot object
#' @export
ggally_nostic_se_fit <- function(
  data, mapping, ...,
  lineColor = brew_colors("green"),
  linePosition = 0
) {
  ggally_nostic_line(
    data, mapping, ...,
    lineColor = lineColor,
    linePosition = linePosition
  )
}


#' ggnostic function to display stats::influence's sigma
#'
#' As stated in \code{stats::\link[stats]{influence}} documentation:
#'
#' sigma: a vector whose i-th element contains the estimate of the residual standard deviation obtained when the i-th case is dropped from the regression.  (The approximations needed for GLMs can result in this being 'NaN'.)
#'
#' A line is added to display the overall model's sigma value. This gives a baseline for comparison
#'
#' @param data,mapping,...,lineColor parameters supplied to \code{\link{ggally_nostic_line}}
#' @param linePosition line that is drawn in the background of the plot. Defaults to the overall model's sigma value.
#' @seealso \code{stats::\link[stats]{influence}}
#' @return ggplot2 plot object
#' @export
ggally_nostic_sigma <- function(
  data, mapping, ...,
  lineColor = brew_colors("red"),
  linePosition = attr(data, "broom_glance")$sigma
) {
  ggally_nostic_line(
    data, mapping, ...,
    lineColor = lineColor,
    linePosition = linePosition
  )
}


#' ggnostic function to display stats::cooks.distance
#'
#' A line is added at 1 to display the general cutoff point for Cook's Distance.
#'
#' Reference: Cook, R. Dennis; Weisberg, Sanford (1982). Residuals and Influence in Regression. New York, NY: Chapman & Hall. ISBN 0-412-24280-X
#'
#' @param data,mapping,...,lineColor parameters supplied to \code{\link{ggally_nostic_line}}
#' @param linePosition 1 is the general cutoff point for Cook's Distance
#' @seealso \code{stats::\link[stats]{cooks.distance}}
#' @return ggplot2 plot object
#' @rdname ggally_nostic_cooksd
#' @export
ggally_nostic_cooksd <- function(
  data, mapping, ...,
  linePosition = 4 / nrow(data),
  lineColor = brew_colors("purple")
) {
  ggally_nostic_line(
    data, mapping, ...,
    linePosition = linePosition,
    lineColor = lineColor
  )
}



#' ggnostic function to display stats::influence's hat information
#'
#' As stated in \code{stats::\link[stats]{influence}} documentation:
#'
#' hat: a vector containing the diagonal of the 'hat' matrix.
#'
#' The diagonal elements of the 'hat' matrix describe the influence each response value has on the fitted value for that same observation.
#'
#' A suggested "cutoff" line is added to the plot at a height of 2 * p / n and an expected line at a height of p / n.

#' If either \code{linePosition} or \code{avgLinePosition} is \code{NULL}, the respective line will not be drawn.
#'
#' @param data,mapping,... supplied directly to \code{\link{ggally_nostic_line}}
#' @param linePosition,lineColor,lineSize,lineAlpha,lineType parameters supplied to \code{ggplot2::\link[ggplot2]{geom_line}} for the cutoff line
#' @param avgLinePosition,avgLineColor,avgLineSize,avgLineAlpha,avgLineType parameters supplied to \code{ggplot2::\link[ggplot2]{geom_line}} for the average line
#' @seealso \code{stats::\link[stats]{influence}}
#' @return ggplot2 plot object
#' @export
ggally_nostic_hat <- function(
  data, mapping, ...,
  linePosition = 2 * sum(data[[deparse(mapping$y)]]) / nrow(data),
  lineColor = brew_colors("purple"),
  lineSize = 0.5, lineAlpha = 1,
  lineType = 1,
  avgLinePosition = sum(data[[deparse(mapping$y)]]) / nrow(data),
  avgLineColor = brew_colors("red"),
  avgLineSize = lineSize, avgLineAlpha = lineAlpha,
  avgLineType = lineType
) {

  if (is.null(linePosition)) {
    lineColor <- lineSize <- lineAlpha <- lineType <- NULL
  }
  if (is.null(avgLinePosition)) {
    avgLineColor <- avgLineSize <- avgLineAlpha <- avgLineType <- NULL
  }

  ggally_nostic_line(
    data, mapping, ...,
    linePosition = c(linePosition, avgLinePosition),
    lineColor = c(lineColor, avgLineColor),
    lineSize = c(lineSize, avgLineSize),
    lineType = c(lineType, avgLineType),
    lineAlpha = c(lineAlpha, avgLineAlpha)
  )
}





nostic_switch <- function(
  types
) {

  function(data, mapping, ...) {
    y_var <- deparse(mapping$y)

    fn <- switch(y_var,
      .fitted = types$fitted,
      .se.fit = types$se_fit,
      .resid = types$resid,
      .hat = types$hat,
      .sigma = types$sigma,
      .cooksd = types$cooksd,
      .std.resid = types$std_resid,
      types$default
    )

    fn(data = data, mapping = mapping, ...)
  }
}


check_and_set_nostic_types <- function(
  types,
  default,
  fitted,
  resid,
  std_resid,
  sigma,
  se_fit,
  hat,
  cooksd
) {

  types_names <- names(types)

  set_type_value <- function(name, value) {
    if (is.null(types[[name]])) {
      # value is not set

      if (! (name %in% types_names)) {
        # set suggested fn
        types[[name]] <- value
      } else {
        # does not plot displayed
        types[[name]] <- ggally_blank
      }
    }
  }

  set_type_value("default", default)
  set_type_value("fitted", fitted)
  set_type_value("resid", resid)
  set_type_value("std_resid", std_resid)
  set_type_value("sigma", sigma)
  set_type_value("se_fit", se_fit)
  set_type_value("hat", hat)
  set_type_value("cooksd", cooksd)

  types
}

# TODO document
#' @export
ggnostic <- function(
  model,
  ...,
  columnsX = attr(data, "var_x"),
  columnsY = c(".fitted", ".se.fit", ".resid", ".std.resid", ".sigma", ".hat", ".cooksd"),
  # columnsY = c(".resid", ".sigma", ".hat", ".cooksd"),
  columnLabelsX = attr(data, "var_x_label"),
  columnLabelsY = gsub("\\.", " ", gsub("^\\.", "", columnsY)),
  continuous = list(
    default = ggally_nostic_line,
    fitted = ggally_nostic_line,
    se_fit = ggally_nostic_se_fit,
    resid = ggally_nostic_resid,
    hat = ggally_nostic_hat,
    sigma = ggally_nostic_sigma,
    cooksd = ggally_nostic_cooksd,
    std_resid = ggally_nostic_std_resid
  ),
  combo = list(
    default = ggally_nostic_line,
    fitted = ggally_nostic_line,
    se_fit = ggally_nostic_se_fit,
    resid = ggally_nostic_resid,
    hat = ggally_nostic_hat,
    sigma = ggally_nostic_sigma,
    cooksd = ggally_nostic_cooksd,
    std_resid = ggally_nostic_std_resid
  ),
  discrete = list(
    default = ggally_ratio,
    fitted = ggally_ratio,
    se_fit = ggally_ratio,
    resid = ggally_ratio,
    hat = ggally_ratio,
    sigma = ggally_ratio,
    cooksd = ggally_ratio,
    std_resid = ggally_ratio
  ),
  xlab = "coefficients",
  ylab = "diagnostics",
  title = deparse(model$call),
  data = broomify(model)
) {


  continuous_types <- check_and_set_nostic_types(
    continuous,
    default = ggally_nostic_line,
    fitted = ggally_nostic_line,
    se_fit = ggally_nostic_se_fit,
    resid = ggally_nostic_resid,
    hat = ggally_nostic_hat,
    sigma = ggally_nostic_sigma,
    cooksd = ggally_nostic_cooksd,
    std_resid = ggally_nostic_std_resid
  )
  combo_types <- check_and_set_nostic_types(
    combo,
    default = ggally_nostic_line,
    fitted = ggally_nostic_line,
    se_fit = ggally_nostic_se_fit,
    resid = ggally_nostic_resid,
    hat = ggally_nostic_hat,
    sigma = ggally_nostic_sigma,
    cooksd = ggally_nostic_cooksd,
    std_resid = ggally_nostic_std_resid
  )
  discrete_types <- check_and_set_nostic_types(
    discrete,
    default = ggally_ratio,
    fitted = ggally_ratio,
    se_fit = ggally_ratio,
    resid = ggally_ratio,
    hat = ggally_ratio,
    sigma = ggally_ratio,
    cooksd = ggally_ratio,
    std_resid = ggally_ratio
  )

  continuous_fn <- nostic_switch(continuous_types)
  combo_fn <- nostic_switch(combo_types)
  discrete_fn <- nostic_switch(discrete_types)

  columnsX <- match_nostic_columns(columnsX, attr(data, "var_x"), "columnsX")
  columnsY <- match_nostic_columns(
    columnsY,
    c(attr(data, "var_y"), broom_columns()),
    "columnsY"
  )

  ggduo(
    data = data,
    columnsX = columnsX, columnsY = columnsY,
    columnLabelsX = columnLabelsX,
    columnLabelsY = columnLabelsY,
    types = list(
      continuous = continuous_fn,
      comboVertical = combo_fn,
      comboHorizontal = combo_fn,
      discrete = discrete_fn
    ),
    ...,
    title = title,
    xlab = xlab,
    ylab = ylab
  )

}




# https://github.com/ggobi/ggobi/blob/master/data/pigs.xml
#' Multiple Time Series
#'
#' GGally implementation of ts.plot
#' @param data,mapping,columnsX,columnsY,... supplied directly to \code{\link{ggduo}}
#' @return ggmatrix object
#' @export
#' @examples
#' pigs <- read.table("http://lib.stat.cmu.edu/datasets/Andrews/T62.1")
#' pigs <- pigs[,-(1:3)]
#' colnames(pigs) <- c("year", "quarter", "gilts", "profit", "s_per_herdsz", "production", "herdsz")
#'
#' pigs$time <- pigs$year + (pigs$quarter - 1) / 4
#'
#' ggts(pigs, "time", c("gilts", "profit", "s_per_herdsz", "production", "herdsz"))
ggts <- function(data, mapping, columnsX, columnsY, ...) {
  pm <- ggduo(
    data = data,
    mapping = mapping,
    columnsX = columnsX, columnsY = columnsY,
    ...
  )

  # remove the "time" strip
  pm$xAxisLabels <- NULL

  pm
}





# if (!is.null(group)) {
#   column_type <- unlist(lapply(
#     data[setdiff(names(data), broom_columns())],
#     plotting_data_type
#   ))
#   is_discrete <- column_type[column_type == "discrete"]
#   group_names <- names(is_discrete)
# } else {
#   group_names <- deparse(mapping$group)
# }
#
# line_mapping <- mapping
# line_mapping[c("x", "y", "xend", "yend")] <-
#   aes_string(x = "xmin", y = "ymin", xend = "xmax", yend = "ymax")
#
# color_group <- c(group_names)
#
# if (!is.null(mapping$colour)) {
#   # set the colors to the mapping colors
#   color_group[length(color_group) + 1] <- deparse(mapping$colour)
# } else {
#   # set the default color to the line color
#   line_mapping$colour <- I(lineColor)
# }
#
# color_group <- unique(color_group)
#
# print(line_mapping)
# print(color_group)
# hline_data <- ddply(
#   data, color_group,
#   function(subsetDt) {
#     ret <- data.frame(
#       ymax = mean(subsetDt[[deparse(mapping$y)]], na.rm = TRUE),
#       ymin = mean(subsetDt[[deparse(mapping$y)]], na.rm = TRUE),
#       xmin = min(subsetDt[[deparse(mapping$x)]], na.rm = TRUE),
#       xmax = max(subsetDt[[deparse(mapping$x)]], na.rm = TRUE)
#     )
#     # transfer the unique columns that need to be there
#     for (col in color_group) {
#       ret[[col]] <- unique(subsetDt[[col]])
#     }
#     ret
#   }
# )


match_nostic_columns <- function(columns, choices, name) {
  column_matches <- pmatch(columns, choices, nomatch = NA, duplicates.ok = TRUE)
  if (any(is.na(column_matches))) {
    stop(paste(
      "Could not match '", name, "': c(",
      paste("'", columns[is.na(column_matches)], "'", collapse = ", ", sep = ""),
      ") to choices: c(",
      paste("'", choices, "'", collapse = ", ", sep = ""),
      ")",
      sep = ""
    ))
  }
  columnsY <- choices[column_matches]

}
