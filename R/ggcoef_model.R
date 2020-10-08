#' Plot model coefficients
#'
#' Redesign of [GGally::ggcoef()] based on [broom.helpers].
#'
#' @inheritParams broom.helpers::tidy_plus_plus
#' @param model a regression model object
#' @param conf.level the confidence level to use for the confidence
#'   interval if `conf.int = TRUE`; must be strictly greater than 0
#'   and less than 1; defaults to 0.95, which corresponds to a 95
#'   percent confidence interval
#' @param show_p_values if `TRUE`, add p-value to labels
#' @param signif_stars if `TRUE`, add significant stars to labels
#' @param significance level (between 0 and 1) below which a
#'   coefficient is consider to be significantly different from 0
#'   (or 1 if `exponentiate = TRUE`), `NULL` for not highlighting
#'   such coefficients
#' @param significance_labels optional vector with custom labels
#'   for significance variable
#' @param return_data if `TRUE`, will return the data.frame used for plotting instead of the plot
#' @param ... parameters passed to [ggcoef_plot()]
#' @export
#' @examples
#' # Small function to display plots only if it's interactive
#' p_ <- GGally::print_if_interactive
#'
#' data(trial, package = "gtsummary")
#' trial$high_marker <- factor(trial$marker > 1, label = c("low", "high"))
#' attr(trial$high_marker, "label") <- "Marker level"
#' mod <- glm(response ~ age + stage + grade + high_marker, trial, family = binomial(link = "logit"))
#' p_(ggcoef_model(mod))
#' p_(ggcoef_model(mod, exponentiate = TRUE))
#' p_(ggcoef_model(mod, exponentiate = TRUE, variable_labels = c(age = "Age in years", stage = "Stage of the disease")))
#' p_(ggcoef_model(mod, exponentiate = TRUE, no_reference_row = "high_marker", intercept = TRUE))
#' p_(ggcoef_model(mod, exponentiate = TRUE, include = c("stage", "age")))
#' p_(ggcoef_model(mod, significance = .10, conf.level = .9, signif_stars = FALSE, show_p_values = FALSE))
#' p_(ggcoef_model(mod, exponentiate = TRUE, colour = NULL, stripped_rows = FALSE, signif_stars = FALSE))
#' p_(ggcoef_model(mod, exponentiate = TRUE, conf.int = FALSE))
#'
#' mod <- glm(response ~ stage:age + grade:stage, trial, family = binomial(link = "logit"))
#' p_(ggcoef_model(mod, exponentiate = TRUE))
#'
#' if (require(survival)) {
#'   test <- list(time = c(4,3,1,1,2,2,3),
#'                 status = c(1,1,1,0,1,1,0),
#'                 x = c(0,2,1,1,1,0,0),
#'                 sex = c("f", "f", "f", "f", "m", "m", "m"))
#'   mod <- coxph(Surv(time, status) ~ x + sex, test)
#'   p_(ggcoef_model(mod, exponentiate = TRUE))
#' }
ggcoef_model <- function (
  model,
  tidy_fun = broom::tidy,
  conf.int = TRUE,
  conf.level = .95,
  exponentiate = TRUE,
  variable_labels = NULL,
  term_labels = NULL,
  add_reference_rows = TRUE,
  no_reference_row  = NULL,
  intercept = FALSE,
  include = dplyr::everything(),
  significance = .05,
  significance_labels = NULL,
  show_p_values = TRUE,
  signif_stars = TRUE,
  return_data = FALSE,
  ...
){
  data <- ggcoef_data(
    model = model,
    tidy_fun = tidy_fun,
    conf.int = conf.int,
    conf.level = conf.level,
    exponentiate = exponentiate,
    variable_labels = variable_labels,
    term_labels = term_labels,
    add_reference_rows = add_reference_rows,
    no_reference_row  = no_reference_row ,
    intercept = intercept,
    include = include,
    significance = significance,
    significance_labels = significance_labels
  )

  if (show_p_values & signif_stars)
    data$add_to_label <- paste0(data$p_value_label, data$signif_stars)
  if (show_p_values & !signif_stars)
    data$add_to_label <- data$p_value_label
  if (!show_p_values & signif_stars)
    data$add_to_label <- data$signif_stars

  if (show_p_values | signif_stars) {
    data$label <- forcats::fct_inorder(
      factor(
        paste0(
          data$label,
          ifelse(data$add_to_label == "", "", paste0(" (", data$add_to_label, ")"))
        )
      )
    )
  }

  if (return_data)
    return(data)

  args <- list(...)
  args$data <- data
  args$exponentiate <- exponentiate

  if (!"colour" %in% names(args)) {
    args$colour <- "var_label"
    if (!"colour_guide" %in% names(args)) {
      args$colour_guide <- FALSE
    }
  }

  do.call(ggcoef_plot, args)
}

#' @rdname ggcoef_model
#' @export
#' @param models named list of models
#' @param type a dodged plot or a facetted plot?
#' @examples
#'
#' # Comparison of several models
#' mod1 <- glm(response ~ age + stage + grade + high_marker, trial, family = binomial())
#' mod2 <- step(mod1, trace = 0)
#' mod3 <- glm(response ~ high_marker * stage, trial, family = binomial())
#' models <- list("Full model" = mod1, "Simplified model" = mod2, "With interaction" = mod3)
#'
#' p_(ggcoef_compare(models, exponentiate = TRUE))
#' p_(ggcoef_compare(models, exponentiate = TRUE, type = "faceted"))
#'
#' # you can reverse the vertical position of the point by using a negative value
#' # for dodged_width (but it will produce some warnings)
#' \dontrun{
#'   p_(ggcoef_compare(models, exponentiate = TRUE, dodged_width = -.9))
#' }
ggcoef_compare <- function (
  models,
  type = c("dodged", "faceted"),
  tidy_fun = broom::tidy,
  conf.int = TRUE,
  conf.level = .95,
  exponentiate = TRUE,
  variable_labels = NULL,
  term_labels = NULL,
  add_reference_rows = TRUE,
  no_reference_row  = NULL,
  intercept = FALSE,
  include = dplyr::everything(),
  significance = .05,
  significance_labels = NULL,
  return_data = FALSE,
  ...
){
  data <- lapply(
    X = models,
    FUN = ggcoef_data,
    tidy_fun = tidy_fun,
    conf.int = conf.int,
    conf.level = conf.level,
    exponentiate = exponentiate,
    variable_labels = variable_labels,
    term_labels = term_labels,
    add_reference_rows = add_reference_rows,
    no_reference_row  = no_reference_row ,
    intercept = intercept,
    significance = significance,
    significance_labels = significance_labels
  )

  data <- dplyr::bind_rows(data, .id = "model")
  coefficients_label <- attr(data, "coefficients_label")

  data$model <- forcats::fct_inorder(data$model)

  # include should be applied after lapply
  data <- data %>%
    broom.helpers::tidy_select_variables(
      include = include,
      model = models[[1]] # we just need to pass a model to allow the function to work
    ) %>%
    broom.helpers::tidy_detach_model()

  # Add NA values for unobserved combinations
  # (i.e. for a term present in one model but not in another)
  data <- data %>%
    tidyr::expand(model, tidyr::nesting(variable, var_label, var_class, var_type, contrasts, reference_row, label)) %>%
    dplyr::left_join(data, by = c("model", "variable", "var_label", "var_class", "var_type", "contrasts", "reference_row", "label"))

  attr(data, "coefficients_label") <- coefficients_label

  if (return_data)
    return(data)

  type <- match.arg(type)

  args <- list(...)
  args$data <- data
  args$exponentiate <- exponentiate

  if (type == "dodged") {
    if (!"dodged " %in% names(args)) {
      args$dodged  <- TRUE
    }
    if (!"colour" %in% names(args)) {
      args$colour <- "model"
    }
    if (!"errorbar_coloured" %in% names(args)) {
      args$errorbar_coloured <- TRUE
    }
  } else {
    if (!"facet_col" %in% names(args)) {
      args$facet_col <- "model"
    }
    if (!"colour" %in% names(args)) {
      args$colour <- "var_label"
      if (!"colour_guide" %in% names(args)) {
        args$colour_guide <- FALSE
      }
    }
  }

  do.call(ggcoef_plot, args)
}

#' @rdname ggcoef_model
#' @description
#' [ggcoef_multinom()] is a variation of [ggcoef_model()] adapted to multinomial
#' logistic regressions performed with [nnet::multinom()].
#' [ggcoef_multinom()] works only with the dev version of `gtsummary`.
#' @param y.level_label an optional named vector for labelling `y.level` (see examples)
#' @export
#' @examples
#'
#' # specific function for multinom models
#' data(tips, package = "reshape")
#' library(nnet)
#' mod <- multinom(day ~ total_bill + tip + sex + smoker, data = tips)
#' p_(ggcoef_multinom(mod))
#' p_(ggcoef_multinom(mod, y.level = c(Thur = "Thursday", Sat = "Saturday", Sun = "Sunday")))
#' p_(ggcoef_multinom(mod, type = "faceted"))
#' p_(ggcoef_multinom(mod, type = "faceted", y.level = c(Thur = "Thursday", Sat = "Saturday", Sun = "Sunday")))
ggcoef_multinom <- function (
  model,
  type = c("dodged", "faceted"),
  y.level_label = NULL,
  tidy_fun = broom::tidy,
  conf.int = TRUE,
  conf.level = .95,
  exponentiate = TRUE,
  variable_labels = NULL,
  term_labels = NULL,
  add_reference_rows = TRUE,
  no_reference_row  = NULL,
  intercept = FALSE,
  include = dplyr::everything(),
  significance = .05,
  significance_labels = NULL,
  show_p_values = TRUE,
  signif_stars = TRUE,
  return_data = FALSE,
  ...
){
  data <- ggcoef_data(
    model,
    tidy_fun = tidy_fun,
    conf.int = conf.int,
    conf.level = conf.level,
    exponentiate = exponentiate,
    variable_labels = variable_labels,
    term_labels = term_labels,
    add_reference_rows = add_reference_rows,
    no_reference_row  = no_reference_row ,
    intercept = intercept,
    include = include,
    significance = significance,
    significance_labels = significance_labels
  )

  if (!is.null(y.level_label))
    data$y.level <- factor(
      data$y.level,
      levels = names(y.level_label),
      labels = y.level_label
  ) else
    data$y.level <- forcats::fct_inorder(factor(data$y.level))

  if (return_data)
    return(data)

  type <- match.arg(type)

  args <- list(...)
  args$data <- data
  args$exponentiate <- exponentiate

  if (type == "dodged") {
    if (!"dodged " %in% names(args)) {
      args$dodged  <- TRUE
    }
    if (!"colour" %in% names(args)) {
      args$colour <- "y.level"
    }
    if (!"errorbar_coloured" %in% names(args)) {
      args$errorbar_coloured <- TRUE
    }
  } else {
    if (!"facet_col" %in% names(args)) {
      args$facet_col <- "y.level"
    }
    if (!"colour" %in% names(args)) {
      args$colour <- "var_label"
      if (!"colour_guide" %in% names(args)) {
        args$colour_guide <- FALSE
      }
    }
  }

  do.call(ggcoef_plot, args)
}

#' @rdname ggcoef_model
#' @export
ggcoef_data <- function (
  model,
  tidy_fun = broom::tidy,
  conf.int = TRUE,
  conf.level = .95,
  exponentiate = TRUE,
  variable_labels = NULL,
  term_labels = NULL,
  add_reference_rows = TRUE,
  no_reference_row  = NULL,
  intercept = FALSE,
  include = dplyr::everything(),
  significance = .05,
  significance_labels = NULL
){
  if (!requireNamespace("broom.helpers"))
    stop("Package broom.helpers is required.")

  data <- broom.helpers::tidy_plus_plus(
    model = model,
    tidy_fun = tidy_fun,
    conf.int = conf.int,
    conf.level = conf.level,
    exponentiate = exponentiate,
    variable_labels = variable_labels,
    term_labels = term_labels,
    add_reference_rows = add_reference_rows,
    no_reference_row = no_reference_row,
    add_estimate_to_reference_rows = TRUE,
    add_header_rows = FALSE,
    intercept = intercept,
    include = include,
    keep_model = FALSE
  )

  if(!is.null(significance)) {
    if (is.null(significance_labels))
      significance_labels <- paste(c("p \u2264", "p >"), significance)
    data$significance <- factor(
      !is.na(data$p.value) & data$p.value <= significance,
      levels = c(TRUE, FALSE),
      labels = significance_labels
    )
  }

  data$signif_stars <- signif_stars(data$p.value, point = NULL)

  data$p_value_label <- ifelse(is.na(data$p.value), "", scales::pvalue(data$p.value, add_p = TRUE))

  # keep only rows with estimate
  data <- data[!is.na(data$estimate), ]

  data$var_label <- forcats::fct_inorder(data$var_label)
  data$label <- forcats::fct_inorder(data$label)

  data
}

#' @rdname ggcoef_model
#' @param data a data frame containing data to be plotted, typically the output of [ggcoef_data()]
#' @param exponentiate if `TRUE` a logarithmic scale will be used for x-axis
#' @param point_size size of the points
#' @param point_stroke thickness of the points
#' @param point_fill fill colour for the points
#' @param colour optional variable name to be mapped to colour aesthetic
#' @param colour_guide should colour guide be displayed in the legend?
#' @param colour_lab label of the colour aesthetic in the legend
#' @param shape optional variable name to be mapped to the shape aesthetic
#' @param shape_values values of the different shapes to use in [ggplot2::scale_shape_manual()]
#' @param shape_guide should shape guide be displayed in the legend?
#' @param shape_lab label of the shape aesthetic in the legend
#' @param errorbar should error bars be plotted?
#' @param errorbar_height height of error bars
#' @param errorbar_coloured should error bars be coloured as the points?
#' @param stripped_rows should stripped rows be displayed in the background?
#' @param strips_odd color of the odd rows
#' @param strips_even color of the even rows
#' @param vline should a vertical line de drawn at 0 (or 1 if `exponentiate = TRUE`)?
#' @param vline_colour colour of vertical line
#' @param dodged should points be dodged (according to the colour aesthetic)?
#' @param dodged_width width value for [ggplot2::position_dodge()]
#' @param facet_col optional variable name to be used for column facets
#' @export
ggcoef_plot <- function (
  data,
  exponentiate = FALSE,
  point_size = 2,
  point_stroke = 2,
  point_fill = "white",
  colour = NULL,
  colour_guide = TRUE,
  colour_lab = "",
  shape = "significance",
  shape_values = c(16, 21),
  shape_guide = TRUE,
  shape_lab = "",
  errorbar = TRUE,
  errorbar_height = .1,
  errorbar_coloured = FALSE,
  stripped_rows = TRUE,
  strips_odd = "#11111111",
  strips_even = "#00000000",
  vline = TRUE,
  vline_colour = "grey50",
  dodged = FALSE,
  dodged_width = .8,
  facet_col = NULL
){
  data$label <- forcats::fct_rev(data$label)

  if (stripped_rows)
    data <- data %>%
      mutate(.fill = dplyr::if_else(as.integer(label) %% 2L == 1, strips_even, strips_odd))

  # mapping
  mapping <- aes_string(x = "estimate", y = "label")

  errorbar <- errorbar & all(c("conf.low", "conf.high") %in% names(data))
  if(errorbar) {
    mapping$xmin <- aes_string(xmin = "conf.low")$xmin
    mapping$xmax <- aes_string(xmax = "conf.high")$xmax
  }
  if(!is.null(shape) && shape %in% names(data)) {
    mapping$shape <- aes_string(shape = shape)$shape
  }
  if(!is.null(colour) && colour %in% names(data)) {
    mapping$colour <- aes_string(colour = colour)$colour
    mapping$group <- aes_string(group = colour)$group
  }

  # position
  if (dodged)
    position <- position_dodge(dodged_width)
  else
    position <- position_identity()

  # plot
  p <- ggplot(data = data, mapping = mapping)

  if (stripped_rows)
    p <- p +
      geom_stripped_rows(
        mapping = aes_string(y = "label", odd = ".fill", even = ".fill"),
        inherit.aes = FALSE
      )

  if (vline)
    p <- p + geom_vline(xintercept = ifelse(exponentiate, 1, 0), colour = vline_colour)

  if(errorbar) {
    if (!is.null(colour) & errorbar_coloured) {
      p <- p +
        geom_errorbarh(
          na.rm = TRUE,
          height = errorbar_height,
          position = position
        )
    } else {
      p <- p +
        geom_errorbarh(
          mapping = aes(colour = NULL),
          na.rm = TRUE,
          height = errorbar_height,
          colour = "black",
          position = position
        )
    }
  }

  if (!is.null(facet_col))
    facet_formula <- as.formula(paste("var_label ~ ", facet_col))
  else
    facet_formula <- var_label ~ .

  p <- p +
    geom_point(
      size = point_size,
      stroke = point_stroke,
      fill = point_fill,
      position = position,
      na.rm = TRUE
    ) +
    facet_grid(
      facet_formula,
      scales = "free_y", space = "free_y", switch = "y"
    ) +
    ylab("") +
    scale_y_discrete(expand = expansion(mult = 0, add = .5)) +
    theme_light() +
    theme(
      legend.position = "bottom",
      legend.box = "vertical",
      strip.placement = "outside",
      strip.text.y.left = element_text(face = "bold", angle = 0, colour = "black", hjust = 0, vjust = 1),
      strip.text.x = element_text(face = "bold", colour = "black"),
      strip.background = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(linetype = "dashed"),
      axis.title.x = element_text(face = "bold"),
      axis.ticks.y = element_blank()
    )

  if(!is.null(colour) && colour %in% names(data)) {
    if (colour_guide)
      colour_guide <- guide_legend()
    p <- p +
      scale_colour_discrete(guide = colour_guide) +
      labs(colour = colour_lab)
  }

  if(!is.null(shape) && shape %in% names(data)) {
    if (shape_guide)
      shape_guide <- guide_legend()
    p <- p +
      scale_shape_manual(
        values = shape_values,
        drop = FALSE,
        guide = shape_guide,
        na.translate = FALSE
      ) +
      labs(shape = shape_lab)
  }

  if (exponentiate)
    p <- p + scale_x_log10()

  if (!is.null(attr(data, "coefficients_label")))
    p <- p + xlab(attr(data, "coefficients_label"))

  p
}

