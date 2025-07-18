#' Correlation matrix
#'
#' Function for making a correlation matrix plot, using \pkg{ggplot2}.
#' The function is directly inspired by Tian Zheng and Yu-Sung Su's
#' \code{corrplot} function in the 'arm' package.
#' Please visit \url{https://github.com/briatte/ggcorr} for the latest version
#' of \code{ggcorr}, and see the vignette at
#' \url{https://briatte.github.io/ggcorr/} for many examples of how to use it.
#'
#' @export
#' @param data a data frame or matrix containing numeric (continuous) data. If
#' any of the columns contain non-numeric data, they will be dropped with a
#' warning.
#' @param method a vector of two character strings. The first value gives the
#' method for computing covariances in the presence of missing values, and must
#' be (an abbreviation of) one of \code{"everything"}, \code{"all.obs"},
#' \code{"complete.obs"}, \code{"na.or.complete"} or
#' \code{"pairwise.complete.obs"}. The second value gives the type of
#' correlation coefficient to compute, and must be one of \code{"pearson"},
#' \code{"kendall"} or \code{"spearman"}.
#' See \code{\link[stats]{cor}} for details.
#' Defaults to \code{c("pairwise", "pearson")}.
#' @param cor_matrix the named correlation matrix to use for calculations.
#' Defaults to the correlation matrix of \code{data} when \code{data} is
#' supplied.
#' @param palette if \code{nbreaks} is used, a ColorBrewer palette to use
#' instead of the colors specified by \code{low}, \code{mid} and \code{high}.
#' Defaults to \code{NULL}.
#' @param name a character string for the legend that shows the colors of the
#' correlation coefficients.
#' Defaults to \code{""} (no legend name).
#' @param geom the geom object to use. Accepts either \code{"tile"},
#' \code{"circle"}, \code{"text"} or \code{"blank"}.
#' @param min_size when \code{geom} has been set to \code{"circle"}, the minimum
#' size of the circles.
#' Defaults to \code{2}.
#' @param max_size when \code{geom} has been set to \code{"circle"}, the maximum
#' size of the circles.
#' Defaults to \code{6}.
#' @param label whether to add correlation coefficients to the plot.
#' Defaults to \code{FALSE}.
#' @param label_alpha whether to make the correlation coefficients increasingly
#' transparent as they come close to 0. Also accepts any numeric value between
#' \code{0} and \code{1}, in which case the level of transparency is set to that
#' fixed value.
#' Defaults to \code{FALSE} (no transparency).
#' @param label_color the color of the correlation coefficients.
#' Defaults to \code{"grey75"}.
#' @param label_round the decimal rounding of the correlation coefficients.
#' Defaults to \code{1}.
#' @param label_size the size of the correlation coefficients.
#' Defaults to \code{4}.
#' @param nbreaks the number of breaks to apply to the correlation coefficients,
#' which results in a categorical color scale. See 'Note'.
#' Defaults to \code{NULL} (no breaks, continuous scaling).
#' @param digits the number of digits to show in the breaks of the correlation
#' coefficients: see \code{\link[base]{cut}} for details.
#' Defaults to \code{2}.
#' @param low the lower color of the gradient for continuous scaling of the
#' correlation coefficients.
#' Defaults to \code{"#3B9AB2"} (blue).
#' @param mid the midpoint color of the gradient for continuous scaling of the
#' correlation coefficients.
#' Defaults to \code{"#EEEEEE"} (very light grey).
#' @param high the upper color of the gradient for continuous scaling of the
#' correlation coefficients.
#' Defaults to \code{"#F21A00"} (red).
#' @param midpoint the midpoint value for continuous scaling of the
#' correlation coefficients.
#' Defaults to \code{0}.
#' @param limits bounding of color scaling for correlations, set \code{limits = NULL} or \code{FALSE} to remove
#' @param drop if using \code{nbreaks}, whether to drop unused breaks from the
#' color scale.
#' Defaults to \code{FALSE} (recommended).
#' @param layout.exp a multiplier to expand the horizontal axis to the left if
#' variable names get clipped.
#' Defaults to \code{0} (no expansion).
#' @param legend.position where to put the legend of the correlation
#' coefficients: see \code{\link[ggplot2]{theme}} for details.
#' Defaults to \code{"bottom"}.
#' @param legend.size the size of the legend title and labels, in points: see
#' \code{\link[ggplot2]{theme}} for details.
#' Defaults to \code{9}.
#' @param ... other arguments supplied to \code{\link[ggplot2]{geom_text}} for
#' the diagonal labels.
#' @note Recommended values for the \code{nbreaks} argument are \code{3} to
#' \code{11}, as values above 11 are visually difficult to separate and are not
#' supported by diverging ColorBrewer palettes.
#'
#' @seealso \code{\link[stats]{cor}} and \code{corrplot} in the
#' \code{arm} package.
#' @author Francois Briatte, with contributions from Amos B. Elberg and
#' Barret Schloerke
#' @importFrom stats cor
#' @importFrom dplyr rename
#' @importFrom tidyr pivot_longer
#' @importFrom grDevices colorRampPalette
#' @examples
#' # Small function to display plots only if it's interactive
#' p_ <- GGally::print_if_interactive
#'
#' # Basketball statistics provided by Nathan Yau at Flowing Data.
#' dt <- read.csv("http://datasets.flowingdata.com/ppg2008.csv")
#'
#' # Default output.
#' p_(ggcorr(dt[, -1]))
#'
#' # Labeled output, with coefficient transparency.
#' p_(ggcorr(dt[, -1],
#'   label = TRUE,
#'   label_alpha = TRUE
#' ))
#'
#' # Custom options.
#' p_(ggcorr(
#'   dt[, -1],
#'   name = expression(rho),
#'   geom = "circle",
#'   max_size = 10,
#'   min_size = 2,
#'   size = 3,
#'   hjust = 0.75,
#'   nbreaks = 6,
#'   angle = -45,
#'   palette = "PuOr" # colorblind safe, photocopy-able
#' ))
#'
#' # Supply your own correlation matrix
#' p_(ggcorr(
#'   data = NULL,
#'   cor_matrix = cor(dt[, -1], use = "pairwise")
#' ))
ggcorr <- function(
  data,
  method = c("pairwise", "pearson"),
  cor_matrix = NULL,
  nbreaks = NULL,
  digits = 2,
  name = "",
  low = "#3B9AB2",
  mid = "#EEEEEE",
  high = "#F21A00",
  midpoint = 0,
  palette = NULL,
  geom = "tile",
  min_size = 2,
  max_size = 6,
  label = FALSE,
  label_alpha = FALSE,
  label_color = "black",
  label_round = 1,
  label_size = 4,
  limits = c(-1, 1),
  drop = is.null(limits) || identical(limits, FALSE),
  layout.exp = 0,
  legend.position = "right",
  legend.size = 9,
  ...
) {
  if (is.numeric(limits)) {
    if (length(limits) != 2) {
      stop("'limits' must be of length 2 if numeric")
    }
  }
  if (is.logical(limits)) {
    if (limits) {
      limits <- c(-1, 1)
    } else {
      limits <- NULL
    }
  }

  # -- check geom argument -----------------------------------------------------

  if (length(geom) > 1 || !geom %in% c("blank", "circle", "text", "tile")) {
    stop("incorrect geom value")
  }

  # -- correlation method ------------------------------------------------------

  if (length(method) == 1) {
    method <- c(method, "pearson") # for backwards compatibility
  }

  # -- check data columns ------------------------------------------------------

  if (!is.null(data)) {
    if (!is.data.frame(data)) {
      data <- as.data.frame(data)
    }

    x <- which(!sapply(data, is.numeric))

    if (length(x) > 0) {
      warning(paste(
        "data in column(s)",
        paste0(paste0("'", names(data)[x], "'"), collapse = ", "),
        "are not numeric and were ignored"
      ))

      data <- data[, -x]
    }
  }

  # -- correlation matrix ------------------------------------------------------

  if (is.null(cor_matrix)) {
    cor_matrix <- cor(data, use = method[1], method = method[2])
  }

  m <- cor_matrix
  colnames(m) <- rownames(m) <- gsub(" ", "_", colnames(m)) # protect spaces

  # -- correlation data.frame --------------------------------------------------

  m[upper.tri(m, diag = T)] <- NA
  rownames(m) <- colnames(m)
  m <- data.frame(m)

  m$.ggally_ggcorr_row_names <- rownames(m)
  m_long <-
    m %>%
    tidyr::pivot_longer(
      cols = -".ggally_ggcorr_row_names",
      names_to = "y",
      values_to = "coefficient"
    ) %>%
    dplyr::rename(x = ".ggally_ggcorr_row_names") %>%
    dplyr::mutate(y = factor(.data$y, levels = rownames(m)))

  # -- correlation quantiles ---------------------------------------------------

  if (!is.null(nbreaks)) {
    x <- seq(-1, 1, length.out = nbreaks + 1)

    if (!nbreaks %% 2) {
      x <- sort(c(x, 0))
    }

    m_long$breaks <- cut(
      m_long$coefficient,
      breaks = unique(x),
      include.lowest = TRUE,
      dig.lab = digits
    )
  }

  # -- gradient midpoint -------------------------------------------------------

  if (is.null(midpoint)) {
    midpoint <- median(m_long$coefficient, na.rm = TRUE)
    message(paste(
      "Color gradient midpoint set at median correlation to",
      round(midpoint, 2)
    ))
  }

  # -- plot structure ----------------------------------------------------------
  m_long$label <-
    format(
      # round(c(0.2, 0, 0.001), digits = 2) #> c(0.2, 0, 0)
      round(m_long$coefficient, digits = label_round),
      # format(c(0.2, 0, 0), nsmall = 2) #> c("0.20", "0.00", "0.00")
      nsmall = label_round
    )

  p <- ggplot(na.omit(m_long), aes(.data$x, .data$y))

  if (geom == "tile") {
    if (is.null(nbreaks)) {
      # -- tiles, continuous ---------------------------------------------------

      p <- p +
        geom_tile(aes(fill = .data$coefficient), color = "white")
    } else {
      # -- tiles, ordinal ------------------------------------------------------

      p <- p +
        geom_tile(aes(fill = .data$breaks), color = "white")
    }

    # -- tiles, color scale ----------------------------------------------------

    if (is.null(nbreaks) && !is.null(limits)) {
      p <- p +
        scale_fill_gradient2(
          name,
          low = low,
          mid = mid,
          high = high,
          midpoint = midpoint,
          limits = limits
        )
    } else if (is.null(nbreaks)) {
      p <- p +
        scale_fill_gradient2(
          name,
          low = low,
          mid = mid,
          high = high,
          midpoint = midpoint
        )
    } else if (is.null(palette)) {
      x <- colorRampPalette(c(low, mid, high))(length(levels(m_long$breaks)))

      p <- p +
        scale_fill_manual(name, values = x, drop = drop)
    } else {
      p <- p +
        scale_fill_brewer(name, palette = palette, drop = drop)
    }
  } else if (geom == "circle") {
    p <- p +
      geom_point(aes(size = abs(.data$coefficient) * 1.25), color = "grey50") # border

    if (is.null(nbreaks)) {
      # -- circles, continuous -------------------------------------------------

      p <- p +
        geom_point(aes(
          size = abs(.data$coefficient),
          color = .data$coefficient
        ))
    } else {
      # -- circles, ordinal ----------------------------------------------------

      p <- p +
        geom_point(aes(size = abs(.data$coefficient), color = .data$breaks))
    }

    p <- p +
      scale_size_continuous(range = c(min_size, max_size)) +
      guides(size = "none")

    r <- list(size = (min_size + max_size) / 2)

    # -- circles, color scale --------------------------------------------------

    if (is.null(nbreaks) && !is.null(limits)) {
      p <- p +
        scale_color_gradient2(
          name,
          low = low,
          mid = mid,
          high = high,
          midpoint = midpoint,
          limits = limits
        )
    } else if (is.null(nbreaks)) {
      p <- p +
        scale_color_gradient2(
          name,
          low = low,
          mid = mid,
          high = high,
          midpoint = midpoint
        )
    } else if (is.null(palette)) {
      x <- colorRampPalette(c(low, mid, high))(length(levels(m_long$breaks)))

      p <- p +
        scale_color_manual(name, values = x, drop = drop) +
        guides(color = guide_legend(override.aes = r))
    } else {
      p <- p +
        scale_color_brewer(name, palette = palette, drop = drop) +
        guides(color = guide_legend(override.aes = r))
    }
  } else if (geom == "text") {
    if (is.null(nbreaks)) {
      # -- text, continuous ----------------------------------------------------

      p <- p +
        geom_text(
          aes(label = label, color = .data$coefficient),
          size = label_size
        )
    } else {
      # -- text, ordinal -------------------------------------------------------

      p <- p +
        geom_text(aes(label = label, color = .data$breaks), size = label_size)
    }

    # -- text, color scale ----------------------------------------------------

    if (is.null(nbreaks) && !is.null(limits)) {
      p <- p +
        scale_color_gradient2(
          name,
          low = low,
          mid = mid,
          high = high,
          midpoint = midpoint,
          limits = limits
        )
    } else if (is.null(nbreaks)) {
      p <- p +
        scale_color_gradient2(
          name,
          low = low,
          mid = mid,
          high = high,
          midpoint = midpoint
        )
    } else if (is.null(palette)) {
      x <- colorRampPalette(c(low, mid, high))(length(levels(m_long$breaks)))

      p <- p +
        scale_color_manual(name, values = x, drop = drop)
    } else {
      p <- p +
        scale_color_brewer(name, palette = palette, drop = drop)
    }
  }

  # -- coefficient labels ------------------------------------------------------

  if (label) {
    if (isTRUE(label_alpha)) {
      p <- p +
        geom_text(
          aes(.data$x, .data$y, label = label, alpha = abs(.data$coefficient)),
          color = label_color,
          size = label_size,
          show.legend = FALSE
        )
    } else if (label_alpha > 0) {
      p <- p +
        geom_text(
          aes(.data$x, .data$y, label = label),
          show.legend = FALSE,
          alpha = label_alpha,
          color = label_color,
          size = label_size
        )
    } else {
      p <- p +
        geom_text(
          aes(.data$x, .data$y, label = label),
          color = label_color,
          size = label_size
        )
    }
  }

  # -- horizontal scale expansion ----------------------------------------------

  textData <- m_long[m_long$x == m_long$y & is.na(m_long$coefficient), ]
  xLimits <- levels(textData$y)
  textData$diagLabel <- textData$x

  if (!is.numeric(layout.exp) || layout.exp < 0) {
    stop("incorrect layout.exp value")
  } else if (layout.exp > 0) {
    layout.exp <- as.integer(layout.exp)
    # copy to fill in spacer info
    textData <- rbind(textData[1:layout.exp, ], textData)

    spacer <- paste(".ggally_ggcorr_spacer_value", 1:layout.exp, sep = "")

    textData$x[1:layout.exp] <- spacer
    textData$diagLabel[1:layout.exp] <- NA
    xLimits <- c(spacer, levels(m_long$y))
  }

  p <- p +
    geom_text(
      data = textData,
      aes(label = !!as.name("diagLabel")),
      ...,
      na.rm = TRUE
    ) +
    scale_x_discrete(breaks = NULL, limits = xLimits) +
    scale_y_discrete(breaks = NULL, limits = levels(m_long$y)) +
    labs(x = NULL, y = NULL) +
    coord_equal() +
    theme(
      panel.background = element_blank(),
      legend.key = element_blank(),
      legend.position = legend.position,
      legend.title = element_text(size = legend.size),
      legend.text = element_text(size = legend.size)
    )

  return(p)
}
