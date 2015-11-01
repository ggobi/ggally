if(getRversion() >= "2.15.1") {
  utils::globalVariables(c("cens", "surv", "up", "low"))
}

#' Plot \code{survfit} objects using \code{ggplot2}
#'
#' This function produces Kaplan-Meier plots using \code{ggplot2}.
#' As a first argument it needs a \code{survfit} object, created by the
#' \code{survival} package. Default settings differ for single stratum and
#' multiple strata objects.
#'
#' @export
#' @param s an object of class \code{survfit}
#' @param CI should a confidence interval be plotted? Defaults to \code{TRUE}
#'    for single stratum objects and \code{FALSE} for multiple strata objects.
#' @param plot.cens mark the censored observations?
#' @param surv.col colour of the survival estimate. Defaults to black for
#'    one stratum, and to the default \code{ggplot2} colours for multiple
#'    strata. Length of vector with colour names should be either 1 or equal
#'    to the number of strata.
#' @param cens.col colour of the points that mark censored observations.
#' @param lty.est linetype of the survival curve(s). Vector length should be
#'    either 1 or equal to the number of strata.
#' @param lty.ci linetype of the bounds that mark the 95\% CI.
#' @param cens.shape shape of the points that mark censored observations.
#' @param back.white if TRUE the background will not be the default
#'    grey of \code{ggplot2} but will be white with borders around the plot.
#' @param xlab the label of the x-axis.
#' @param ylab the label of the y-axis.
#' @param main the plot label.
#' @return An object of class \code{ggplot}
#' @author Edwin Thoen \email{edwinthoen@@gmail.com}
#' @importFrom stats time
#' @examples
#'
#' if (require(survival) && require(scales)) {
#'   data(lung, package = "survival")
#'   sf.lung <- survival::survfit(Surv(time, status) ~ 1, data = lung)
#'   ggsurv(sf.lung)
#'
#'   # Multiple strata examples
#'   sf.sex <- survival::survfit(Surv(time, status) ~ sex, data = lung)
#'   pl.sex <- ggsurv(sf.sex)
#'   pl.sex
#'
#'   # Adjusting the legend of the ggsurv fit
#'   pl.sex +
#'     ggplot2::guides(linetype = FALSE) +
#'     ggplot2::scale_colour_discrete(
#'       name   = 'Sex',
#'       breaks = c(1,2),
#'       labels = c('Male', 'Female')
#'     )
#'
#'   # We can still adjust the plot after fitting
#'   data(kidney, package = "survival")
#'   sf.kid <- survival::survfit(Surv(time, status) ~ disease, data = kidney)
#'   pl.kid <- ggsurv(sf.kid, plot.cens = FALSE)
#'   pl.kid
#'
#'   # Zoom in to first 80 days
#'   pl.kid <- pl.kid + ggplot2::coord_cartesian(xlim = c(0, 80), ylim = c(0.45, 1))
#'   pl.kid
#'
#'   # Add the diseases names to the plot and remove legend
#'   col <- scales::hue_pal(
#'     h         = c(0, 360) + 15,
#'     c         = 100,
#'     l         = 65,
#'     h.start   = 0,
#'     direction = 1
#'   )(4)
#'   pl.kid +
#'     ggplot2::annotate(
#'       "text",
#'       label  = c('AN', 'GN', 'Other', 'PKD'),
#'       x      = c(50, 20, 50, 71),
#'       y      = c(0.47, 0.55, 0.67, 0.8),
#'       size   = 5,
#'       colour = col
#'     ) +
#'     ggplot2::guides(color = FALSE, linetype = FALSE)
#' }
ggsurv <- function(
  s,
  CI         = 'def',
  plot.cens  = TRUE,
  surv.col   = 'gg.def',
  cens.col   = 'gg.def',
  lty.est    = 1,
  lty.ci     = 2,
  cens.shape = 3,
  back.white = FALSE,
  xlab       = 'Time',
  ylab       = 'Survival',
  main       = ''
){

  require_pkgs(c("survival", "scales"))

  strata <- ifelse(is.null(s$strata) == TRUE, 1, length(s$strata))
  stopifnot(length(surv.col) == 1 | length(surv.col) == strata)
  stopifnot(length(lty.est) == 1 | length(lty.est) == strata)

  if(strata == 1) {
    fn <- ggsurv_s
  } else {
    fn <- ggsurv_m
  }

  pl <- fn(
    s, CI , plot.cens, surv.col,
    cens.col, lty.est, lty.ci,
    cens.shape, back.white, xlab,
    ylab, main, strata
  )
  pl
}

# survival function for single survival
ggsurv_s <- function(
  s,
  CI         = 'def',
  plot.cens  = TRUE,
  surv.col   = 'gg.def',
  cens.col   = 'gg.def',
  lty.est    = 1,
  lty.ci     = 2,
  cens.shape = 3,
  back.white = FALSE,
  xlab       = 'Time',
  ylab       = 'Survival',
  main       = '',
  strata     = 1
){

  dat <- data.frame(
    time = c(0, s$time),
    surv = c(1, s$surv),
    up   = c(1, s$upper),
    low  = c(1, s$lower),
    cens = c(0, s$n.censor)
  )
  dat.cens <- subset(dat, cens != 0)

  col <- ifelse(surv.col == 'gg.def', 'black', surv.col)

  pl <- ggplot(dat, aes(x = time, y = surv)) +
    geom_step(col = col, lty = lty.est) +
    xlab(xlab) +
    ylab(ylab) +
    ggtitle(main)

  if(identical(CI, TRUE) | identical(CI, 'def')) {
    pl <- pl +
      geom_step(aes(y = up), color = col, lty = lty.ci) +
      geom_step(aes(y = low), color = col, lty = lty.ci)
  }

  if (identical(plot.cens, TRUE) ) {
    if (nrow(dat.cens) == 0){
      stop('There are no censored observations')
    }
    col <- ifelse(cens.col == 'gg.def', 'red', cens.col)

    pl <- pl + geom_point(
      data    = dat.cens,
      mapping = aes(y = surv),
      shape   = cens.shape,
      col     = col
    )
  }

  if(back.white == TRUE) {
    pl <- pl + theme_bw()
  }

  pl
}

# survival function for multiple survivals
ggsurv_m <- function(
  s,
  CI         = 'def',
  plot.cens  = TRUE,
  surv.col   = 'gg.def',
  cens.col   = 'gg.def',
  lty.est    = 1,
  lty.ci     = 2,
  cens.shape = 3,
  back.white = FALSE,
  xlab       = 'Time',
  ylab       = 'Survival',
  main       = '',
  strata     = length(s$strata)
) {
  n <- s$strata

  strataEqualNames <- unlist(strsplit(names(s$strata), '='))
  groups <- factor(
    strataEqualNames[seq(2, 2 * strata, by = 2)],
    levels = strataEqualNames[seq(2, 2 * strata, by = 2)]
  )

  gr.name <-  strataEqualNames[1]
  gr.df   <- vector('list', strata)
  n.ind   <- cumsum(c(0,n))

  for (i in 1:strata) {
    indI <- (n.ind[i]+1):n.ind[i+1]
    gr.df[[i]] <- data.frame(
      time  = c(0, s$time[ indI ]),
      surv  = c(1, s$surv[ indI ]),
      up    = c(1, s$upper[ indI ]),
      low   = c(1, s$lower[ indI ]),
      cens  = c(0, s$n.censor[ indI ]),
      group = rep(groups[i], n[i] + 1)
    )
  }

  dat      <- do.call(rbind, gr.df)
  dat.cens <- subset(dat, cens != 0)

  pl <- ggplot(dat, aes(x = time, y = surv, group = group)) +
    geom_step(aes(col = group, lty = group)) +
    xlab(xlab) +
    ylab(ylab) +
    ggtitle(main)

  pl <- if(surv.col[1] != 'gg.def'){
    scaleValues <- if (length(surv.col) == 1) {
      rep(surv.col, strata)
    } else{
      surv.col
    }
    pl + scale_colour_manual(name = gr.name, values = scaleValues)

  } else {
    pl + scale_colour_discrete(name = gr.name)
  }

  lineScaleValues <- if (length(lty.est) == 1) {
    rep(lty.est, strata)
  } else {
    lty.est
  }
  pl <- pl + scale_linetype_manual(name = gr.name, values = lineScaleValues)

  if(identical(CI,TRUE)) {
    if(length(surv.col) > 1 || length(lty.est) > 1){
      stop('Either surv.col or lty.est should be of length 1 in order to plot 95% CI with multiple strata')
    }

    stepLty <- if ((length(surv.col) > 1 | surv.col == 'gg.def')[1]) {
      lty.ci
    } else {
      surv.col
    }
    pl <- pl +
      geom_step(aes(y = up, lty = group), lty = stepLty) +
      geom_step(aes(y = low,lty = group), lty = stepLty)
  }

  if (identical(plot.cens, TRUE) ){
    if (nrow(dat.cens) == 0) {
      stop('There are no censored observations')
    }
    if (length(cens.col) == 1) {
      col <- ifelse(cens.col == 'gg.def', 'red', cens.col)
      pl <- pl + geom_point(
        data    = dat.cens,
        mapping = aes(y = surv),
        shape   = cens.shape,
        col     = col
      )

    } else if (length(cens.col) > 0) {
      # if(!(identical(cens.col,surv.col) || is.null(cens.col))) {
      #   warning ("Color scales for survival curves and censored points don't match.\nOnly one color scale can be used. Defaulting to surv.col")
      # }


      if (! identical(cens.col, "gg.def")) {
        if (length(cens.col) != strata) {
          warning("Color scales for censored points don't match the number of groups. Defaulting to ggplot2 default color scale")
          cens.col <- "gg.def"
        }
      }

      if (identical(cens.col, "gg.def")) {
        pl <- pl + geom_point(
          data = dat.cens,
          mapping = aes(y=surv, col = group),
          shape = cens.shape,
          show.legend = FALSE
        )
      } else {

        uniqueGroupVals = unique(dat.cens$group)
        if (length(cens.shape) == 1) {
          cens.shape = rep(cens.shape, strata)
        }

        if (length(cens.shape) != strata) {
          warning("The length of the censored shapes does not match the number of groups (or 1). Defaulting shape = 3 (+)")
          cens.shape = rep(3, strata)
        }
        for (i in seq_along(uniqueGroupVals)) {
          groupVal = uniqueGroupVals[i]
          dtGroup <- subset(dat.cens, group == groupVal)

          pl <- pl + geom_point(
            data = dtGroup,
            mapping = aes(y=surv),
            color = I(cens.col[i]),
            shape = cens.shape[i],
            show.legend = FALSE
          )

        }
      }

    }
  }

  if(identical(back.white, TRUE)) {
    pl <- pl + theme_bw()
  }

  pl
}
