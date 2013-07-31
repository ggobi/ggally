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
#' @param lty.ci linetype of the bounds that mark the 95% CI.
#' @param cens.shape shape of the points that mark censored observations.
#' @param back.white if TRUE the background will not be the default
#'    grey of \code{ggplot2} but will be white with borders around the plot.
#' @param xlab the label of the x-axis.
#' @param ylab the label of the y-axis.
#' @param main the plot label.
#' @return An object of class \code{ggplot}
#' @author Edwin Thoen <edwinthoen@@gmail.com>
#' @examples
#'
#' library(survival); data(lung)
#' sf.lung <- survfit(Surv(time, status) ~ 1, data = lung)
#' ggsurv(sf.lung)
#'
#' # Multiple strata examples
#' sf.sex <- survfit(Surv(time, status) ~ sex, data = lung)
#' (pl.sex <- ggsurv(sf.sex))
#'
#' # Adjusting the legend of the ggsurv fit
#' pl.sex +
#'   guides(linetype = F) +
#'   scale_colour_discrete(
#'     name   = 'Sex',
#'     breaks = c(1,2),
#'     labels = c('Male', 'Female')
#'   )
#'
#' # We can still adjust the plot after fitting
#' data(kidney)
#' sf.kid <- survfit(Surv(time, status) ~ disease, data = kidney)
#' (pl.kid <- ggsurv(sf.kid, plot.cens = F))
#'
#' # Zoom in to first 80 days
#' (pl.kid <-  pl.kid + xlim(c(0, 80)) + ylim(c(0.45, 1)))
#'
#' # Add the diseases names to the plot and remove legend
#' col <- scales::hue_pal(
#'   h         = c(0, 360) + 15,
#'   c         = 100,
#'   l         = 65,
#'   h.start   = 0,
#'   direction = 1
#' )(4)
#' pl.kid +
#'   annotate(
#'     "text",
#'     label  = c('AN', 'GN', 'Other', 'PKD'),
#'     x      = c(50, 20, 50, 71),
#'     y      = c(0.47, 0.55, 0.67, 0.8),
#'     size   = 5,
#'     colour = col
#'   ) +
#'   guides(color = F, linetype = F)
ggsurv <- function(
  s,
  CI         = 'def',
  plot.cens  = T,
  surv.col   = 'gg.def',
  cens.col   = 'red',
  lty.est    = 1,
  lty.ci     = 2,
  cens.shape = 3,
  back.white = F,
  xlab       = 'Time',
  ylab       = 'Survival',
  main       = ''
){

  require(ggplot2)

  strata <- ifelse(is.null(s$strata) ==T, 1, length(s$strata))
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
    ylab, main
  )
  pl
}


ggsurv_s <- function(s, CI = 'def', plot.cens = T, surv.col = 'gg.def',
                     cens.col = 'red', lty.est = 1, lty.ci = 2,
                     cens.shape = 3, back.white = F, xlab = 'Time',
                     ylab = 'Survival', main = ''){

  dat <- data.frame(time = c(0, s$time),
                    surv = c(1, s$surv),
                    up = c(1, s$upper),
                    low = c(1, s$lower),
                    cens = c(0, s$n.censor))
  dat.cens <- subset(dat, cens != 0)

  col <- ifelse(surv.col == 'gg.def', 'black', surv.col)

  pl <- ggplot(dat, aes(x = time, y = surv)) +
    xlab(xlab) + ylab(ylab) + ggtitle(main) +
    geom_step(col = col, lty = lty.est)

  pl <- if(CI == T | CI == 'def') {
    pl + geom_step(aes(y = up), color = col, lty = lty.ci) +
      geom_step(aes(y = low), color = col, lty = lty.ci)
  } else (pl)

  pl <- if(plot.cens == T & length(dat.cens) > 0){
    pl + geom_point(data = dat.cens, aes(y = surv), shape = cens.shape,
                     col = cens.col)
  } else if (plot.cens == T & length(dat.cens) == 0){
    stop ('There are no censored observations')
  } else(pl)

  pl <- if(back.white == T) {pl + theme_bw()
  } else (pl)
  pl
}

ggsurv_m <- function(s, CI = 'def', plot.cens = T, surv.col = 'gg.def',
                     cens.col = 'red', lty.est = 1, lty.ci = 2,
                     cens.shape = 3, back.white = F, xlab = 'Time',
                     ylab = 'Survival', main = '') {
  n <- s$strata

  groups <- factor(unlist(strsplit(names
                                   (s$strata), '='))[seq(2, 2*strata, by = 2)])
  gr.name <-  unlist(strsplit(names(s$strata), '='))[1]
  gr.df <- vector('list', strata)
  ind <- vector('list', strata)
  n.ind <- c(0,n); n.ind <- cumsum(n.ind)
  for(i in 1:strata) ind[[i]] <- (n.ind[i]+1):n.ind[i+1]

  for(i in 1:strata){
    gr.df[[i]] <- data.frame(
      time = c(0, s$time[ ind[[i]] ]),
      surv = c(1, s$surv[ ind[[i]] ]),
      up = c(1, s$upper[ ind[[i]] ]),
      low = c(1, s$lower[ ind[[i]] ]),
      cens = c(0, s$n.censor[ ind[[i]] ]),
      group = rep(groups[i], n[i] + 1))
  }

  dat <- do.call(rbind, gr.df)
  dat.cens <- subset(dat, cens != 0)

  pl <- ggplot(dat, aes(x = time, y = surv, group = group)) +
    xlab(xlab) + ylab(ylab) + ggtitle(main) +
    geom_step(aes(col = group, lty = group))

  col <- if(length(surv.col == 1)){
    scale_colour_manual(name = gr.name, values = rep(surv.col, strata))
  } else{
    scale_colour_manual(name = gr.name, values = surv.col)
  }

  pl <- if(surv.col[1] != 'gg.def'){
    pl + col
  } else {pl + scale_colour_discrete(name = gr.name)}

  line <- if(length(lty.est) == 1){
    scale_linetype_manual(name = gr.name, values = rep(lty.est, strata))
  } else {scale_linetype_manual(name = gr.name, values = lty.est)}

  pl <- pl + line

  pl <- if(CI == T) {
    if(length(surv.col) > 1 && length(lty.est) > 1){
      stop('Either surv.col or lty.est should be of length 1 in order
           to plot 95% CI with multiple strata')
    }else if((length(surv.col) > 1 | surv.col == 'gg.def')[1]){
      pl + geom_step(aes(y = up, color = group), lty = lty.ci) +
        geom_step(aes(y = low, color = group), lty = lty.ci)
    } else{pl +  geom_step(aes(y = up, lty = group), col = surv.col) +
             geom_step(aes(y = low,lty = group), col = surv.col)}
  } else {pl}


  pl <- if(plot.cens == T & length(dat.cens) > 0){
    pl + geom_point(data = dat.cens, aes(y = surv), shape = cens.shape,
                    col = cens.col)
  } else if (plot.cens == T & length(dat.cens) == 0){
    stop ('There are no censored observations')
  } else(pl)

  pl <- if(back.white == T) {pl + theme_bw()
  } else (pl)
  pl
}
