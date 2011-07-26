# Yaletoolkit version of gpairs

gpairs <- function (
  x, 
  upper.pars = list(scatter = "points", conditional = "barcode", mosaic = "mosaic"), 
  lower.pars = list(scatter = "points", conditional = "boxplot", mosaic = "mosaic"), 
  diagonal = "default", 
  outer.margins = list(bottom = unit(2, "lines"), 
  left = unit(2, "lines"), 
  top = unit(2, "lines"),
  right = unit(2, "lines")), 
  xylim = NULL, 
  outer.labels = NULL, 
  outer.rot = c(90, 0), 
  gap = 0.05, 
  buffer = 0.02, 
  reorder = NULL, 
  cluster.pars = NULL, 
  stat.pars = NULL, 
  scatter.pars = NULL, 
  bwplot.pars = NULL, 
  stripplot.pars = NULL, 
  barcode.pars = NULL, 
  mosaic.pars = NULL, 
  axis.pars = NULL, 
  diag.pars = NULL, 
  whatis = FALSE
) 
{
  if (!require(grid)) 
    stop("library(grid) is required and unavailable.\n\n")
  if (!require(lattice)) 
    stop("library(lattice) is required and unavailable.\n\n")
  if (!require(vcd)) 
    stop("library(vcd) is required and unavailable.\n\n")
  if (!is.data.frame(x)) {
    if (is.matrix(x)) 
      x <- as.data.frame(x)
    else stop("What did you give me? You might want to use Excel. (Only one column in argument to gpairs.\n\n")
  }
  
  
  zc <- function(x) length(unique(x)) <= 1
  
  if (any(sapply(x, zc), na.rm = TRUE)) {
    warning(paste(sum(sapply(x, zc), na.rm = TRUE), "columns with less than two distinct values eliminated"))
    x <- x[, !(sapply(x, zc))]
  }
  
  if (!is.null(lower.pars) & !is.list(lower.pars)) {
    warning("lower.pars is not a list, proceed with caution.")
  }
  
  if (!is.null(upper.pars) & !is.list(upper.pars)) {
    warning("upper.pars is not a list, proceed with caution.")
  }
  
  if (!is.null(reorder)) {
    if (pmatch(reorder, "cluster", nomatch = FALSE)) {
      if (is.null(cluster.pars)) {
        cluster.pars <- list(dist.method = "euclidean", 
          hclust.method = "complete")
      }
      x.num <- as.matrix(as.data.frame(lapply(x, as.numeric)))
      x.clust <- hclust(dist(t(x.num), method = cluster.pars$dist.method), 
        method = cluster.pars$hclust.method)
      x <- x[, x.clust$order]
    }
  }
  if (is.null(lower.pars$scatter.pars)) {
    lower.pars$scatter.pars <- "points"
  }
  if (is.null(lower.pars$conditional)) {
    lower.pars$conditional <- "boxplot"
  }
  if (is.null(lower.pars$mosaic)) {
    lower.pars$mosaic <- "mosaic"
  }
  if (is.null(upper.pars$scatter.pars)) {
    upper.pars$scatter.pars <- "points"
  }
  if (is.null(upper.pars$conditional)) {
    upper.pars$conditional <- "barcode"
  }
  if (is.null(upper.pars$mosaic)) {
    upper.pars$mosaic <- "mosaic"
  }
  if (!is.list(outer.margins)) {
    if (length(outer.margins) == 4) {
      if (is.unit(outer.margins[1])) {
        outer.margins <- list(bottom = outer.margins[1], 
          left = outer.margins[2], top = outer.margins[3], 
          right = outer.margins[4])
      }
      else {
        outer.margins <- list(bottom = unit(outer.margins[1], 
          "lines"), left = unit(outer.margins[2], "lines"), 
          top = unit(outer.margins[3], "lines"), right = unit(outer.margins[4], 
          "lines"))
      }
    }
    else {
      stop("outer.margins are not valid.")
    }
  }
  if (is.null(outer.labels)) {
    outer.labels$top <- rep(FALSE, ncol(x))
    outer.labels$top[seq(2, ncol(x), by = 2)] <- TRUE
    outer.labels$left <- rep(FALSE, ncol(x))
    outer.labels$left[seq(2, ncol(x), by = 2)] <- TRUE
    outer.labels$right <- !outer.labels$left
    outer.labels$bottom <- !outer.labels$top
  }
  else {
    if (pmatch(as.character(outer.labels), "all", nomatch = FALSE)) {
      all.labeling <- TRUE
    }
    else if (pmatch(as.character(outer.labels), "none", nomatch = FALSE)) {
      all.labeling <- FALSE
    }
    else {
      stop("argument to outer.labels not understood\n")
    }
    outer.labels <- NULL
    outer.labels$top <- rep(all.labeling, ncol(x))
    outer.labels$left <- rep(all.labeling, ncol(x))
    outer.labels$bottom <- rep(all.labeling, ncol(x))
    outer.labels$right <- rep(all.labeling, ncol(x))
  }
  if (is.null(stat.pars$fontsize)) {
    stat.pars$fontsize <- 7
  }
  if (is.null(stat.pars$signif)) {
    stat.pars$signif <- 0.05
  }
  if (is.null(stat.pars$verbose)) {
    stat.pars$verbose <- FALSE
  }
  if (is.null(stat.pars$use.color)) {
    stat.pars$use.color <- TRUE
  }
  if (is.null(stat.pars$missing)) {
    stat.pars$missing <- "missing"
  }
  if (is.null(stat.pars$just)) {
    stat.pars$just <- "centre"
  }
  if (is.null(scatter.pars$pch)) {
    scatter.pars$pch <- 1
  }
  if (is.null(scatter.pars$size)) {
    scatter.pars$size <- unit(0.25, "char")
  }
  if (is.null(scatter.pars$col)) {
    scatter.pars$col <- "black"
  }
  if (is.null(scatter.pars$plotpoints)) {
    scatter.pars$plotpoints <- TRUE
  }
  if (is.null(axis.pars$n.ticks)) {
    axis.pars$n.ticks <- 5
  }
  if (is.null(axis.pars$fontsize)) {
    axis.pars$fontsize <- 9
  }
  if (axis.pars$n.ticks < 3) {
    axis.pars$n.ticks <- 3
    warning("Fewer than 3 axis ticks might cause problems.")
  }
  if (is.null(diag.pars$fontsize)) {
    diag.pars$fontsize <- 9
  }
  if (is.null(diag.pars$show.hist)) {
    diag.pars$show.hist <- TRUE
  }
  if (is.null(diag.pars$hist.color)) {
    diag.pars$hist.color <- "black"
  }
  if (is.null(stripplot.pars$pch)) {
    stripplot.pars$pch <- 1
  }
  if (is.null(stripplot.pars$size)) {
    stripplot.pars$size <- unit(0.5, "char")
  }
  if (is.null(stripplot.pars$col)) {
    stripplot.pars$col <- "black"
  }
  if (is.null(stripplot.pars$jitter)) {
    stripplot.pars$jitter <- FALSE
  }
  if (is.null(barcode.pars$nint)) {
    barcode.pars$nint <- 0
  }
  if (is.null(barcode.pars$ptsize)) {
    barcode.pars$ptsize <- unit(0.25, "char")
  }
  if (is.null(barcode.pars$ptpch)) {
    barcode.pars$ptpch <- 1
  }
  if (is.null(barcode.pars$bcspace)) {
    barcode.pars$bcspace <- NULL
  }
  if (is.null(barcode.pars$use.points)) {
    barcode.pars$use.points <- FALSE
  }
  if (is.null(mosaic.pars$gp_labels)) {
    mosaic.pars$gp_labels <- gpar(fontsize = 9)
  }
  draw.axis <- function(x, y, axis.pars, xpos, ypos, cat.labels = NULL, 
    horiz = NULL, xlim = NULL, ylim = NULL) {
    x <- as.numeric(x)
    y <- as.numeric(y)
    if (is.null(xlim)) {
      px <- pretty(x, axis.pars$n.ticks)
      px <- px[px > min(x, na.rm = TRUE) & px < max(x, 
        na.rm = TRUE)]
    }
    else {
      px <- pretty(xlim, axis.pars$n.ticks)
      px <- px[px > min(xlim, na.rm = TRUE) & px < max(xlim, 
        na.rm = TRUE)]
    }
    if (is.null(ylim)) {
      py <- pretty(y, axis.pars$n.ticks)
      py <- py[py > min(y, na.rm = TRUE) & py < max(y, 
        na.rm = TRUE)]
    }
    else {
      py <- pretty(ylim, axis.pars$n.ticks)
      py <- py[py > min(ylim, na.rm = TRUE) & py < max(ylim, 
        na.rm = TRUE)]
    }
    k <- length(cat.labels)
    if (!is.null(xpos)) {
      if (!is.null(cat.labels) && !horiz) {
        grid.text(cat.labels, x = unit(1:k, "native"), 
          y = unit(rep(1 * (1 - xpos), k), "npc") + unit(rep(-1 * 
          xpos + 1 * (1 - xpos), k), "lines"), rot = outer.rot[2 - 
          xpos], gp = gpar(fontsize = axis.pars$fontsize))
      }
      else grid.xaxis(at = px, gp = gpar(fontsize = axis.pars$fontsize), 
        main = xpos)
    }
    if (!is.null(ypos)) {
      if (!is.null(cat.labels) && horiz) {
        grid.text(cat.labels, y = unit(1:k, "native"), 
          x = unit(rep(1 * (1 - ypos), k), "npc") + unit(rep(-1 * 
          ypos + 1 * (1 - ypos), k), "lines"), rot = outer.rot[2 - 
          ypos], gp = gpar(fontsize = axis.pars$fontsize))
      }
      else grid.yaxis(at = py, gp = gpar(fontsize = axis.pars$fontsize), 
        main = ypos)
    }
  }
  qq.panel <- function(x, y, scatter.pars, axis.pars, xpos, 
    ypos, xlim, ylim) {
    pushViewport(viewport(xscale = xlim, yscale = ylim))
    draw.axis(x, y, axis.pars, xpos, ypos, NULL, NULL, xlim, 
      ylim)
    popViewport(1)
    pushViewport(viewport(xscale = xlim, yscale = ylim, clip = TRUE))
    grid.rect(gp = gpar(fill = scatter.pars$frame.fill, col = scatter.pars$border.col))
    x <- sort(x)
    y <- sort(y)
    grid.lines(unit(x, "native"), unit(y, "native"))
    popViewport(1)
  }
  scatterplot.panel <- function(x, y, type, scatter.pars, axis.pars, 
    xpos, ypos, xylim) {
    if (is.null(xylim)) {
      xlim <- range(x, na.rm = TRUE) + c(-buffer * (max(x, 
        na.rm = TRUE) - min(x, na.rm = TRUE)), buffer * 
        (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
      ylim <- range(y, na.rm = TRUE) + c(-buffer * (max(y, 
        na.rm = TRUE) - min(y, na.rm = TRUE)), buffer * 
        (max(y, na.rm = TRUE) - min(y, na.rm = TRUE)))
    }
    else {
      xlim <- xylim
      ylim <- xylim
    }
    pushViewport(viewport(xscale = xlim, yscale = ylim))
    draw.axis(x, y, axis.pars, xpos, ypos, NULL, NULL, xlim, 
      ylim)
    popViewport(1)
    pushViewport(viewport(xscale = xlim, yscale = ylim, clip = TRUE))
    grid.rect(gp = gpar(fill = scatter.pars$frame.fill, col = scatter.pars$border.col))
    if (scatter.pars$plotpoints & (type == "points" || type == 
      "lm" || type == "ci" || type == "symlm" || type == 
      "loess")) {
      grid.points(x, y, pch = scatter.pars$pch, size = scatter.pars$size, 
        gp = gpar(col = scatter.pars$col))
    }
    if (type == "lm") {
      xy.lm <- lm(y ~ x)
      panel.abline(xy.lm$coef[1], xy.lm$coef[2], col = "red", 
        lwd = 2)
    }
    if (type == "ci") {
      xy.lm <- lm(y ~ x)
      xy <- data.frame(x = seq(min(x, na.rm = TRUE), max(x, 
        na.rm = TRUE), length.out = 20))
      yhat <- predict(xy.lm, newdata = xy, interval = "confidence")
      ci <- data.frame(lower = yhat[, "lwr"], upper = yhat[, 
        "upr"])
      grid.lines(x = c(xy$x), y = c(ci$lower), default.units = "native")
      grid.lines(x = c(xy$x), y = c(ci$upper), default.units = "native")
      grid.polygon(x = c(xy$x, xy$x[length(xy$x):1]), y = c(ci$lower, 
        ci$upper[length(ci$upper):1]), gp = gpar(fill = "grey"), 
        default.units = "native")
    }
    if (type == "loess") {
      junk <- try(panel.loess(x, y, color = "red", span = 1))
      if (class(junk) == "try-error") 
        warning("An error in loess occurred and was ignored; no line was plotted.")
    }
    if (type == "symlm") {
      pcs <- try(prcomp(cbind(x, y)))
      if (class(pcs) == "try-error") 
        warning("An error in symlm occurred and was ignored; no line was plotted.")
      else {
        slope <- abs(pcs$rotation[1, 2]/pcs$rotation[1, 
          1])
        if (cor(x, y) < 0) 
          slope <- -1 * slope
        panel.abline(pcs$center[2] - slope * pcs$center[1], 
          slope, col = "blue")
      }
    }
    if (type == "corrgram") {
      pear.test <- cor.test(x, y, method = "pearson", alternative = "two.sided")
      corr <- format(pear.test$estimate, digits = 2)
      if (as.numeric(corr) > 0) {
        panel.fill(col = hsv(h = 0.5, s = abs(as.numeric(corr)), 
          v = 1), border = hsv(h = 0.5, s = abs(as.numeric(corr)), 
          v = 1))
        grid.lines(x = unit(c(0, 1), "npc"), y = unit(c(0, 
          1), "npc"), gp = gpar(col = "white", lwd = 2))
      }
      else {
        panel.fill(col = hsv(h = 0, s = abs(as.numeric(corr)), 
          v = 1), border = hsv(h = 0, s = abs(as.numeric(corr)), 
          v = 1))
        grid.lines(x = unit(c(0, 1), "npc"), y = unit(c(1, 
          0), "npc"), gp = gpar(col = "white", lwd = 2))
      }
    }
    if (type == "qqplot") {
      qq.panel(x, y, scatter.pars, axis.pars, xpos, ypos, 
        xlim, ylim)
    }
    if (type == "stats") {
      complete.obs <- nrow(na.omit(cbind(x, y)))
      missing <- length(x) - complete.obs
      pear.test <- cor.test(x, y, method = "pearson", alternative = "two.sided")
      corr <- sprintf("%03.2f", pear.test$estimate)
      rho.test <- cor.test(x, y, method = "spearman", alternative = "two.sided")
      tau.test <- cor.test(x, y, method = "kendall", alternative = "two.sided")
      rho <- sprintf("%03.2f", rho.test$estimate)
      tau <- sprintf("%03.2f", tau.test$estimate)
      xy.lm <- lm(y ~ x)
      r2 <- sprintf("%03.2f", summary(xy.lm)$r.squared)
      p <- sprintf("%06.4f", pf(q = as.numeric(summary(xy.lm)$fstatistic)[1], 
        df1 = as.numeric(summary(lm(xy.lm))$fstatistic)[2], 
        df2 = as.numeric(summary(lm(xy.lm))$fstatistic)[3], 
        lower.tail = FALSE))
      bonfp <- stat.pars$signif/(N * (N - 1))/2
      sig <- 1
      sigrho <- NULL
      sigtau <- NULL
      sigcor <- NULL
      sigp <- NULL
      if (pear.test$p.value < bonfp) {
        sig <- sig + 1
        sigcor <- "*"
      }
      if (rho.test$p.value < bonfp) {
        sig <- sig + 1
        sigrho <- "*"
      }
      if (tau.test$p.value < bonfp) {
        sig <- sig + 1
        sigtau <- "*"
      }
      if (as.numeric(p) < bonfp) {
        sig <- sig + 1
        sigp <- "*"
      }
      if (mean(as.numeric(rho), as.numeric(tau), as.numeric(corr)) > 
        0) {
        text.color <- "black"
        if (sig == 1) 
          box.color <- 0.5
        else if (sig > 1 && sig < 5) 
          box.color <- 0.75
        else if (sig == 5) 
          box.color <- 1
      }
      else if (mean(as.numeric(rho), as.numeric(tau), as.numeric(corr)) < 
        0) {
        text.color <- "white"
        if (sig == 1) 
          box.color <- 0.5
        else if (sig > 1 && sig < 5) 
          box.color <- 0.25
        else if (sig == 5) 
          box.color <- 0
      }
      if (!stat.pars$use.color) {
        panel.fill(col = grey(box.color), border = grey(box.color))
      }
      else {
        text.color <- "black"
        if (as.numeric(corr) > 0) {
          panel.fill(col = hsv(h = 0.5, s = abs(as.numeric(corr)), 
          v = 1), border = hsv(h = 0.5, s = abs(as.numeric(corr)), 
          v = 1))
        }
        else {
          panel.fill(col = hsv(h = 0, s = abs(as.numeric(corr)), 
          v = 1), border = hsv(h = 0, s = abs(as.numeric(corr)), 
          v = 1))
        }
      }
      if (!is.na(stat.pars$verbose)) {
        if (stat.pars$verbose == TRUE) {
          grid.text(bquote(rho == .(rho) * .(sigrho)), 
          x = 0.5, y = 0.9, just = stat.pars$just, 
          gp = gpar(fontsize = stat.pars$fontsize, 
            col = text.color))
          grid.text(bquote(tau == .(tau) * .(sigtau)), 
          x = 0.5, y = 0.7, just = stat.pars$just, 
          gp = gpar(fontsize = stat.pars$fontsize, 
            col = text.color))
          grid.text(paste("r=", corr, sigcor, sep = ""), 
          x = 0.5, y = 0.5, just = stat.pars$just, 
          gp = gpar(fontsize = stat.pars$fontsize, 
            col = text.color))
          grid.text(paste("p=", p, sigp, sep = ""), x = 0.5, 
          y = 0.3, just = stat.pars$just, gp = gpar(fontsize = stat.pars$fontsize, 
            col = text.color))
          if (missing > 0) 
          grid.text(paste(missing, stat.pars$missing), 
            x = 0.5, y = 0.1, just = stat.pars$just, 
            gp = gpar(fontsize = stat.pars$fontsize, 
            col = "red"))
        }
        else {
          grid.text(paste(corr, sigcor, sep = ""), x = 0.5, 
          y = 0.7, just = stat.pars$just, gp = gpar(fontsize = stat.pars$fontsize, 
            col = text.color))
          if (missing > 0) 
          grid.text(paste(missing, "missing"), x = 0.5, 
            y = 0.3, just = stat.pars$just, gp = gpar(fontsize = stat.pars$fontsize, 
            col = text.color))
        }
      }
    }
    popViewport(1)
  }
  mosaic.panel <- function(x, y, mosaic.pars, axis.pars, xpos, 
    ypos) {
    if (!is.null(xpos) & !is.null(ypos)) {
      strucplot(table(y, x), margins = c(0, 0, 0, 0), newpage = FALSE, 
        pop = FALSE, keep_aspect_ratio = FALSE, shade = mosaic.pars$shade, 
        legend = FALSE, labeling_args = list(tl_labels = c(xpos, 
          !ypos), gp_labels = mosaic.pars$gp_labels, 
          varnames = c(FALSE, FALSE), rot_labels = c(outer.rot, 
          outer.rot)))
    }
    else {
      if (is.null(xpos) & is.null(ypos)) {
        strucplot(table(y, x), margins = c(0, 0, 0, 0), 
          shade = mosaic.pars$shade, legend = FALSE, 
          newpage = FALSE, pop = FALSE, keep_aspect_ratio = FALSE, 
          labeling = NULL)
      }
      else {
        if (is.null(xpos)) {
          strucplot(table(y, x), margins = c(0, 0, 0, 
          0), newpage = FALSE, pop = FALSE, keep_aspect_ratio = FALSE, 
          shade = mosaic.pars$shade, legend = FALSE, 
          labeling_args = list(labels = c(TRUE, FALSE), 
            tl_labels = c(ypos, FALSE), gp_labels = mosaic.pars$gp_labels, 
            varnames = c(FALSE, FALSE), rot_labels = c(outer.rot, 
            outer.rot)))
        }
        else {
          strucplot(table(y, x), margins = c(0, 0, 0, 
          0), newpage = FALSE, pop = FALSE, keep_aspect_ratio = FALSE, 
          shade = mosaic.pars$shade, legend = FALSE, 
          labeling_args = list(labels = c(FALSE, TRUE), 
            tl_labels = c(FALSE, !xpos), gp_labels = mosaic.pars$gp_labels, 
            varnames = c(FALSE, FALSE), rot_labels = c(outer.rot, 
            outer.rot)))
        }
      }
    }
  }
  boxplot.panel <- function(x, y, type, axis.pars, xpos, ypos, 
    xylim) {
    xlim <- NULL
    ylim <- NULL
    old.color <- trellis.par.get("box.rectangle")$col
    trellis.par.set(name = "box.rectangle", value = list(col = "black"))
    trellis.par.set(name = "box.umbrella", value = list(col = "black"))
    trellis.par.set(name = "box.dot", value = list(col = "black"))
    trellis.par.set(name = "plot.symbol", value = list(col = "black"))
    if (is.factor(x)) {
      cat.labels <- levels(x)
      k <- length(levels(x))
      cat.var <- as.numeric(x)
      cont.var <- y
      horiz <- FALSE
    }
    else {
      cat.labels <- levels(y)
      k <- length(levels(y))
      cat.labels <- cat.labels[k:1]
      cat.var <- k + 1 - as.numeric(y)
      cont.var <- x
      horiz <- TRUE
    }
    if (horiz) {
      if (is.null(xylim)) {
        xlim <- range(cont.var, na.rm = TRUE) + c(-buffer * 
          (max(cont.var, na.rm = TRUE) - min(cont.var, 
          na.rm = TRUE)), buffer * (max(cont.var, na.rm = TRUE) - 
          min(cont.var, na.rm = TRUE)))
      }
      else {
        xlim <- xylim
      }
      pushViewport(viewport(xscale = xlim, yscale = c(0.5, 
        max(cat.var, na.rm = TRUE) + 0.5)))
      if (is.null(ypos)) 
        cat.labels <- NULL
      draw.axis(cont.var, cat.var, axis.pars, xpos, ypos, 
        cat.labels, horiz, xlim, ylim)
      popViewport(1)
      pushViewport(viewport(xscale = xlim, yscale = c(0.5, 
        max(cat.var, na.rm = TRUE) + 0.5), clip = TRUE))
      if (type == "boxplot") 
        panel.bwplot(cont.var, cat.var, horizontal = horiz, 
          col = "black", pch = "|", gp = gpar(box.umbrella = list(col = "black")))
      if (type == "stripplot") 
        panel.stripplot(cont.var, cat.var, horizontal = horiz, 
          jitter = stripplot.pars$jitter, col = stripplot.pars$col, 
          cex = stripplot.pars$size, pch = stripplot.pars$pch)
    }
    else {
      if (is.null(xylim)) {
        ylim <- range(cont.var, na.rm = TRUE) + c(-buffer * 
          (max(cont.var, na.rm = TRUE) - min(cont.var, 
          na.rm = TRUE)), buffer * (max(cont.var, na.rm = TRUE) - 
          min(cont.var, na.rm = TRUE)))
      }
      else {
        ylim <- xylim
      }
      pushViewport(viewport(yscale = ylim, xscale = c(0.5, 
        max(cat.var, na.rm = TRUE) + 0.5)))
      if (is.null(xpos)) 
        cat.labels <- NULL
      draw.axis(cat.var, cont.var, axis.pars, xpos, ypos, 
        cat.labels, horiz, xlim, ylim)
      popViewport(1)
      pushViewport(viewport(yscale = ylim, xscale = c(0.5, 
        max(cat.var, na.rm = TRUE) + 0.5), clip = TRUE))
      if (type == "boxplot") 
        panel.bwplot(cat.var, cont.var, horizontal = horiz, 
          col = "black", pch = "|", gp = gpar(box.umbrella = list(col = "black")))
      if (type == "stripplot") 
        panel.stripplot(cat.var, cont.var, horizontal = horiz, 
          jitter = stripplot.pars$jitter, col = stripplot.pars$col, 
          cex = stripplot.pars$size, pch = stripplot.pars$pch)
    }
    grid.rect(gp = gpar(fill = NULL))
    popViewport(1)
    trellis.par.set(name = "box.rectangle", value = list(col = old.color))
    trellis.par.set(name = "box.umbrella", value = list(col = old.color))
    trellis.par.set(name = "box.dot", value = list(col = old.color))
    trellis.par.set(name = "plot.symbol", value = list(col = old.color))
  }
  diag.panel <- function(x, varname, diag.pars, axis.pars, 
    xpos, ypos, xylim) {
    x <- x[!is.na(x)]
    if (is.null(xylim)) {
      xlim <- range(as.numeric(x), na.rm = TRUE) + c(-buffer * 
        (max(as.numeric(x), na.rm = TRUE) - min(as.numeric(x), 
          na.rm = TRUE)), buffer * (max(as.numeric(x), 
        na.rm = TRUE) - min(as.numeric(x), na.rm = TRUE)))
    }
    else {
      xlim <- xylim
    }
    ylim <- xlim
    pushViewport(viewport(xscale = xlim, yscale = ylim))
    draw.axis(as.numeric(x), as.numeric(x), axis.pars, xpos, 
      ypos, NULL, NULL, xlim, ylim)
    popViewport(1)
    pushViewport(viewport(xscale = xlim, yscale = ylim, clip = TRUE))
    if (!diag.pars$show.hist) {
      grid.rect()
      grid.text(varname, 0.5, 0.5, gp = gpar(fontsize = diag.pars$fontsize, 
        fontface = 2))
    }
    popViewport(1)
    if (diag.pars$show.hist) {
      if (!is.factor(x)) {
        pushViewport(viewport(xscale = xlim, yscale = c(0, 
          100), clip = TRUE))
      }
      else {
        pushViewport(viewport(xscale = c(min(as.numeric(x), 
          na.rm = TRUE) - 1, max(as.numeric(x), na.rm = TRUE) + 
          1), yscale = c(0, 100), clip = TRUE))
      }
      panel.histogram(as.numeric(x), breaks = NULL, type = "percent", 
        col = diag.pars$hist.color)
      grid.text(varname, 0.5, 0.85, gp = gpar(fontsize = diag.pars$fontsize))
      popViewport(1)
    }
  }
  grid.newpage()
  N <- ncol(x)
  vp.main <- viewport(x = outer.margins$bottom, y = outer.margins$left, 
    w = unit(1, "npc") - outer.margins$right - outer.margins$left, 
    h = unit(1, "npc") - outer.margins$top - outer.margins$bottom, 
    just = c("left", "bottom"), name = "main", clip = "off")
  pushViewport(vp.main)
  for (i in 1:N) {
    for (j in 1:N) {
      if (diagonal == "default") 
        labelj <- j
      else labelj <- N - j + 1
      x[is.infinite(x[, i]), i] <- NA
      x[is.infinite(x[, j]), j] <- NA
      vp <- viewport(x = (labelj - 1)/N, y = 1 - i/N, w = 1/N, 
        h = 1/N, just = c("left", "bottom"), name = as.character(i * 
          N + j))
      pushViewport(vp)
      vp.in <- viewport(x = 0.5, y = 0.5, w = 1 - gap, 
        h = 1 - gap, just = c("center", "center"), name = paste("IN", 
          as.character(i * N + j)))
      pushViewport(vp.in)
      xpos <- NULL
      if (i == 1 && outer.labels$top[j]) {
        xpos <- FALSE
      }
      if (i == N && outer.labels$bottom[j]) {
        xpos <- TRUE
      }
      ypos <- NULL
      if (j == N && outer.labels$right[i]) {
        ypos <- FALSE
      }
      if (j == 1 && outer.labels$left[i]) {
        ypos <- TRUE
      }
      if (!is.null(ypos) & diagonal != "default") {
        ypos <- !ypos
      }
      if (i == j) {
        diag.panel(x[, i], names(x)[i], diag.pars, axis.pars, 
          xpos, ypos, xylim)
      }
      else {
        if (is.factor(x[, i]) + is.factor(x[, j]) == 
          1) {
          if (i < j & upper.pars$conditional != "barcode") 
          boxplot.panel(x[, j], x[, i], upper.pars$conditional, 
            axis.pars, xpos, ypos, xylim)
          if (i > j & lower.pars$conditional != "barcode") 
          boxplot.panel(x[, j], x[, i], lower.pars$conditional, 
            axis.pars, xpos, ypos, xylim)
          if (i < j & upper.pars$conditional == "barcode") {
          if (is.factor(x[, i])) {
            barcode(split(x[, j], x[, i])[length(levels(x[, 
            i])):1], horizontal = TRUE, xlim = xylim, 
            labelloc = ypos, axisloc = xpos, labelouter = TRUE, 
            newpage = FALSE, fontsize = axis.pars$fontsize, 
            buffer = buffer, nint = barcode.pars$nint, 
            ptsize = barcode.pars$ptsize, ptpch = barcode.pars$ptpch, 
            bcspace = barcode.pars$bcspace, use.points = barcode.pars$use.points)
          }
          else {
            if (!is.null(ypos)) 
            ypos <- !ypos
            barcode(split(x[, i], x[, j])[length(levels(x[, 
            j])):1], horizontal = FALSE, xlim = xylim, 
            labelloc = xpos, axisloc = ypos, labelouter = TRUE, 
            newpage = FALSE, fontsize = axis.pars$fontsize, 
            buffer = buffer, nint = barcode.pars$nint, 
            ptsize = barcode.pars$ptsize, ptpch = barcode.pars$ptpch, 
            bcspace = barcode.pars$bcspace, use.points = barcode.pars$use.points)
          }
          }
          if (i > j & lower.pars$conditional == "barcode") {
          if (is.factor(x[, i])) {
            barcode(split(x[, j], x[, i])[length(levels(x[, 
            i])):1], horizontal = TRUE, xlim = xylim, 
            labelloc = ypos, axisloc = xpos, labelouter = TRUE, 
            newpage = FALSE, fontsize = axis.pars$fontsize, 
            buffer = buffer, nint = barcode.pars$nint, 
            ptsize = barcode.pars$ptsize, ptpch = barcode.pars$ptpch, 
            bcspace = barcode.pars$bcspace, use.points = barcode.pars$use.points)
          }
          else {
            if (!is.null(ypos)) 
            ypos <- !ypos
            barcode(split(x[, i], x[, j])[length(levels(x[, 
            j])):1], horizontal = FALSE, xlim = xylim, 
            labelloc = xpos, axisloc = ypos, labelouter = TRUE, 
            newpage = FALSE, fontsize = axis.pars$fontsize, 
            buffer = buffer, nint = barcode.pars$nint, 
            ptsize = barcode.pars$ptsize, ptpch = barcode.pars$ptpch, 
            bcspace = barcode.pars$bcspace, use.points = barcode.pars$use.points)
          }
          }
        }
        if (is.factor(x[, i]) + is.factor(x[, j]) == 
          0) {
          if (i < j) 
          type <- upper.pars$scatter
          else type <- lower.pars$scatter
          scatterplot.panel(x[, j], x[, i], type, scatter.pars, 
          axis.pars, xpos, ypos, xylim)
        }
        if (is.factor(x[, i]) + is.factor(x[, j]) == 
          2) {
          if (i < j) 
          mosaic.panel(x[, j], x[, i], mosaic.pars, 
            axis.pars, xpos, ypos)
          else mosaic.panel(x[, j], x[, i], mosaic.pars, 
          axis.pars, xpos, ypos)
        }
      }
      popViewport(1)
      upViewport()
    }
  }
  popViewport()
  if (whatis) 
    whatis(x)
}
