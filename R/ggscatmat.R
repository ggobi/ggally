if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("xvalue", "yvalue"))
}


#' lowertriangle - rearrange dataset as the preparation of \code{\link{ggscatmat}} function
#'
#' function for making the melted dataset used to plot the lowertriangle scatterplots.
#'
#' @export
#' @param data a data matrix. Should contain numerical (continuous) data.
#' @param columns an option to choose the column to be used in the raw dataset. Defaults to \code{1:ncol(data)}
#' @param color an option to choose a factor variable to be grouped with. Defaults to \code{(NULL)}
#' @author Mengjia Ni, Di Cook
#' @examples
#' data(flea)
#' head(lowertriangle(flea, columns= 2:4))
#' head(lowertriangle(flea))
#' head(lowertriangle(flea, color="species"))
lowertriangle <- function(data, columns=1:ncol(data), color=NULL) {
  # why do  we need to ocheck this again?
  # data <- upgrade_scatmat_data(data)
  data.choose <- data[columns]
  dn <- data.choose[sapply(data.choose, is.numeric)]
  factor <- data[sapply(data, is.factor)]
  p <- ncol(dn)
  q <- nrow(dn)
  newdata      <- as.data.frame(matrix(NA, nrow = q*p*p, ncol = 6+ncol(factor)), stringsAsFactors = FALSE)
  newdata[5:6] <- as.data.frame(matrix("", nrow = q*p*p, ncol = 2), stringsAsFactors = FALSE)

  r <-1
  for (i in 1:p) {
    for (j in 1:p) {
      newdata[r:(r+q-1), 1:6] <- cbind(dn[[i]], dn[[j]], i, j, colnames(dn)[i], colnames(dn)[j])
      r <- r+q
    }
  }

  if (ncol(newdata) > 6){newdata[7:ncol(newdata)] <- factor}
  colnames(newdata) <- c("xvalue", "yvalue", "xslot", "yslot", "xlab", "ylab", colnames(factor))

  rp <- data.frame(newdata)
  rp[[2]][rp[[3]] >= rp[[4]]] <- "NA"
  rp[[1]][rp[[3]] > rp[[4]]] <- "NA"

  rp$xvalue <- suppressWarnings(as.numeric(as.character(rp$xvalue)))
  rp$yvalue <- suppressWarnings(as.numeric(as.character(rp$yvalue)))
  rp$xslot  <- suppressWarnings(as.numeric(as.character(rp$xslot)))
  rp$yslot  <- suppressWarnings(as.numeric(as.character(rp$yslot)))
  rp$xlab <- factor(rp$xlab, levels = unique(rp$xlab))
  rp$ylab <- factor(rp$ylab, levels = unique(rp$ylab))

  if (is.null(color)){
    rp.new <- rp[1:6]
  } else {
    colorcolumn <- rp[[which(colnames(rp) == color)]]
    rp.new <- cbind(rp[1:6], colorcolumn)
  }
  return(rp.new)
}

#' Rearrange dataset as the preparation of \code{\link{ggscatmat}} function
#'
#' Function for making the dataset used to plot the uppertriangle plots.
#'
#' @export
#' @param data a data matrix. Should contain numerical (continuous) data.
#' @param columns an option to choose the column to be used in the raw dataset. Defaults to \code{1:ncol(data)}
#' @param color an option to choose a factor variable to be grouped with. Defaults to \code{(NULL)}
#' @param corMethod method argument supplied to \code{\link[stats]{cor}}
#' @author Mengjia Ni, Di Cook
#' @importFrom stats cor
#' @examples
#' data(flea)
#' head(uppertriangle(flea, columns=2:4))
#' head(uppertriangle(flea))
#' head(uppertriangle(flea, color="species"))
uppertriangle <- function(data, columns=1:ncol(data), color=NULL, corMethod = "pearson") {
  # data <- upgrade_scatmat_data(data)
  data.choose <- data[columns]
  # why do  we need to ocheck this again?
  dn <- data.choose[sapply(data.choose, is.numeric)]
  factor <- data[sapply(data, is.factor)]
  p <- ncol(dn)
  newdata <- NULL
  for (i in 1:p) {
    for (j in 1:p) {
      newdata <- rbind(newdata,
                       cbind(dn[, i], dn[, j], i, j, colnames(dn)[i], colnames(dn)[j],
                             min(dn[, i]) + 0.5 * (max(dn[, i]) - min(dn[, i])),
                             min(dn[, j]) + 0.5 * (max(dn[, j]) - min(dn[, j])), factor)
      )
    }
  }
  colnames(newdata) <- c(
    "xvalue", "yvalue",
    "xslot", "yslot",
    "xlab", "ylab",
    "xcenter", "ycenter",
    colnames(factor)
  )

  rp <- data.frame(newdata, stringsAsFactors = TRUE)
  rp[[2]][rp[[3]] <= rp[[4]]] <- "NA"
  rp[[1]][rp[[3]] < rp[[4]]] <- "NA"

  rp$xvalue <- suppressWarnings(as.numeric(as.character(rp$xvalue)))
  rp$yvalue <- suppressWarnings(as.numeric(as.character(rp$yvalue)))

  if (is.null(color)){
    rp.new <- rp[1:8]
  }else{
    colorcolumn <- rp[[which(colnames(rp) == color)]]
    rp.new <- cbind(rp[1:8],  colorcolumn)
  }
  a <- rp.new
  b <- subset(a, (a$yvalue != "NA") & (a$xvalue != "NA"))
  b$xlab <- factor(b$xlab, levels=unique(b$xlab))
  b$ylab <- factor(b$ylab, levels=unique(b$ylab))
  if (is.null(color)){
    data.cor <- ddply(
      b, .(xlab, ylab),
      function(subsetDt) {
        xlab <- subsetDt$xlab
        ylab <- subsetDt$ylab
        xvalue <- subsetDt$xvalue
        yvalue <- subsetDt$yvalue

        if (identical(corMethod, "rsquare")) {
          r <- cor(
            xvalue, yvalue,
            use = "pairwise.complete.obs",
            method = "pearson"
          )
          r <- r ^ 2
        } else {
          r <- cor(
            xvalue, yvalue,
            use = "pairwise.complete.obs",
            method = corMethod
          )
        }
        r <- paste(round(r, digits = 2))

        data.frame(
          xlab = unique(xlab), ylab = unique(ylab),
          r = r,
          xvalue = min(xvalue) + 0.5 * (max(xvalue) - min(xvalue)),
          yvalue = min(yvalue) + 0.5 * (max(yvalue) - min(yvalue))
        )
      }
    )
    return(data.cor)

  }else{
    c <- b
    data.cor1 <- ddply(
      c, .(ylab, xlab, colorcolumn),
      function(subsetDt) {
        xlab <- subsetDt$xlab
        ylab <- subsetDt$ylab
        colorcolumn <- subsetDt$colorcolumn
        xvalue <- subsetDt$xvalue
        yvalue <- subsetDt$yvalue

        if (identical(corMethod, "rsquare")) {
          r <- cor(
            xvalue, yvalue,
            use = "pairwise.complete.obs",
            method = "pearson"
          )
          r <- r ^ 2
        } else {
          r <- cor(
            xvalue, yvalue,
            use = "pairwise.complete.obs",
            method = corMethod
          )
        }
        r <- paste(round(r, digits = 2))
        data.frame(
          ylab = unique(ylab), xlab = unique(xlab), colorcolumn = unique(colorcolumn),
          r = r
        )
      }
    )

    n <- nrow(data.frame(unique(b$colorcolumn)))
    position <- ddply(b, .(ylab, xlab), summarise,
                      xvalue = min(xvalue) + 0.5 * (max(xvalue) - min(xvalue)),
                      ymin = min(yvalue),
                      ymax = max(yvalue),
                      range = max(yvalue) - min(yvalue))
    df <- data.frame()
    for (i in 1:nrow(position)) {
      for (j in 1:n){
        row <- position[i, ]
        df <- rbind(df, cbind(row[, 3], (row[, 4] + row[, 6] * j / (n + 1))))
      }
    }
    data.cor <- cbind(data.cor1, df)
    colnames(data.cor) <- c("ylab", "xlab", "colorcolumn", "r", "xvalue", "yvalue")
    return(data.cor)
  }
}

#' Plots the lowertriangle and density plots of the scatter plot matrix.
#'
#' Function for making scatterplots in the lower triangle and diagonal density plots.
#'
#' @export
#' @param data a data matrix. Should contain numerical (continuous) data.
#' @param columns an option to choose the column to be used in the raw dataset. Defaults to \code{1:ncol(data)}
#' @param color an option to group the dataset by the factor variable and color them by different colors. Defaults to \code{NULL}
#' @param alpha an option to set the transparency in scatterplots for large data. Defaults to \code{1}.
#' @author Mengjia Ni, Di Cook
#' @examples
#' # small function to display plots only if it's interactive
#' p_ <- GGally::print_if_interactive
#'
#' data(flea)
#'
#' p_(scatmat(flea, columns=2:4))
#' p_(scatmat(flea, columns= 2:4, color="species"))
scatmat <- function(data, columns=1:ncol(data), color=NULL, alpha=1) {
  # data <- upgrade_scatmat_data(data)
  data.choose <- data[columns]
  dn <- data.choose[sapply(data.choose, is.numeric)]
  if (ncol(dn) == 0) {
    stop("All of your variables are factors. Need numeric variables to make scatterplot matrix.")
  } else {
     ltdata.new <- lowertriangle(data, columns = columns, color = color)
     ## set up the plot
    r <- ggplot(ltdata.new, mapping = aes_string(x = "xvalue", y = "yvalue")) +
      theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
      facet_grid(ylab ~ xlab, scales = "free") +
      theme(aspect.ratio = 1)
    if (is.null(color)) {
       ## b/w version
      densities <- do.call("rbind", lapply(1:ncol(dn), function(i) {
        data.frame(xlab = names(dn)[i], ylab = names(dn)[i],
                   x = dn[, i], stringsAsFactors = TRUE)
      }))
      for (m in 1:ncol(dn)) {
        j <- subset(densities, xlab == names(dn)[m])
        r <- r + stat_density(
          aes(
            x = x,
            y = ..scaled.. * diff(range(x)) + min(x) # nolint
          ),
          data = j, position = "identity", geom = "line", color = "black")
      }
       ## add b/w points
      r <- r + geom_point(alpha = alpha, na.rm = TRUE)
      return(r)
    } else {
       ## do the colored version
      densities <- do.call("rbind", lapply(1:ncol(dn), function(i) {
        data.frame(xlab = names(dn)[i], ylab = names(dn)[i],
                   x = dn[, i], colorcolumn = data[, which(colnames(data) == color)],
                   stringsAsFactors = TRUE)
      }))
      for (m in 1:ncol(dn)) {
        j <- subset(densities, xlab == names(dn)[m])
        r <- r +
                           # r is the facet grid plot
          stat_density(
            aes_string(
              x = "x", y = "..scaled.. * diff(range(x)) + min(x)",
              colour = "colorcolumn"
            ),
            data = j,
            position = "identity",
            geom = "line"
          )
      }
      ## add color points
      r <- r +
        geom_point(
          data = ltdata.new,
          aes_string(colour = "colorcolumn"),
          alpha = alpha,
          na.rm = TRUE
        )
      return(r)
    }
  }
}

#'Traditional scatterplot matrix for purely quantitative variables
#'
#' This function makes a scatterplot matrix for quantitative variables with density plots on the diagonal
#' and correlation printed in the upper triangle.
#'
#' @export
#' @param data a data matrix. Should contain numerical (continuous) data.
#' @param columns an option to choose the column to be used in the raw dataset. Defaults to \code{1:ncol(data)}.
#' @param color an option to group the dataset by the factor variable and color them by different colors.
#'   Defaults to \code{NULL}, i.e. no coloring. If supplied, it will be converted to a factor.
#' @param alpha an option to set the transparency in scatterplots for large data. Defaults to \code{1}.
#' @param corMethod method argument supplied to \code{\link[stats]{cor}}
#' @author Mengjia Ni, Di Cook
#' @examples
#' # small function to display plots only if it's interactive
#' p_ <- GGally::print_if_interactive
#'
#' data(flea)
#'
#' p_(ggscatmat(flea, columns = 2:4))
#' p_(ggscatmat(flea, columns = 2:4, color = "species"))
ggscatmat <- function(data, columns = 1:ncol(data), color = NULL, alpha = 1, corMethod = "pearson"){
  ## if 'color' is not a factor, mold it into one
  if (!is.null(color)) {
     if (is.null(data[[color]])) {
        stop(paste0("Non-existent column <", color, "> requested"))
     }
     data[[color]] <- as.factor(data[[color]])
  }
  ## do we really need this next line?
  data <- upgrade_scatmat_data(data)
  data.choose <- data[columns]
  dn <- data.choose[sapply(data.choose, is.numeric)]

  if (ncol(dn) == 0) {
    stop("All of your variables are factors. Need numeric variables to make scatterplot matrix.")
  }
  if (ncol(dn) < 2){
    stop ("Not enough numeric variables to make a scatter plot matrix")
  }

  a <- uppertriangle(data, columns = columns, color = color, corMethod = corMethod)
  if (is.null(color)){
    plot <- scatmat(data, columns = columns, alpha = alpha) +
      geom_text(data = a, aes_string(label = "r"), colour = "black")
  } else {
    plot <- scatmat(data, columns = columns, color = color, alpha = alpha) +
      geom_text(data = a, aes_string(label = "r", color = "colorcolumn")) + labs(color = color)
  }
  is.factor.or.character <- function(x) {is.factor(x)|is.character(x)}
  factor <- data.choose[sapply(data.choose, is.factor.or.character)]
  if (ncol(factor) == 0){
    return(plot)
  } else {
    warning("Factor variables are omitted in plot")
    return(plot)
  }
}


upgrade_scatmat_data <- function(data) {
  data <- as.data.frame(data)
  dataIsCharacter <- sapply(data, is.factor)
  if (any(dataIsCharacter)) {
    dataCharacterColumns <- names(dataIsCharacter[dataIsCharacter])
    for (dataCol in dataCharacterColumns) {
      data[[dataCol]] <- as.factor(data[[dataCol]])
    }
  }

  data
}
