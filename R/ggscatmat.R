if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("xvalue", "yvalue"))
}


#' lowertriangle - rearrange dataset as the preparation of ggscatmat function
#'
#' function for making the melted dataset used to plot the lowertriangle scatterplots.
#'
#' @export
#' @param data a data matrix. Should contain numerical (continuous) data.
#' @param columns an option to choose the column to be used in the raw dataset. Defaults to \code{1:ncol(data)}
#' @param color an option to choose a factor variable to be grouped with. Defaults to \code{(NULL)}
#' @author Mengjia Ni, Di Cook \email{dicook@@monash.edu}
#' @examples
#' data(flea)
#' head(lowertriangle(flea, columns= 2:4))
#' head(lowertriangle(flea))
#' head(lowertriangle(flea, color="species"))
lowertriangle <- function(data, columns=1:ncol(data), color=NULL) {
  data <- upgrade_scatmat_data(data)
  data.choose <- data[, columns]
  dn <- data.choose[sapply(data.choose, is.numeric)]
  factor <- data[sapply(data, is.factor)]
  p <- ncol(dn)
  newdata <- NULL
  for (i in 1:p) {
    for (j in 1:p) {
      newdata <- rbind(newdata,
                       cbind(dn[, i], dn[, j], i, j, colnames(dn)[i], colnames(dn)[j], factor)
      )
    }
  }
  colnames(newdata) <- c("xvalue", "yvalue", "xslot", "yslot", "xlab", "ylab", colnames(factor))

  rp <- data.frame(newdata)
  rp[, 2][rp[, 3] >= rp[, 4]] <- "NA"
  rp[, 1][rp[, 3] > rp[, 4]] <- "NA"

  rp$xvalue <- suppressWarnings(as.numeric(as.character(rp$xvalue)))
  rp$yvalue <- suppressWarnings(as.numeric(as.character(rp$yvalue)))

  if (is.null(color)){
    rp.new <- rp[, 1:6]
  } else {
    colorcolumn <- rp[, which(colnames(rp) == color)]
    rp.new <- cbind(rp[, 1:6], colorcolumn)
  }
  return(rp.new)
}

#' uppertriangle - rearrange dataset as the preparation of ggscatmat function
#'
#' function for making the dataset used to plot the uppertriangle plots.
#'
#' @export
#' @param data a data matrix. Should contain numerical (continuous) data.
#' @param columns an option to choose the column to be used in the raw dataset. Defaults to \code{1:ncol(data)}
#' @param color an option to choose a factor variable to be grouped with. Defaults to \code{(NULL)}
#' @author Mengjia Ni, Di Cook \email{dicook@@monash.edu}
#' @importFrom stats cor
#' @examples
#' data(flea)
#' head(uppertriangle(flea, columns=2:4))
#' head(uppertriangle(flea))
#' head(uppertriangle(flea, color="species"))
uppertriangle <- function(data, columns=1:ncol(data), color=NULL) {
  data <- upgrade_scatmat_data(data)
  data.choose <- data[, columns]
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

  rp <- data.frame(newdata)
  rp[, 2][rp[, 3] <= rp[, 4]] <- "NA"
  rp[, 1][rp[, 3] < rp[, 4]] <- "NA"

  rp$xvalue <- suppressWarnings(as.numeric(as.character(rp$xvalue)))
  rp$yvalue <- suppressWarnings(as.numeric(as.character(rp$yvalue)))

  if (is.null(color)){
    rp.new <- rp[, 1:8]
  }else{
    colorcolumn <- rp[, which(colnames(rp) == color)]
    rp.new <- cbind(rp[, 1:8],  colorcolumn)
  }
  a <- rp.new
  b <- subset(a, (a$yvalue != "NA") & (a$xvalue != "NA"))
  if (is.null(color)){
    data.cor <- ddply(b, .(ylab, xlab), summarise,
                      r = paste(round(
                        cor(xvalue, yvalue, use = "pairwise.complete.obs"),
                        digits = 2
                      )),
                      xvalue = min(xvalue) + 0.5 * (max(xvalue) - min(xvalue)),
                      yvalue = min(yvalue) + 0.5 * (max(yvalue) - min(yvalue)))
    return(data.cor)
  }else{
    c <- b
    data.cor1 <- ddply(c, .(ylab, xlab, colorcolumn), summarise,
                      r = paste(round(
                        cor(xvalue, yvalue, use = "pairwise.complete.obs"),
                        digits = 2
                      ))
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

#' scatmat - plot the lowertriangle plots and density plots of the scatter plot matrix.
#'
#' function for making scatterplots in the lower triangle and diagonal density plots.
#'
#' @export
#' @param data a data matrix. Should contain numerical (continuous) data.
#' @param columns an option to choose the column to be used in the raw dataset. Defaults to \code{1:ncol(data)}
#' @param color an option to group the dataset by the factor variable and color them by different colors. Defaults to \code{NULL}
#' @param alpha an option to set the transparency in scatterplots for large data. Defaults to \code{1}.
#' @author Mengjia Ni, Di Cook \email{dicook@@monash.edu}
#' @examples
#' data(flea)
#' scatmat(flea, columns=2:4)
#' scatmat(flea, columns= 2:4, color="species")
scatmat <- function(data, columns=1:ncol(data), color=NULL, alpha=1) {
  data <- upgrade_scatmat_data(data)
  data.choose <- data[, columns]
  dn <- data.choose[sapply(data.choose, is.numeric)]
  if (ncol(dn) == 0) {
    stop("All of your variables are factors. Need numeric variables to make scatterplot matrix.")
  } else {
    ltdata.new <- lowertriangle(data, columns = columns, color = color)
    r <- ggplot(ltdata.new, mapping = aes_string(x = "xvalue", y = "yvalue")) +
      theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
      facet_grid(ylab ~ xlab, scales = "free") +
      theme(aspect.ratio = 1)
    if (is.null(color)) {
      densities <- do.call("rbind", lapply(1:ncol(dn), function(i) {
        data.frame(xlab = names(dn)[i], ylab = names(dn)[i],
                   x = dn[, i])
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
      r <- r + geom_point(alpha = alpha, na.rm = TRUE)
      return(r)
    } else {
      densities <- do.call("rbind", lapply(1:ncol(dn), function(i) {
        data.frame(xlab = names(dn)[i], ylab = names(dn)[i],
                   x = dn[, i], colorcolumn = data[, which(colnames(data) == color)])
      }))
      for (m in 1:ncol(dn)) {
        j <- subset(densities, xlab == names(dn)[m])
        r <- r +
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

#' ggscatmat - a traditional scatterplot matrix for purely quantitative variables
#'
#' This function makes a scatterplot matrix for quantitative variables with density plots on the diagonal
#' and correlation printed in the upper triangle.
#'
#' @export
#' @param data a data matrix. Should contain numerical (continuous) data.
#' @param columns an option to choose the column to be used in the raw dataset. Defaults to \code{1:ncol(data)}.
#' @param color an option to group the dataset by the factor variable and color them by different colors. Defaults to \code{NULL}.
#' @param alpha an option to set the transparency in scatterplots for large data. Defaults to \code{1}.
#' @author Mengjia Ni, Di Cook \email{dicook@@monash.edu}
#' @examples
#' data(flea)
#' ggscatmat(flea, columns = 2:4)
#' ggscatmat(flea, columns = 2:4, color = "species")
ggscatmat <- function(data, columns=1:ncol(data), color = NULL, alpha = 1){

  data <- upgrade_scatmat_data(data)
  data.choose <- data[, columns]
  dn <- data.choose[sapply(data.choose, is.numeric)]

  if (ncol(dn) == 0) {
    stop("All of your variables are factors. Need numeric variables to make scatterplot matrix.")
  }
  if (ncol(dn) < 2){
    stop ("Not enough numeric variables to make a scatter plot matrix")
  }

  a <- uppertriangle(data, columns = columns, color = color)
  if (is.null(color)){
    plot <- scatmat(data, columns = columns, alpha = alpha) +
      geom_text(data = a, aes_string(label = "r"), colour = "black")
  } else {
    plot <- scatmat(data, columns = columns, color = color, alpha = alpha) +
      geom_text(data = a, aes_string(label = "r", color = "colorcolumn")) + labs(color = color)
  }
  factor <- data.choose[sapply(data.choose, is.factor)]
  if (ncol(factor) == 0){
    return(plot)
  } else {
    warning("Factor variables are omitted in plot")
    return(plot)
  }
}


upgrade_scatmat_data <- function(data) {
  dataIsCharacter <- sapply(data, is.character)
  if (any(dataIsCharacter)) {
    dataCharacterColumns <- names(dataIsCharacter[dataIsCharacter])
    for (dataCol in dataCharacterColumns) {
      data[dataCol] <- as.factor(data[, dataCol])
    }
  }

  data
}
