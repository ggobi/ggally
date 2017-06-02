
#' Correlation from the Scatter Plot
#'
#' Estimate correlation from the given data.
#'
#' @param data data set using
#' @param mapping aesthetics being used
#' @param corAlignPercent right align position of numbers. Default is 60 percent across the horizontal
#' @param corMethod \code{method} supplied to cor function
#' @param corUse \code{use} supplied to cor function
#' @param ... other arguments being supplied to geom_text
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @export
#' @keywords hplot
#' @examples
#'  data(tips, package = "reshape")
#'  ggally_cor(tips, mapping = ggplot2::aes_string(x = "total_bill", y = "tip"))
#'  ggally_cor(
#'    tips,
#'    mapping = ggplot2::aes_string(x = "total_bill", y = "tip", size = 15, colour = "I(\"red\")")
#'  )
#'  ggally_cor(
#'    tips,
#'    mapping = ggplot2::aes_string(x = "total_bill", y = "tip", color = "sex"),
#'    size = 5
#'  )
ggally_custom_cor_fn <- function(data, mapping, corAlignPercent = 0.6, corMethod = "pearson", corUse = "complete.obs", ...){

  useOptions = c("all.obs", "complete.obs", "pairwise.complete.obs", "everything", "na.or.complete")
  corUse <-  pmatch(corUse, useOptions)
  if (is.na(corUse)) {
    warning("correlation 'use' not found.  Using default value of 'all.obs'")
    corUse <- useOptions[1]
  } else {
    corUse <- useOptions[corUse]
  }


  cor_stars <- function(pValue) {
    if (pValue < 0.001) {
      "***"
    } else if (pValue < 0.01) {
      "**"
    } else if (pValue < 0.05) {
      "*"
    } else if (pValue < 0.10) {
      "."
    }
  }
  cor_fn <- function(x, y) {

    # also do ddply below if fn is altered
    # pVals <- cor(x,y, method = corMethod, use = corUse)
    corObj <- stats::cor.test(x,y, method = corMethod, use = corUse)
    signifPVals <- sprintf("%.3f",corObj$estimate)
    pStars <- cor_stars(corObj$p.value)

    ret <- str_c(signifPVals, pStars)
    ret
  }



  # xVar <- data[[as.character(mapping$x)]]
  # yVar <- data[[as.character(mapping$y)]]
  # x_bad_rows <- is.na(xVar)
  # y_bad_rows <- is.na(yVar)
  # bad_rows <- x_bad_rows | y_bad_rows
  # if (any(bad_rows)) {
  #   total <- sum(bad_rows)
  #   if (total > 1) {
  #     warning("Removed ", total, " rows containing missing values")
  #   } else if (total == 1) {
  #     warning("Removing 1 row that contained a missing value")
  #   }
  #
  #   xVar <- xVar[!bad_rows]
  #   yVar <- yVar[!bad_rows]
  # }

  # mapping$x <- mapping$y <- NULL

  xCol <- as.character(mapping$x)
  yCol <- as.character(mapping$y)

  # if (is_date(data[,xCol]) || is_date(data[,yCol])) {
  #
  #   # make sure it's a data.frame, as data.tables don't work well
  #   if (! identical(class(data),"data.frame")) {
  #     data <- as.data.frame(data)
  #   }
  #
  #   for (col in c(xCol, yCol)) {
  #     if (is_date(data[,col])) {
  #       data[, col] <- as.numeric(data[, col])
  #     }
  #   }
  # }

  colorCol <- as.character(mapping$colour)
  singleColorCol <- paste(colorCol, collapse = "")

  if (corUse %in% c("complete.obs", "pairwise.complete.obs", "na.or.complete")) {
    if(length(colorCol) > 0) {
      if(singleColorCol %in% colnames(data)) {
        rows <- complete.cases(data[,c(xCol,yCol,colorCol)])
      } else {
        rows <- complete.cases(data[,c(xCol,yCol)])
      }
    } else {
      rows <- complete.cases(data[,c(xCol,yCol)])
    }

    if(any(!rows)) {
      total <- sum(!rows)
      if (total > 1) {
        warning("Removed ", total, " rows containing missing values")
      } else if (total == 1) {
        warning("Removing 1 row that contained a missing value")
      }
    }
    data <- data[rows, ]
  }

  xVal <- data[,xCol]
  yVal <- data[,yCol]

  if(length(names(mapping)) > 0){
    for(i in length(names(mapping)):1){
      # find the last value of the aes, such as cyl of as.factor(cyl)
      tmp_map_val <- as.character(mapping[names(mapping)[i]][[1]])
      if(tmp_map_val[length(tmp_map_val)] %in% colnames(data))
        mapping[names(mapping)[i]] <- NULL

      if(length(names(mapping)) < 1){
        mapping <- NULL
        break;
      }
    }
  }


  # splits <- str_c(as.character(mapping$group), as.character(mapping$colour), sep = ", ", collapse = ", ")
  # splits <- str_c(colorCol, sep = ", ", collapse = ", ")
  final_text <- ""
  if (length(colorCol) < 1) {
    colorCol <- "ggally_NO_EXIST"
  }

  if (
    (singleColorCol != "ggally_NO_EXIST") &&
    (singleColorCol %in% colnames(data))
  ) {

    cord <- ddply(data, c(colorCol), function(x) {
      cor_fn(x[, xCol], x[, yCol])
    }, .parallel = FALSE)
    colnames(cord)[2] <- "ggally_cor"

    # put in correct order
    lev <- levels(data[[colorCol]])
    ord <- rep(-1, nrow(cord))
    for(i in 1:nrow(cord)) {
      for(j in seq_along(lev)){
        if(identical(as.character(cord[i, colorCol]), as.character(lev[j]))) {
          ord[i] <- j
        }
      }
    }

    # print(order(ord[ord >= 0]))
    # print(lev)
    cord <- cord[order(ord[ord >= 0]), ]

    cord$label <- str_c(cord[[colorCol]], ": ", cord$ggally_cor)

    # calculate variable ranges so the gridlines line up
    xmin <- min(xVal, na.rm = TRUE)
    xmax <- max(xVal, na.rm = TRUE)
    xrange <- c(xmin-.01*(xmax-xmin),xmax+.01*(xmax-xmin))
    ymin <- min(yVal, na.rm = TRUE)
    ymax <- max(yVal, na.rm = TRUE)
    yrange <- c(ymin-.01*(ymax-ymin),ymax+.01*(ymax-ymin))


    # print(cord)
    p <- ggally_text(
      label   = str_c("r=", cor_fn(xVal,yVal)),
      mapping = mapping,
      xP      = 0.5,
      yP      = 0.9,
      xrange  = xrange,
      yrange  = yrange,
      color   = "black",
      ...
    ) +
    #element_bw() +
    theme(legend.position = "none")

    xPos <- rep(corAlignPercent, nrow(cord)) * diff(xrange) + min(xrange, na.rm = TRUE)
    yPos <- seq(from = 0.9, to = 0.2, length.out = nrow(cord) + 1) * diff(yrange) + min(yrange, na.rm = TRUE)
    yPos <- yPos[-1]
    # print(range(yVal))
    # print(yPos)

    cordf <- data.frame(xPos = xPos, yPos = yPos, labelp = cord$label)
    cordf$labelp <- factor(cordf$labelp, levels = cordf$labelp)
    # print(cordf)
    # print(str(cordf))

    p <- p + geom_text(
      data = cordf,
      aes(
        x = xPos,
        y = yPos,
        label = labelp,
        color = labelp
      ),
      hjust = 1,
      ...

    )

    p$type <- "continuous"
    p$subType <- "cor"
    p
  } else {
    # calculate variable ranges so the gridlines line up
    xmin <- min(xVal, na.rm = TRUE)
    xmax <- max(xVal, na.rm = TRUE)
    xrange <- c(xmin-.01*(xmax-xmin),xmax+.01*(xmax-xmin))
    ymin <- min(yVal, na.rm = TRUE)
    ymax <- max(yVal, na.rm = TRUE)
    yrange <- c(ymin-.01*(ymax-ymin),ymax+.01*(ymax-ymin))

    p <- ggally_text(
      label = str_c("r=\n", cor_fn(xVal,yVal), collapse=""),
      mapping,
      xP=0.5,
      yP=0.5,
      xrange = xrange,
      yrange = yrange,
      ...
    ) +
    #element_bw() +
    theme(legend.position = "none")

    p$type <- "continuous"
    p$subType <- "cor"
    p
  }
}





ggally_custom_cor_fn(tips, mapping = ggplot2::aes_string(x = "total_bill", y = "tip", color = "sex"))


ggpairs(tips, 1:3, columnLabels = c("Total Bill", "Tip", "Sex"), upper=list(continuous = "custom_cor_fn"))
ggpairs(tips, 1:3, columnLabels = c("Total Bill", "Tip", "Sex"), upper=list(continuous = "custom_cor_fn"), color = "sex")
