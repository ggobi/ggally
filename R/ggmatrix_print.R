
#' Is Blank Plot?
#' Find out if the plot equals a blank plot
#'
#' @keywords internal
#' @examples
#'  GGally:::is_blank_plot(ggally_blank())
#'  GGally:::is_blank_plot(ggally_points(mtcars, ggplot2::aes_string(x = "disp", y = "hp")))
#'
is_blank_plot <- function(p){
  if (is.null(p) || identical(p, "blank")) {
    TRUE
  } else if ( !is.null(p$subType) && !is.null(p$type)) {
    (p$subType == "blank") && (p$type == "blank")
  } else {
    FALSE
  }
}


#' Viewport Layout Wrapper
#'
#' A wrapper function to set the viewport.
#'
#' @param x row position
#' @param y coloumn position
#' @importFrom grid viewport
#' @keywords internal
#' @author Hadley Wickham \email{h.wickham@@gmail.com}
# '
vplayout <- function(x, y) {
  viewport(layout.pos.row = x, layout.pos.col = y)
}


#' Get theme element
#'
#' Get the info from theme or a default value
#' @param p ggplot2 object
#' @param element first key
#' @param elementKey key within element object
#' @keywords internal
get_theme_element <- function(p, element, elementKey) {
  themeObj <- p$gg

  if (!is.null(themeObj)) {
    elementObj <- themeObj[[element]]

    if (!is.null(elementObj)) {
      elementValue <- elementObj[[elementKey]]
      if (!is.null(elementValue)) {
        return(elementValue)
      }
    }
  }
  return(NULL)
}

#' Get first non null value
#'
#' @param ... args to be checked
#' @keywords internal
#' @examples
#' p <- ggplot2::qplot(1:10, 1:10) + ggplot2::theme(plot.title = ggplot2::element_text(size = 13))
#' GGally:::first_non_null(GGally:::get_theme_element(p, "plot.title", "size"), 15)
#' GGally:::first_non_null(GGally:::get_theme_element(p, "plot.title", "BAD"), 15)
first_non_null <- function(...) {
  vals <- c(...)
  vals[which.min(is.null(vals))]
}



#' Print ggpair object
#'
#' Specialized method to print the ggpair object-
#'
#' @param x ggpair object to be plotted
#' @param leftWidthProportion proportion of a plot area devoted to left axis labels
#' @param bottomHeightProportion proportion of a plot area devoted to bottom axis labels
#' @param spacingProportion proportion of a plot area devoted to the space between plots
#' @param gridNewPage boolean that determines if a \code{\link[grid]{grid.newpage}()} should be executed before printing. Defaults to \code{TRUE}
#' @param ... ignored
#' @method print ggmatrix
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @importFrom grid gpar grid.layout grid.newpage grid.text grid.rect popViewport pushViewport viewport grid.draw
#' @export
#' @examples
#'  data(tips, package = "reshape")
#'  pMat <- ggpairs(tips, c(1,3,2), color = "sex")
#'  pMat # calls print(pMat), which calls print.ggmatrix(pMat)
#'
#'  ## defaults; (prints strips on top and right edges of matrix)
#'  # print(pMat, left = 0.2, spacing = 0.03, bottom = 0.1)
#'
#'  ## give the left axis labels area a proportion of 3 plot size
#'  # print(pMat, leftWidthProportion = 3)
#'
#'  ## give the bottom axis labels area a proportion of 1 plot size
#'  # print(pMat, bottomHeightProportion = 1)
#'
#'  ## give the spacing between plots a proportion of 1 plot size
#'  # print(pMat, spacing = 1)
print.ggmatrix <- function(
  x,
  leftWidthProportion = 0.2,
  bottomHeightProportion = 0.1,
  spacingProportion = 0.03,
  gridNewPage = TRUE,
  ...
) {

  args <- list(...)
  if ("printInfo" %in% names(args)) {
    printInfo <- args[["printInfo"]]
  } else {
    printInfo <- FALSE
  }


  displayXAxisLabels <- !is.null(x$xAxisLabels)
  displayYAxisLabels <- !is.null(x$yAxisLabels)

  # If using internal axis labels, extend the plotting region out since
  # variable names on the margins will not be used

  # determine if space should be given for Y text labels
  v1XWidth <- unit(1, "npc") - unit(ifelse(displayYAxisLabels, 3, 1), "lines")
  v1YHeight <- unit(1, "npc") - unit(ifelse(displayXAxisLabels, 3, 2), "lines")
  v1YVal <- unit(0.5, "npc")
  if (!displayXAxisLabels) {
    v1YVal <- v1YVal - unit(0.5, "lines")
  }
  v1 <- viewport(
    y = v1YVal,
    width  = v1XWidth,
    height = v1YHeight
  )

  viewPortWidths <- c(1, rep(c(spacingProportion, 1), x$ncol - 1))
  viewPortHeights <- c(rep(c(1, spacingProportion), x$nrow - 1), 1)


  x$showXAxisPlotLabels <- identical(x$showXAxisPlotLabels, TRUE)
  x$showYAxisPlotLabels <- identical(x$showYAxisPlotLabels, TRUE)
  if (x$showXAxisPlotLabels) {
    # x plot axis are displayed
    viewPortHeights <- c(viewPortHeights, bottomHeightProportion)
  }
  if (x$showYAxisPlotLabels) {
    # y plot axis are displayed
    viewPortWidths <- c(leftWidthProportion, viewPortWidths)
  }

  v2 <- viewport(
    layout = grid.layout(
      length(viewPortHeights),
      length(viewPortWidths),
      ## added left and bottom spacers for axis labels
      widths = viewPortWidths,
      heights = viewPortHeights
  ))


##############################################################
####################  End Viewports  #########################
##############################################################

####################  Start Labels  #########################

  if (identical(gridNewPage, TRUE)) {
    grid.newpage()
  }

  if (! is.null(x$title)) {
    if (x$title != "") {
      pushViewport(viewport(height = unit(1, "npc") - unit(.4, "lines")))
      grid.text(
        x$title,
        x = .5, y = 1,
        just = c(.5, 1),
        gp = gpar(fontsize = first_non_null(
          get_theme_element(x, "title", "size"),
          get_theme_element(x, "plot.title", "size"),
          15
        ))
      )
      popViewport()
    }
  }

  # plot the y text axis labels
  if (displayYAxisLabels) {

    # viewport for Left Names
    leftViewPort <- v1
    leftViewPort$width <- leftViewPort$width + unit(1, "lines")
    pushViewport(leftViewPort)
    #   viewport(
    #     width = unit(1, "npc") - unit(2,"lines"),
    #     height = unit(1, "npc") - unit(3, "lines")
    #   )
    # )

    ## new for axis spacingProportion
    pushViewport(
      viewport(
        layout = grid.layout(
          length(viewPortHeights), length(viewPortWidths),
          widths = viewPortWidths, heights = viewPortHeights
        )
      )
    )

    # Left Side
    for (i in 1:(x$nrow)) {
      grid.text(
        x$yAxisLabels[i],
        0, 0.5, rot = 90,
        just = c("centre", "centre"),
        vp = vplayout(as.numeric(i) * 2 - 1, 1),
        gp = gpar(fontsize = first_non_null(
          get_theme_element(x, "axis.title.y", "size"),
          get_theme_element(x, "axis.title", "size"),
          12
        ))
      )
    }

    popViewport()# layout
    popViewport()# spacing
  }

  # plot the x text labels
  if (displayXAxisLabels) {

    # viewport for Bottom Names
    bottomViewPort <- v1
    bottomViewPort$height <- bottomViewPort$height + unit(1, "lines")
    pushViewport(bottomViewPort)
    #   viewport(
    #     width = unit(1, "npc") - unit(3,"lines"),
    #     height = unit(1, "npc") - unit(2, "lines")
    #   )
    # )

    ## new for axis spacing
    pushViewport(
      viewport(
        layout = grid.layout(
          length(viewPortHeights), length(viewPortWidths),
          widths = viewPortWidths, heights = viewPortHeights
        )
      )
    )

    # Bottom Side
    for (i in 1:(x$ncol)) {
      grid.text(
        x$xAxisLabels[i],
        0.5,
        0,
        just = c("centre", "centre"),
        vp = vplayout(
          ifelse(x$showXAxisPlotLabels, 2 * (x$nrow), 2 * (x$nrow) - 1),
          ifelse(x$showYAxisPlotLabels, 2 * i, 2 * i - 1)
        ),
        gp = gpar(fontsize = first_non_null(
          get_theme_element(x, "axis.title.x", "size"),
          get_theme_element(x, "axis.title", "size"),
          12
        ))
      )
    }

    popViewport() #layout
    popViewport() #spacing
  }

##############################################################
######################  End Labels  ##########################
##############################################################

#####################  Plot Objects  #########################

  pushViewport(v1) # labels on outside
  pushViewport(v2) # layout of plots

  for (rowPos in 1:(x$nrow)) {
    for (columnPos in 1:(x$ncol)) {
      p <- x[rowPos, columnPos]

      if (is_blank_plot(p)) {
        next
      }

      pGtable <- ggplot_gtable(ggplot_build(p))

      ## New axis labels

      # left axis
      if (columnPos == 1 && x$showYAxisPlotLabels) {
        if (identical(printInfo, TRUE)) {
          print("trying left axis")
        }
        pAxisLabels <- gtable_filter(pGtable, "axis-l")

        hasTopStrips <- FALSE
        if (rowPos == 1 || identical(x$showStrips, TRUE)) {
          # worry about top strips
          if (is.null(x$showStrips) || identical(x$showStrips, TRUE)) {
            if ("strip-top" %in% pGtable$layout$name) {
              hasTopStrips <- TRUE
              stripHeight <- gtable_filter(pGtable, "strip-top")$heights[1]
              stripViewport <- viewport(layout = grid.layout(
                nrow = 2,
                ncol = 1,
                widths = unit(1, "null"),
                heights = unit(
                  c(stripHeight, 1),
                  units = c(attr(stripHeight, "unit"), "null")
                )
              ))
            }
          }
        }

        # make a viewport that is chopped into numFacets parts vertically
        grobLength <- length(pAxisLabels$grobs)
        leftAxisLayoutHeight <- rep(c(5.5, 1), grobLength)[-1]
        leftAxisLayoutHeightUnits <- rep(c("pt", "null"), grobLength)[-1]
        vpLAxis <- viewport(
          layout = grid.layout(
            nrow = 2 * grobLength - 1,
            ncol = 1,
            widths  = unit(1, "null"),
            heights = unit(leftAxisLayoutHeight, leftAxisLayoutHeightUnits)
          )
        )

        pushViewport(vplayout(rowPos * 2 - 1, 1))
        if (hasTopStrips) {
          pushViewport(stripViewport)
          pushViewport(vplayout(2, 1))
        }
        pushViewport(vpLAxis)
          for (lAxisPos in 1:grobLength) {
            pushViewport(vplayout(lAxisPos * 2 - 1, 1))
            grid.draw(pAxisLabels$grobs[[lAxisPos]])
            popViewport()
          }
        popViewport() # vpLAxis
        if (hasTopStrips) {
          popViewport()
          popViewport()
        }
        popViewport() # left Axis 'plot' area
      }

      ## bottom axis
      if (rowPos == (x$nrow) && x$showXAxisPlotLabels) {
        if (identical(printInfo, TRUE)) {
          print("trying bottom axis")
        }
        pAxisLabels <- gtable_filter(pGtable, "axis-b")
        grobLength <- length(pAxisLabels$grobs)

        hasRightStrips <- FALSE
        if (columnPos == (x$ncol) || identical(x$showStrips, TRUE)) {
          # worry about top strips
          if (is.null(x$showStrips) || identical(x$showStrips, TRUE)) {
            if ("strip-right" %in% pGtable$layout$name) {
              hasRightStrips <- TRUE
              stripWidth <- gtable_filter(pGtable, "strip-right")$widths[1]
              stripViewport <- viewport(layout = grid.layout(
                nrow = 1,
                ncol = 2,
                heights = unit(1, "null"),
                widths = unit(
                  c(1, stripWidth),
                  units = c("null", attr(stripWidth, "unit"))
                )
              ))
            }
          }
        }

        botAxisLayoutWidth <- rep(c(5.5, 1), grobLength)[-1]
        botAxisLayoutWidthUnits <- rep(c("pt", "null"), grobLength)[-1]
        vpBAxis <- viewport(
          layout = grid.layout(
            nrow = 1,
            ncol = 2 * grobLength - 1,
            heights = unit(1, "null"),
            widths  = unit(botAxisLayoutWidth, botAxisLayoutWidthUnits)
          )
        )

        pushViewport(
          vplayout(
            2 * (x$nrow),
            ifelse(
              x$showYAxisPlotLabels,
              2 * columnPos,
              2 * columnPos - 1
            )
          )
        )
        if (hasRightStrips) {
          pushViewport(stripViewport)
          pushViewport(vplayout(1, 1))
        }
        pushViewport(vpBAxis)
          for (bAxisPos in 1:grobLength) {
            pushViewport(vplayout(1, bAxisPos * 2 - 1))
              grid.draw(pAxisLabels$grobs[[bAxisPos]])
            popViewport()
          }
        popViewport() # vpBAxis
        if (hasRightStrips) {
          popViewport()
          popViewport()
        }
        popViewport() # bottom Axis 'plot' area

      }

      ## get 'plot panel' grob to draw

      # ask about strips
      layoutNames <- c("panel")
      stripRightName <- "strip-right"
      stripTopName <- "strip-top"
      legendName <- "guide-box"
      allLayoutNames <- c(layoutNames, stripRightName, stripTopName, legendName)

      if (is.null(x$showStrips)) {
        # make sure it's on the outer right and top edge
        if (columnPos == (x$ncol)) {
          layoutNames <- c(layoutNames, stripRightName)
        }
        if (rowPos == 1) {
          layoutNames <- c(layoutNames, stripTopName)
        }

      } else if (x$showStrips) {
        layoutNames <- c(layoutNames, stripRightName, stripTopName)
      }

      # if they have a custom plot, make sure it shows up
      if (! is.null(p$axisLabels)) {
        # pShowStrips <- ! identical(p$axisLabels, FALSE)

        # copied from old code.  want to replace it to something like above
        if (p$axisLabels %in% c("internal", "none")) {
          layoutNames <- allLayoutNames
        }
      }

      if (identical(x$legends, TRUE)) {
        layoutNames <- c(layoutNames, legendName)
      }

      # get correct panel (and strips)
      layoutRows <- pGtable$layout$name %in% layoutNames

      layoutInfo <- pGtable$layout[layoutRows, ]
      layoutTB <- layoutInfo[, c("t", "b")]
      layoutLR <- layoutInfo[, c("l", "r")]

      pPanel <- pGtable[
        min(layoutTB):max(layoutTB),
        min(layoutLR):max(layoutLR)
      ]

      ## Draw 'plot panel'
      pushViewport(
        vplayout(
          2 * rowPos - 1,
          ifelse(
            x$showYAxisPlotLabels,
            2 * columnPos,
            (2 * columnPos) - 1
          )
        )
      )
        suppressMessages(suppressWarnings(
          grid.draw(pPanel)
        ))
      popViewport() # 'plot panel' area

    } # end cols
  } # end rows

  popViewport() #layout
  popViewport() #spacing
}
