#' Old plotting method
#'
#' @keywords internal
print_ggpairs_old <- function(x, ...){
  plotObj <- x

  # If using internal axis labels, extend the plotting region out since
  # variable names on the margins will not be used
  if(identical(plotObj$axisLabels,"internal")) {
    v1 <- viewport(
#    x = unit(0.5, "npc") + unit(1,"lines"),
    y = unit(0.5, "npc") - unit(0.5,"lines"),
    width=unit(1, "npc") - unit(1,"lines"),
    height=unit(1, "npc") - unit(2, "lines")
  )
  } else {
    v1 <- viewport(
#    x = unit(0.5, "npc") + unit(1,"lines"),
#    y = unit(0.5, "npc") + unit(1,"lines"),
    width=unit(1, "npc") - unit(3,"lines"),
    height=unit(1, "npc") - unit(3, "lines")
  )
  }

  numCol <- length(plotObj$columns)

  v2 <- viewport(
       layout = grid.layout(
               numCol,
               numCol,
               widths = rep(1,numCol),
               heights = rep(1,numCol)
     ))

  grid.newpage()

  if(plotObj$title != ""){
    pushViewport(viewport(height = unit(1,"npc") - unit(.4,"lines")))
    grid.text(plotObj$title,x = .5, y = 1, just = c(.5,1),gp=gpar(fontsize=15))
    popViewport()
  }

  # This plots the variable names on the margins, which is not needed if using internal
# axis labels
if(!identical(plotObj$axisLabels,"internal")) {
  # viewport for Left Names
  pushViewport(viewport(width=unit(1, "npc") - unit(2,"lines"), height=unit(1, "npc") - unit(3, "lines")))

  pushViewport(viewport(layout = grid.layout(numCol, numCol, widths = rep(1,numCol), heights = rep(1,numCol) )))

  # Left Side
  for(i in 1:numCol){
    grid.text(names(plotObj$data[,plotObj$columns])[i],0,0.5,rot=90,just=c("centre","centre"), vp = vplayout(as.numeric(i),1))
  }

  popViewport()# layout
  popViewport()# spacing

  # viewport for Bottom Names
  pushViewport(viewport(width=unit(1, "npc") - unit(3,"lines"), height=unit(1, "npc") - unit(2, "lines")))

  pushViewport(viewport(layout = grid.layout(numCol, numCol, widths = rep(1,numCol), heights = rep(1,numCol) )))


  # Bottom Side
  for(i in 1:numCol){
    grid.text(names(plotObj$data[,plotObj$columns])[i],0.5,0,just=c("centre","centre"), vp = vplayout(numCol, i))
  }

  popViewport() #layout
  popViewport() #spacing
}

##############################################################
####################  End Viewports  #########################
##############################################################

#####################  Plot Objects  #########################

  pushViewport(v1) # labels on outside
  pushViewport(v2) # layout of plots

  for(rowPos in 1:numCol){
    for(columnPos in 1:numCol){
      p <- getPlot(plotObj, rowPos, columnPos)
      if(!is_blank_plot(p)){

        pos <- columnPos + (rowPos - 1) * numCol
        type <- p$type
        subType <- p$subType
        if(plotObj$printInfo) {
          cat("Pos #", pos)
          if(!is.null(type)) cat(": type = ", type)
          if(!is.null(subType)) cat(": subType = ", subType)
          cat("\n")
        }

        # hack because ggplot2 is annoying
        # if(!is.null(subType)){
        #   if(subType == "facethist"){
        #     p <- p + scale_x_continuous(NULL) + scale_y_continuous(NULL)
        #   } else if (subType %in% c("box", "dot")) {
        #     p <- p + scale_x_continuous(NULL, labels="", breaks=1)
        #   } else if (subType == "ratio"){
        #     p <- p +
        #       scale_x_continuous(
        #         NULL,
        #         limits=c(1,length(p$x_names) + 1),
        #         breaks=1:(length(p$x_names) + 1),
        #         labels=c(p$x_names,""),
        #         minor_breaks=FALSE
        #       ) +
        #       scale_y_continuous(
        #         NULL,
        #         limits=c(1,length(p$y_names) + 1),
        #         breaks=1:(length(p$y_names) + 1),
        #         labels=c(p$y_names,""),
        #         minor_breaks=FALSE
        #       )

        #   }
        # }

        noTicks <- c("internal", "none")
        removeTicks <- plotObj$axisLabels %in% noTicks
        if( ! is.null(p$axisLabels)) {
          removeTicks <- p$axisLabels %in% noTicks
        }

        if( columnPos != 1 || removeTicks){
          p <- p + theme(axis.text.y = element_blank(), axis.title.y = element_blank() )
        }

        if( (rowPos != numCol) || removeTicks){
          p <- p + theme(axis.text.x = element_blank(), axis.title.x = element_blank() )
        }

        if(removeTicks) {
          p <- p + theme(
            # strip.background = element_blank(),
            # strip.text.x     = element_blank(),
            # strip.text.y     = element_blank(),
            # axis.ticks       = element_blank()
            strip.background = element_rect(fill="white", colour = NA),
            strip.text.x     = element_blank(),
            strip.text.y     = element_blank(),
            axis.ticks       = element_blank()
          )

        }

        # Adjusts for the blank space left by faceting, and manually
        # sets the limits for numeric axes to 1% of the variable's
        # range below the min and above the max.
        if (identical(p$type,"combo")) {
          # Scale the numeric variable; the numeric variable is
          # mapped to the y variable for dot and box plots, but to the
          # x variable for the others
          p <- p + labs(x = NULL, y = NULL)

          # if (p$subType %in% c("dot","box")) {
          #   if (is.numeric(p$data[,as.character(p$mapping$y)])) {
          #     ymin <- min(p$data[,as.character(p$mapping$y)])
          #     ymax <- max(p$data[,as.character(p$mapping$y)])
          #   p <- p + labs(x = NULL, y = NULL) +
          #     scale_y_continuous(limits=c(ymin-.01*(ymax-ymin),ymax+.01*(ymax-ymin)))
          #   }
          #   if (is.numeric(p$data[,as.character(p$mapping$x)])) {
          #     xmin <- min(p$data[,as.character(p$mapping$x)])
          #     xmax <- max(p$data[,as.character(p$mapping$x)])
          #     p <- p + labs(x = NULL, y = NULL) +
          #       scale_x_continuous(limits=c(xmin-.01*(xmax-xmin),xmax+.01*(xmax-xmin)))
          #   }

          # }

          # Adjust for blank space left by faceting
          if(plotObj$printInfo) {
            print(p$subType)
            print(p$horizontal)
          }

          if (p$horizontal) {
#            p <- p + theme(plot.margin = unit(c(0,-0.5,0,0), "lines"))

            # HACK!
            if (p$subType %in% c("facethist")) {
              p <- p + theme(plot.margin = unit(c(0, -0.5, 0, 0), "lines"))
            } else {
              p <- p + theme(plot.margin = unit(c(0, -0.5, 0, -0.5), "lines"))
            }

            if (columnPos != numCol) {
              p <- p + theme(
                strip.background = element_blank(),
                strip.text.x     = element_blank(),
                strip.text.y     = element_blank()
              )
            }

          } else {
            # vertical

            # default
            # p <- p + theme(plot.margin = unit(c(-0.5,0,-0.5,-0.5), "lines"))

            if (p$subType %in% c("facethist")) {
              p <- p + theme(plot.margin = unit(c(-0.5, 0, 0, 0), "lines"))
            } else {
              p <- p + theme(plot.margin = unit(c(-0.5, 0, -0.5, 0), "lines"))
            }

            if (rowPos != 1) {
              p <- p + theme(
                strip.background = element_blank(),
                strip.text.x     = element_blank(),
                strip.text.y     = element_blank()
              )
            }
          }
        } # end if p$type==combo
        # Adjust for blank space left by faceting in faceted bar plot
        else if (identical(p$subType,"facetbar")) {
          p <- p + labs(x = NULL, y = NULL) + theme(plot.margin = unit(c(0,-0.5,0,0), "lines"))

          if (rowPos != 1) {
            p <- p + theme(
              strip.background = element_blank(),
              strip.text.x     = element_blank(),
              strip.text.y     = element_blank()
            )
          }

        }
        # Need to scale both variables for continuous plots
        else if (identical(p$type,"continuous") && !identical(p$subType,"cor")) {
          p <- p + labs(x = NULL, y = NULL) + theme(plot.margin = unit(rep(0,4), "lines"))
        }
        # Scale the variable for numeric diagonal plots
        else if (identical(p$type,"diag") && is.numeric(p$data[,as.character(p$mapping$x)])) {
          p <- p + labs(x = NULL, y = NULL) + theme(plot.margin = unit(rep(0,4), "lines"))
        }

        # if not internal labels
        else {
          p <- p + labs(x = NULL, y = NULL) + theme(plot.margin = unit(rep(0,4), "lines"))
        }

        showLegend = FALSE
        if (!is.null(plotObj$legends)) showLegend <- identical(plotObj$legends, TRUE)
        if (showLegend == FALSE) {
          if (!is.null(p$ggally$legend) && ! is.na(p$ggally$legend)) {
            showLegend <- identical(p$ggally$legend, TRUE)
          }
        }
        if (showLegend == FALSE) {
          p <- p + theme(legend.position = "none")
        }

        grid.rect(
          gp = gpar(fill = "white", lty = "blank"),
          vp = vplayout(rowPos, columnPos)
        )

        if(identical(plotObj$verbose, TRUE)) {
          print(p, vp = vplayout(rowPos, columnPos))
        } else {
          suppressMessages(suppressWarnings(print(p, vp = vplayout(rowPos, columnPos))))
        }

      }# end plot alterations
    }# end cols
  }# end rows

  popViewport() #layout
  popViewport() #spacing
}
