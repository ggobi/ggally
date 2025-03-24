###Original text for the issue ####

####Load packages####
library(effects)
library(ggplot2)
library(GGally)
####Load data ####
data(Arrests)
####Plot####
plot <- ggpairs(Arrests)
plot

####Première propal ChatGPT####

# Select only the 'released' column against all other variables
plot <- ggpairs(Arrests, columns = c(1, 2:5),
                mapping = ggplot2::aes(color = released)) +
  theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

plot


# Select only the 'released' column against all other variables
plot <- ggpairs(Arrests, columns = c(1, 2:5),
                mapping = ggplot2::aes(color = released)) +
  theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        legend.title = ggplot2::element_text(size = 12),
        legend.text = ggplot2::element_text(size = 10))

plot

#JE teste des trucs

# Select only the 'released' column against all other variables
plot <- ggpairs(Arrests, columns = c(1, 2:5),
                mapping = ggplot2::aes(color = released),
                upper = list(continuous = wrap("points")),
                lower = list(continuous = wrap("points"))) +
  theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        legend.title = ggplot2::element_text(size = 12),
        legend.text = ggplot2::element_text(size = 10))

# Ensure x-axis labels appear on every subplot
plot <- plot + theme(strip.background = ggplot2::element_blank(),
                     strip.text.x = ggplot2::element_text(size = 10),
                     axis.text.x = ggplot2::element_text(size = 9))

plot


# Function to add x-axis labels and gridlines to each subplot
add_axes_and_grids <- function(plot) {
  plot + theme(
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
    axis.ticks.x = ggplot2::element_line(),
    panel.grid.major = ggplot2::element_line(color = "grey80"),
    panel.grid.minor = ggplot2::element_line(color = "grey90"),
    strip.background = ggplot2::element_blank(),
    strip.text.x = ggplot2::element_text(size = 10),
    axis.title.x = ggplot2::element_text(size = 12)
  )
}

# Select only the 'released' column against all other variables
base_plot <- ggpairs(Arrests, columns = c(1, 2:5),
                     mapping = ggplot2::aes(color = released),
                     upper = list(continuous = wrap("points")),
                     lower = list(continuous = wrap("points")),
                     diag = list(continuous = wrap("barDiag")))

# Ensure x-axis labels and gridlines are shown on each subplot, even in the same column
plot <- base_plot +
  theme(
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
    panel.grid.major = ggplot2::element_line(color = "grey80"),
    panel.grid.minor = ggplot2::element_line(color = "grey90"),
    strip.background = ggplot2::element_blank(),
    strip.text.x = ggplot2::element_text(size = 10),
    axis.title.x = ggplot2::element_text(size = 12),
    axis.ticks.x = ggplot2::element_line(),
    axis.text.x.bottom = ggplot2::element_text(size = 10)
  )


plot

