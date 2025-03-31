
library(ggplot2)

library(GGally)


library(gridExtra)

library(effects)  # Contains the 'Arrests' dataset


library(cowplot)

library(effects)  # For 'Arrests' dataset

data("Arrests")

ggpairs_focus2(Arrests, target_column = "released")
