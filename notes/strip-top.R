
library(gtable)
library(grid)
library(ggplot)

qplot(total_bill, tip, data = tips) + facet_grid(time ~ day) -> p
p
ggplot_gtable(ggplot_build(p)) -> x
gtable_show_layout(x)

qplot(total_bill, tip, data = tips, main = "test\ntitle") + facet_grid(time ~ day) -> p
p
ggplot_gtable(ggplot_build(p)) -> x2
gtable_show_layout(x2)

grid.draw(gtable_filter(x, "strip-top"))
grid.draw(gtable_filter(x, "strip-top")[1, ])

gtable_show_layout(x)
gtable_show_layout(x2)

str(x2, max.level = 2)
x2$heights[[3]]
