mtcars

ggparcoord(mtcars, columns = c(1, 3:6), mapping = aes(linewidth = gear))


dt <- melt(mtcars, c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec"))
qplot(mpg, value, data = dt, color = variable, geom = "line")
