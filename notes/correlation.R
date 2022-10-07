ggpairs(tips, columns = 1:2, params = c(corMethod = "pearson"))
ggpairs(tips, columns = 1:2, params = c(corMethod = "kendall"))
ggpairs(tips, columns = 1:2, params = c(corMethod = "pearson"), color = "sex")
ggpairs(tips, columns = 1:2, params = c(corMethod = "kendall"), color = "sex")

ggpairs(tips, columns = 1:2, upper = list(params = list(corMethod = "kendall")))
ggpairs(tips, columns = 1:2, upper = list(params = list(corMethod = "pearson")))
ggpairs(tips, columns = 1:2, upper = list(params = list(corMethod = "pearson")), color = "sex")
ggpairs(tips, columns = 1:2, upper = list(params = list(corMethod = "kendall")), color = "sex")



swM <- swiss
colnames(swM) <- abbreviate(colnames(swiss), min = 6)
swM[1, 2] <- swM[7, 3] <- swM[25, 5] <- NA # create 3 "missing"
## Consider all 5 "use" cases :
(C. <- cov(swM)) # use="everything"  quite a few NA's in cov.matrix
try(cov(swM, use = "all")) # Error: missing obs...
(C2 <- cov(swM, use = "complete"))
stopifnot(identical(C2, cov(swM, use = "na.or.complete")))
range(eigen(C2, only.values = TRUE)$values) # 6.46   1930
(C3 <- cov(swM, use = "pairwise"))
range(eigen(C3, only.values = TRUE)$values) # 6.19   1938



# using full data
cor(mtcars[, 1:3], method = "kendall", use = "complete")[2, 3]
0.7915213
cor(mtcars[, 1:3], method = "kendall", use = "pairwise")[2, 3] # matches below
cor(mtcars[, 1:3], method = "kendall", use = "na.or")[2, 3]

# only two columns
cor(mtcars$cyl, mtcars$disp, method = "kendall", use = "complete")
cor(mtcars$cyl, mtcars$disp, method = "kendall", use = "pairwise")
cor(mtcars$cyl, mtcars$disp, method = "kendall", use = "na.or")


ggally_cor(mtcars, aes(cyl, disp), corMethod = "kendall", use = "complete.obs")
