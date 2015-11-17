context("allNA")

test_that("allNA", {
              dd <- data.frame(x=c(1:5,rep(NA,5)),y=c(rep(NA,5),2:6),z=1:10)
              ggpairs(dd)
          })
