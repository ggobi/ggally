psychademic <- read.csv("http://stats.idre.ucla.edu/stat/data/mmreg.csv")

str(psychademic)
# 'data.frame':	600 obs. of  8 variables:
#  $ locus_of_control: num  -0.84 -0.38 0.89 0.71 -0.64 1.11 0.06 -0.91 0.45 0 ...
#  $ self_concept    : num  -0.24 -0.47 0.59 0.28 0.03 0.9 0.03 -0.59 0.03 0.03 ...
#  $ motivation      : num  1 0.67 0.67 0.67 1 0.33 0.67 0.67 1 0.67 ...
#  $ read            : num  54.8 62.7 60.6 62.7 41.6 62.7 41.6 44.2 62.7 62.7 ...
#  $ write           : num  64.5 43.7 56.7 56.7 46.3 64.5 39.1 39.1 51.5 64.5 ...
#  $ math            : num  44.5 44.7 70.5 54.7 38.4 61.4 56.3 46.3 54.4 38.3 ...
#  $ science         : num  52.6 52.6 58 58 36.3 58 45 36.3 49.8 55.8 ...
#  $ female          : int  1 1 0 0 1 1 0 0 1 1 ...

psychademic$motivation <- as.character(round(psychademic$motivation * 3 + 1))

psychademic$sex <- (c("male", "female"))[c(psychademic$female + 1)]
psychademic$female <- NULL

attr(psychademic, "academic") <- c("read", "write", "math", "science", "sex")
attr(psychademic, "psychology") <- c(
  "locus_of_control",
  "self_concept",
  "motivation"
)


devtools::use_data(psychademic, overwrite = TRUE)
