# cor

    Code
      ggally_cor(ti, ggplot2::aes(x = total_bill, y = tip, color = size))
    Condition
      Error in `ggally_statistic()`:
      ! `mapping` color column must be categorical, not numeric

# diagAxis

    Code
      ggally_diagAxis(iris, mapping = ggplot2::aes(y = Sepal.Length))
    Condition
      Error in `ggally_diagAxis()`:
      ! mapping$x is null. There must be a column value in this location.

