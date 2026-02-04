# Print [`ggmatrix`](https://ggobi.github.io/ggally/dev/reference/ggmatrix.md) object

Print method taken from `ggplot2:::print.ggplot` and altered for a
[`ggmatrix`](https://ggobi.github.io/ggally/dev/reference/ggmatrix.md)
object

## Arguments

- x:

  plot to display

- newpage:

  draw new (empty) page first?

- vp:

  viewport to draw plot in

- ...:

  arguments passed onto
  [`ggmatrix_gtable`](https://ggobi.github.io/ggally/dev/reference/ggmatrix_gtable.md)

## Author

Barret Schloerke

## Examples

``` r
data(tips)
pMat <- ggpairs(tips, c(1, 3, 2), mapping = ggplot2::aes(color = sex))
pMat # calls print(pMat), which calls print.ggmatrix(pMat)
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.
```
