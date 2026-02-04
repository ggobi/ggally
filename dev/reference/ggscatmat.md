# Traditional scatterplot matrix for purely quantitative variables

This function makes a scatterplot matrix for quantitative variables with
density plots on the diagonal and correlation printed in the upper
triangle.

## Usage

``` r
ggscatmat(
  data,
  columns = 1:ncol(data),
  color = NULL,
  alpha = 1,
  corMethod = "pearson"
)
```

## Arguments

- data:

  a data matrix. Should contain numerical (continuous) data.

- columns:

  an option to choose the column to be used in the raw dataset. Defaults
  to `1:ncol(data)`.

- color:

  an option to group the dataset by the factor variable and color them
  by different colors. Defaults to `NULL`, i.e. no coloring. If
  supplied, it will be converted to a factor.

- alpha:

  an option to set the transparency in scatterplots for large data.
  Defaults to `1`.

- corMethod:

  method argument supplied to [`cor`](https://rdrr.io/r/stats/cor.html)

## Author

Mengjia Ni, Di Cook

## Examples

``` r
# small function to display plots only if it's interactive
p_ <- GGally::print_if_interactive

data(flea)

p_(ggscatmat(flea, columns = 2:4))

p_(ggscatmat(flea, columns = 2:4, color = "species"))
```
