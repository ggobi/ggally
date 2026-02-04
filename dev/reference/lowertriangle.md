# lowertriangle - rearrange dataset as the preparation of [`ggscatmat`](https://ggobi.github.io/ggally/dev/reference/ggscatmat.md) function

function for making the melted dataset used to plot the lowertriangle
scatterplots.

## Usage

``` r
lowertriangle(data, columns = 1:ncol(data), color = NULL)
```

## Arguments

- data:

  a data matrix. Should contain numerical (continuous) data.

- columns:

  an option to choose the column to be used in the raw dataset. Defaults
  to `1:ncol(data)`

- color:

  an option to choose a factor variable to be grouped with. Defaults to
  `(NULL)`

## Author

Mengjia Ni, Di Cook

## Examples

``` r
data(flea)
head(lowertriangle(flea, columns = 2:4))
#>   xvalue yvalue xslot yslot  xlab  ylab
#> 1    191     NA     1     1 tars1 tars1
#> 2    185     NA     1     1 tars1 tars1
#> 3    200     NA     1     1 tars1 tars1
#> 4    173     NA     1     1 tars1 tars1
#> 5    171     NA     1     1 tars1 tars1
#> 6    160     NA     1     1 tars1 tars1
head(lowertriangle(flea))
#>   xvalue yvalue xslot yslot  xlab  ylab
#> 1    191     NA     1     1 tars1 tars1
#> 2    185     NA     1     1 tars1 tars1
#> 3    200     NA     1     1 tars1 tars1
#> 4    173     NA     1     1 tars1 tars1
#> 5    171     NA     1     1 tars1 tars1
#> 6    160     NA     1     1 tars1 tars1
head(lowertriangle(flea, color = "species"))
#>   xvalue yvalue xslot yslot  xlab  ylab colorcolumn
#> 1    191     NA     1     1 tars1 tars1    Concinna
#> 2    185     NA     1     1 tars1 tars1    Concinna
#> 3    200     NA     1     1 tars1 tars1    Concinna
#> 4    173     NA     1     1 tars1 tars1    Concinna
#> 5    171     NA     1     1 tars1 tars1    Concinna
#> 6    160     NA     1     1 tars1 tars1    Concinna
```
