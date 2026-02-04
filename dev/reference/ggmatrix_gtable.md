# [`ggmatrix`](https://ggobi.github.io/ggally/dev/reference/ggmatrix.md) gtable object

Specialized method to print the
[`ggmatrix`](https://ggobi.github.io/ggally/dev/reference/ggmatrix.md)
object.

## Usage

``` r
ggmatrix_gtable(
  pm,
  ...,
  progress = NULL,
  progress_format = formals(ggmatrix_progress)$format
)
```

## Arguments

- pm:

  [`ggmatrix`](https://ggobi.github.io/ggally/dev/reference/ggmatrix.md)
  object to be plotted

- ...:

  ignored

- progress, progress_format:

  **\[deprecated\]** Please use the 'progress' parameter in your
  [`ggmatrix`](https://ggobi.github.io/ggally/dev/reference/ggmatrix.md)-like
  function. See
  [`ggmatrix_progress`](https://ggobi.github.io/ggally/dev/reference/ggmatrix_progress.md)
  for a few examples.

## Author

Barret Schloerke

## Examples

``` r
data(tips)
pm <- ggpairs(tips, c(1, 3, 2), mapping = ggplot2::aes(color = sex))
ggmatrix_gtable(pm)
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.
#> TableGrob (21 x 18) "layout": 40 grobs
#>     z         cells             name
#> 1   0 ( 1-21, 1-18)       background
#> 2   1 (10-10, 7- 7)        panel-1-1
#> 3   1 (12-12, 7- 7)        panel-1-2
#> 4   1 (14-14, 7- 7)        panel-1-3
#> 5   1 (10-10, 9- 9)        panel-2-1
#> 6   1 (12-12, 9- 9)        panel-2-2
#> 7   1 (14-14, 9- 9)        panel-2-3
#> 8   1 (10-10,11-11)        panel-3-1
#> 9   1 (12-12,11-11)        panel-3-2
#> 10  1 (14-14,11-11)        panel-3-3
#> 11  3 ( 8- 8, 7- 7)         axis-t-1
#> 12  3 ( 8- 8, 9- 9)         axis-t-2
#> 13  3 ( 8- 8,11-11)         axis-t-3
#> 14  3 (15-15, 7- 7)         axis-b-1
#> 15  3 (15-15, 9- 9)         axis-b-2
#> 16  3 (15-15,11-11)         axis-b-3
#> 17  3 (10-10, 6- 6)         axis-l-1
#> 18  3 (12-12, 6- 6)         axis-l-2
#> 19  3 (14-14, 6- 6)         axis-l-3
#> 20  3 (10-10,13-13)         axis-r-1
#> 21  3 (12-12,13-13)         axis-r-2
#> 22  3 (14-14,13-13)         axis-r-3
#> 23  2 ( 9- 9, 7- 7)        strip-t-1
#> 24  2 ( 9- 9, 9- 9)        strip-t-2
#> 25  2 ( 9- 9,11-11)        strip-t-3
#> 26  2 (10-10,12-12)        strip-r-1
#> 27  2 (12-12,12-12)        strip-r-2
#> 28  2 (14-14,12-12)        strip-r-3
#> 29  4 ( 7- 7, 7-11)           xlab-t
#> 30  5 (16-16, 7-11)           xlab-b
#> 31  6 (10-14, 5- 5)           ylab-l
#> 32  7 (10-14,14-14)           ylab-r
#> 33  8 (10-14,16-16)  guide-box-right
#> 34  9 (10-14, 3- 3)   guide-box-left
#> 35 10 (18-18, 7-11) guide-box-bottom
#> 36 11 ( 5- 5, 7-11)    guide-box-top
#> 37 12 (10-14, 7-11) guide-box-inside
#> 38 13 ( 4- 4, 7-11)         subtitle
#> 39 14 ( 3- 3, 7-11)            title
#> 40 15 (19-19, 7-11)          caption
#>                                       grob
#> 1        rect[plot.background..rect.39399]
#> 2                           gtable[layout]
#> 3                           gtable[layout]
#> 4                           gtable[layout]
#> 5                           gtable[layout]
#> 6                           gtable[layout]
#> 7                           gtable[layout]
#> 8                           gtable[layout]
#> 9                           gtable[layout]
#> 10                          gtable[layout]
#> 11                          zeroGrob[NULL]
#> 12                          zeroGrob[NULL]
#> 13                          zeroGrob[NULL]
#> 14                          gtable[layout]
#> 15                          gtable[layout]
#> 16                          gtable[layout]
#> 17                          gtable[layout]
#> 18                          gtable[layout]
#> 19                          gtable[layout]
#> 20                          zeroGrob[NULL]
#> 21                          zeroGrob[NULL]
#> 22                          zeroGrob[NULL]
#> 23                           gtable[strip]
#> 24                           gtable[strip]
#> 25                           gtable[strip]
#> 26                           gtable[strip]
#> 27                           gtable[strip]
#> 28                           gtable[strip]
#> 29                          zeroGrob[NULL]
#> 30                          zeroGrob[NULL]
#> 31                          zeroGrob[NULL]
#> 32                          zeroGrob[NULL]
#> 33                          zeroGrob[NULL]
#> 34                          zeroGrob[NULL]
#> 35                          zeroGrob[NULL]
#> 36                          zeroGrob[NULL]
#> 37                          zeroGrob[NULL]
#> 38 zeroGrob[plot.subtitle..zeroGrob.39396]
#> 39    zeroGrob[plot.title..zeroGrob.39395]
#> 40  zeroGrob[plot.caption..zeroGrob.39397]
```
