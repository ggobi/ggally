# ggally\_\*(): List of available high-level plots

The purpose of this vignette is to display all high-level plots
available in `GGally` to be used in particular with
[`ggduo()`](https://ggobi.github.io/ggally/dev/reference/ggduo.md) and
[`ggpairs()`](https://ggobi.github.io/ggally/dev/reference/ggpairs.md).
The name of all the corresponding functions are of the form
`ggally_*()`. Most of them accept a discrete variables to be passed to
the **colour** aesthetic.

We can distinct **bivariate plots** requiring two variables for **x**
and **y** axis respectively and **diagonal plots** when the same
variable is plotted on **x** and **y** axis.

``` r

library(GGally, quietly = TRUE)
data(tips)
```

## Bivariate plots

### with 2x continuous variables

#### `ggally_autopoint()`

``` r

ggally_autopoint(tips, aes(x = total_bill, y = tip))
```

![](ggally_plots_files/figure-html/unnamed-chunk-3-1.png)

``` r

ggally_autopoint(tips, aes(x = total_bill, y = tip, colour = time))
```

![](ggally_plots_files/figure-html/unnamed-chunk-3-2.png)

#### `ggally_cor()`

``` r

ggally_cor(tips, aes(x = total_bill, y = tip))
```

![](ggally_plots_files/figure-html/unnamed-chunk-4-1.png)

``` r

ggally_cor(tips, aes(x = total_bill, y = tip, colour = time))
```

![](ggally_plots_files/figure-html/unnamed-chunk-4-2.png)

See also
[`ggally_statistic()`](https://ggobi.github.io/ggally/dev/reference/ggally_statistic.md).

#### `ggally_density()`

``` r

ggally_density(tips, aes(x = total_bill, y = tip))
```

![](ggally_plots_files/figure-html/unnamed-chunk-5-1.png)

``` r

ggally_density(tips, aes(x = total_bill, y = tip, colour = time))
```

![](ggally_plots_files/figure-html/unnamed-chunk-5-2.png)

#### `ggally_points()`

``` r

ggally_points(tips, aes(x = total_bill, y = tip))
```

![](ggally_plots_files/figure-html/unnamed-chunk-6-1.png)

``` r

ggally_points(tips, aes(x = total_bill, y = tip, colour = time))
```

![](ggally_plots_files/figure-html/unnamed-chunk-6-2.png)

#### `ggally_smooth()`, `ggally_smooth_lm()` & `ggally_smooth_loess()`

``` r

ggally_smooth_lm(tips, aes(x = total_bill, y = tip))
```

![](ggally_plots_files/figure-html/unnamed-chunk-7-1.png)

``` r

ggally_smooth_lm(tips, aes(x = total_bill, y = tip, colour = time))
```

![](ggally_plots_files/figure-html/unnamed-chunk-7-2.png)

``` r


ggally_smooth_loess(tips, aes(x = total_bill, y = tip))
```

![](ggally_plots_files/figure-html/unnamed-chunk-7-3.png)

``` r

ggally_smooth_loess(tips, aes(x = total_bill, y = tip, colour = time))
```

![](ggally_plots_files/figure-html/unnamed-chunk-7-4.png)

See also
[`ggally_smooth()`](https://ggobi.github.io/ggally/dev/reference/ggally_smooth.md)
for more options.

### with 2x discrete variables

#### `ggally_colbar()`

``` r

ggally_colbar(tips, aes(x = day, y = smoker))
```

![](ggally_plots_files/figure-html/unnamed-chunk-8-1.png)

Note: the **colour** aesthetic is not taken into account.

#### `ggally_autopoint()`

``` r

ggally_autopoint(tips, aes(x = day, y = smoker))
```

![](ggally_plots_files/figure-html/unnamed-chunk-9-1.png)

``` r

ggally_autopoint(tips, aes(x = day, y = smoker, colour = time))
```

![](ggally_plots_files/figure-html/unnamed-chunk-9-2.png)

#### `ggally_count()`

``` r

ggally_count(tips, aes(x = day, y = smoker))
```

![](ggally_plots_files/figure-html/unnamed-chunk-10-1.png)

``` r

ggally_count(tips, aes(x = day, y = smoker, colour = time))
```

![](ggally_plots_files/figure-html/unnamed-chunk-10-2.png)

#### `ggally_cross()`

``` r

ggally_cross(tips, aes(x = day, y = smoker))
```

![](ggally_plots_files/figure-html/unnamed-chunk-11-1.png)

``` r

ggally_cross(tips, aes(x = day, y = smoker, colour = time))
```

![](ggally_plots_files/figure-html/unnamed-chunk-11-2.png)

``` r

ggally_cross(tips, aes(x = day, y = smoker, colour = smoker))
```

![](ggally_plots_files/figure-html/unnamed-chunk-11-3.png)

Note: **colour** aesthetic is taken into account only if it corresponds
to **x** or to **y**.

#### `ggally_crosstable()`

``` r

ggally_crosstable(tips, aes(x = day, y = smoker))
```

![](ggally_plots_files/figure-html/unnamed-chunk-12-1.png)

``` r

ggally_crosstable(tips, aes(x = day, y = smoker), cells = "col.prop", fill = "std.resid")
```

![](ggally_plots_files/figure-html/unnamed-chunk-12-2.png)

Note: **colour** aesthetic is not taken into account.

#### `ggally_facetbar()`

``` r

ggally_facetbar(tips, aes(x = day, y = smoker))
```

![](ggally_plots_files/figure-html/unnamed-chunk-13-1.png)

``` r

ggally_facetbar(tips, aes(x = day, y = smoker, colour = time))
```

![](ggally_plots_files/figure-html/unnamed-chunk-13-2.png)

#### `ggally_ratio()`

``` r

ggally_ratio(tips, aes(x = day, y = smoker))
```

![](ggally_plots_files/figure-html/unnamed-chunk-14-1.png)

``` r

ggally_ratio(tips, aes(x = day, y = smoker, colour = time))
```

![](ggally_plots_files/figure-html/unnamed-chunk-14-2.png)

#### `ggally_rowbar()`

``` r

ggally_rowbar(tips, aes(x = day, y = smoker))
```

![](ggally_plots_files/figure-html/unnamed-chunk-15-1.png)

Note: the **colour** aesthetic is not taken into account.

#### `ggally_table()`

``` r

ggally_table(tips, aes(x = day, y = smoker))
```

![](ggally_plots_files/figure-html/unnamed-chunk-16-1.png)

``` r

ggally_table(tips, aes(x = day, y = smoker, colour = time))
```

![](ggally_plots_files/figure-html/unnamed-chunk-16-2.png)

``` r

ggally_table(tips, aes(x = day, y = smoker, colour = smoker))
```

![](ggally_plots_files/figure-html/unnamed-chunk-16-3.png)

Note: **colour** aesthetic is taken into account only if it corresponds
to **x** or to **y**.

#### `ggally_trends()`

``` r

ggally_trends(tips, aes(x = day, y = smoker))
```

![](ggally_plots_files/figure-html/unnamed-chunk-17-1.png)

``` r

ggally_trends(tips, aes(x = day, y = smoker, colour = time))
```

![](ggally_plots_files/figure-html/unnamed-chunk-17-2.png)

### with 1x continuous and 1x discrete variables

#### `ggally_autopoint()`

``` r

ggally_autopoint(tips, aes(x = total_bill, y = day))
```

![](ggally_plots_files/figure-html/unnamed-chunk-18-1.png)

``` r

ggally_autopoint(tips, aes(x = total_bill, y = day, colour = time))
```

![](ggally_plots_files/figure-html/unnamed-chunk-18-2.png)

#### `ggally_box()` & `ggally_box_no_facet()`

``` r

ggally_box(tips, aes(x = total_bill, y = day))
```

![](ggally_plots_files/figure-html/unnamed-chunk-19-1.png)

``` r

ggally_box(tips, aes(x = total_bill, y = day, colour = time))
```

![](ggally_plots_files/figure-html/unnamed-chunk-19-2.png)

``` r

ggally_box_no_facet(tips, aes(x = total_bill, y = day))
```

![](ggally_plots_files/figure-html/unnamed-chunk-19-3.png)

``` r

ggally_box_no_facet(tips, aes(x = total_bill, y = day, colour = time))
```

![](ggally_plots_files/figure-html/unnamed-chunk-19-4.png)

#### `ggally_denstrip()`

``` r

ggally_denstrip(tips, aes(x = total_bill, y = day))
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.
```

![](ggally_plots_files/figure-html/unnamed-chunk-20-1.png)

``` r

ggally_denstrip(tips, aes(x = total_bill, y = day, colour = time))
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.
```

![](ggally_plots_files/figure-html/unnamed-chunk-20-2.png)

#### `ggally_dot()` & `ggally_dot_no_facet()`

``` r

ggally_dot(tips, aes(x = total_bill, y = day))
```

![](ggally_plots_files/figure-html/unnamed-chunk-21-1.png)

``` r

ggally_dot(tips, aes(x = total_bill, y = day, colour = time))
```

![](ggally_plots_files/figure-html/unnamed-chunk-21-2.png)

``` r

ggally_dot_no_facet(tips, aes(x = total_bill, y = day))
```

![](ggally_plots_files/figure-html/unnamed-chunk-21-3.png)

``` r

ggally_dot_no_facet(tips, aes(x = total_bill, y = day, colour = time))
```

![](ggally_plots_files/figure-html/unnamed-chunk-21-4.png)

#### `ggally_facetdensitystrip()`

``` r

ggally_facetdensitystrip(tips, aes(x = total_bill, y = day))
```

![](ggally_plots_files/figure-html/unnamed-chunk-22-1.png)

``` r

ggally_facetdensitystrip(tips, aes(x = total_bill, y = day, colour = time))
#> Warning: Groups with fewer than two data points have been
#> dropped.
#> Warning: Removed 1 row containing missing values or values outside the scale
#> range (`geom_line()`).
```

![](ggally_plots_files/figure-html/unnamed-chunk-22-2.png)

#### `ggally_facethist()`

``` r

ggally_facethist(tips, aes(x = total_bill, y = day))
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.
```

![](ggally_plots_files/figure-html/unnamed-chunk-23-1.png)

``` r

ggally_facethist(tips, aes(x = total_bill, y = day, colour = time))
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.
```

![](ggally_plots_files/figure-html/unnamed-chunk-23-2.png)

#### `ggally_summarise_by()`

``` r

ggally_summarise_by(tips, aes(x = total_bill, y = day))
```

![](ggally_plots_files/figure-html/unnamed-chunk-24-1.png)

``` r

ggally_summarise_by(tips, aes(x = total_bill, y = day, colour = day))
```

![](ggally_plots_files/figure-html/unnamed-chunk-24-2.png)

Note: colour aesthetic is kept only if corresponding to the discrete
axis.

#### `ggally_trends()`

``` r

ggally_trends(tips, aes(y = total_bill, x = day))
```

![](ggally_plots_files/figure-html/unnamed-chunk-25-1.png)

``` r

ggally_trends(tips, aes(y = total_bill, x = day, colour = time))
```

![](ggally_plots_files/figure-html/unnamed-chunk-25-2.png)

## Diagonal plots

### with 1x continuous variable

#### `ggally_autopointDiag()`

``` r

ggally_autopointDiag(tips, aes(x = total_bill))
```

![](ggally_plots_files/figure-html/unnamed-chunk-26-1.png)

``` r

ggally_autopointDiag(tips, aes(x = total_bill, colour = time))
```

![](ggally_plots_files/figure-html/unnamed-chunk-26-2.png)

#### `ggally_barDiag()`

``` r

ggally_barDiag(tips, aes(x = total_bill))
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.
```

![](ggally_plots_files/figure-html/unnamed-chunk-27-1.png)

``` r

ggally_barDiag(tips, aes(x = total_bill, colour = time))
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.
```

![](ggally_plots_files/figure-html/unnamed-chunk-27-2.png)

#### `ggally_densityDiag()`

``` r

ggally_densityDiag(tips, aes(x = total_bill))
```

![](ggally_plots_files/figure-html/unnamed-chunk-28-1.png)

``` r

ggally_densityDiag(tips, aes(x = total_bill, colour = time))
```

![](ggally_plots_files/figure-html/unnamed-chunk-28-2.png)

### with 1x discrete variable

#### `ggally_autopointDiag()`

``` r

ggally_autopointDiag(tips, aes(x = day))
```

![](ggally_plots_files/figure-html/unnamed-chunk-29-1.png)

``` r

ggally_autopointDiag(tips, aes(x = day, colour = time))
```

![](ggally_plots_files/figure-html/unnamed-chunk-29-2.png)

#### `ggally_barDiag()`

``` r

ggally_barDiag(tips, aes(x = day))
```

![](ggally_plots_files/figure-html/unnamed-chunk-30-1.png)

``` r

ggally_barDiag(tips, aes(x = day, colour = time))
```

![](ggally_plots_files/figure-html/unnamed-chunk-30-2.png)

#### `ggally_countDiag()`

``` r

ggally_countDiag(tips, aes(x = day))
```

![](ggally_plots_files/figure-html/unnamed-chunk-31-1.png)

``` r

ggally_countDiag(tips, aes(x = day, colour = time))
```

![](ggally_plots_files/figure-html/unnamed-chunk-31-2.png)

#### `ggally_densityDiag()`

``` r

ggally_densityDiag(tips, aes(x = day))
```

![](ggally_plots_files/figure-html/unnamed-chunk-32-1.png)

``` r

ggally_densityDiag(tips, aes(x = day, colour = time))
#> Warning: Groups with fewer than two data points have been
#> dropped.
#> Warning: Removed 1 row containing missing values or values outside the scale
#> range (`geom_density()`).
```

![](ggally_plots_files/figure-html/unnamed-chunk-32-2.png)

## Additional plots

- [`ggally_statistic()`](https://ggobi.github.io/ggally/dev/reference/ggally_statistic.md)
  and
  [`ggally_text()`](https://ggobi.github.io/ggally/dev/reference/ggally_text.md)
  to display custom text
- [`ggally_blank()`](https://ggobi.github.io/ggally/dev/reference/ggally_blank.md)
  and
  [`ggally_blankDiag()`](https://ggobi.github.io/ggally/dev/reference/ggally_blank.md)
  for blank plot
- [`ggally_na()`](https://ggobi.github.io/ggally/dev/reference/ggally_na.md)
  and
  [`ggally_naDiag()`](https://ggobi.github.io/ggally/dev/reference/ggally_na.md)
  to display a large `NA`
