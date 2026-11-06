# ggcoef(): Model coefficients

``` r

library(GGally)
#> Loading required package: ggplot2
```

## `GGally::ggcoef()`

The purpose of this function is to quickly plot the coefficients of a
model. For an updated and improved version, see
[`GGally::ggcoef_model()`](https://larmarange.github.io/ggstats/reference/ggcoef_model.html)
and the [corresponding
vignette](https://ggobi.github.io/ggally/articles/ggcoef_model.html).

### Quick coefficients plot

To work automatically, this function requires the `broom` package.
Simply call
[`ggcoef()`](https://ggobi.github.io/ggally/dev/reference/ggcoef.md)
with a model object. It could be the result of
[`stats::lm`](https://rdrr.io/r/stats/lm.html),
[`stats::glm`](https://rdrr.io/r/stats/glm.html) or any other model
covered by `broom` and its
[`broom::tidy`](https://generics.r-lib.org/reference/tidy.html)
method[^1].

``` r

reg <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data = iris)
ggcoef(reg)
```

![Dot plot of linear model coefficients for predicting sepal length,
with one row per term (intercept, sepal width, petal length, petal
width) and horizontal confidence interval bars. A dotted vertical line
marks zero; petal width is the only negative
coefficient.](ggcoef_files/figure-html/ggcoef-reg-1.png)

In the case of a logistic regression (or any other model for which
coefficients are usually exponentiated), simply indicated
`exponentiate = TRUE`. Note that a logarithmic scale will be used for
the x-axis.

``` r

d <- as.data.frame(Titanic)
log.reg <- glm(Survived ~ Sex + Age + Class, family = binomial, data = d, weights = d$Freq)
ggcoef(log.reg, exponentiate = TRUE)
```

![Dot plot of odds ratios from a logistic regression of Titanic survival
on sex, age, and class, on a log x-axis with confidence interval bars. A
dotted line marks 1; being female raises the odds of survival about
elevenfold, while adult age and every class other than first lower
it.](ggcoef_files/figure-html/ggcoef-titanic-1.png)

### Customizing the plot

You can use `conf.int`, `vline` and `exclude_intercept` to display or
not confidence intervals as error bars, a vertical line for `x = 0` (or
`x = 1` if coefficients are exponentiated) and the intercept.

``` r

ggcoef(reg, vline = FALSE, conf.int = FALSE, exclude_intercept = TRUE)
```

![Dot plot of the sepal length model coefficients for sepal width, petal
length, and petal width only, without confidence interval bars, without
the intercept, and without a vertical reference
line.](ggcoef_files/figure-html/ggcoef-reg-custom-1.png)

See the help page of
[`ggcoef()`](https://ggobi.github.io/ggally/dev/reference/ggcoef.md) for
the full list of arguments that could be used to personalize how error
bars and the vertical line are plotted.

``` r

ggcoef(
  log.reg,
  exponentiate = TRUE,
  vline_color = "red",
  vline_linetype = "solid",
  errorbar_color = "blue",
  errorbar_height = .25
)
```

![Dot plot of Titanic survival odds ratios on a log x-axis, with blue
confidence interval bars that have tall end caps and a solid red
vertical line at 1.](ggcoef_files/figure-html/ggcoef-full-args-1.png)

Additional parameters will be passed to \[ggplot2::geom_point()\].

``` r

ggcoef(log.reg, exponentiate = TRUE, color = "purple", size = 5, shape = 18)
```

![Dot plot of Titanic survival odds ratios on a log x-axis, with each
estimate drawn as a large purple
diamond.](ggcoef_files/figure-html/ggcoef-log.reg-1.png)

Finally, you can also customize the aesthetic mapping of the points.

``` r

library(ggplot2)
ggcoef(log.reg, exponentiate = TRUE, mapping = aes(x = estimate, y = term, size = p.value)) +
  scale_size_continuous(trans = "reverse")
#> Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
#> ℹ Please use `linewidth` instead.
#> ℹ The deprecated feature was likely used in the GGally package.
#>   Please report the issue at <https://github.com/ggobi/ggally/issues>.
#> This warning is displayed once per session.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning
#> was generated.
```

![Dot plot of Titanic survival odds ratios on a log x-axis where point
size shows the p-value on a reversed scale, so the most significant
terms have the largest
points.](ggcoef_files/figure-html/ggcoef-aes-1.png)

### Custom data frame

You can also pass a custom data frame to \[ggcoef()\]. The following
variables are expected:

- `term` (except if you customize the mapping)
- `estimate` (except if you customize the mapping)
- `conf.low` and `conf.high` (only if you want to display error bars)

``` r

cust <- data.frame(
  term = c("male vs. female", "30-49 vs. 18-29", "50+ vs. 18-29", "urban vs. rural"),
  estimate = c(.456, 1.234, 1.897, 1.003),
  conf.low = c(.411, 1.042, 1.765, 0.678),
  conf.high = c(.498, 1.564, 2.034, 1.476),
  variable = c("sex", "age", "age", "residence")
)
cust$term <- factor(cust$term, cust$term)
ggcoef(cust, exponentiate = TRUE)
```

![Dot plot of odds ratios from a custom data frame on a log x-axis, with
confidence interval bars for four terms (male vs. female, 30-49 vs.
18-29, 50+ vs. 18-29, and urban vs. rural). Male vs. female is below 1,
both age terms are above 1, and urban vs. rural crosses
1.](ggcoef_files/figure-html/ggcoef-data-frame-1.png)

``` r

ggcoef(
  cust,
  exponentiate = TRUE,
  mapping = aes(x = estimate, y = term, colour = variable),
  size = 5
)
```

![The same odds ratio dot plot with large points coloured by variable
group (sex, age, and
residence).](ggcoef_files/figure-html/ggcoef-data-frame-2.png)

[^1]: See <http://www.rdocumentation.org/packages/broom>.
