# Broomify a model

broom::augment a model and add broom::glance and broom::tidy output as
attributes. X and Y variables are also added.

## Usage

``` r
broomify(model, lmStars = TRUE)
```

## Arguments

- model:

  model to be sent to
  [`broom::augment()`](https://broom.tidymodels.org/reference/reexports.html),
  [`broom::glance()`](https://broom.tidymodels.org/reference/reexports.html),
  and
  [`broom::tidy()`](https://broom.tidymodels.org/reference/reexports.html)

- lmStars:

  boolean that determines if stars are added to labels

## Value

broom::augmented data frame with the broom::glance data.frame and
broom::tidy data.frame as 'broom_glance' and 'broom_tidy' attributes
respectively. `var_x` and `var_y` variables are also added as attributes

## Examples

``` r
data(mtcars)
model <- stats::lm(mpg ~ wt + qsec + am, data = mtcars)
if (require(broom)) {
  broomified_model <- broomify(model)
  str(broomified_model)
}
#> Loading required package: broom
#> tibble[,12] (S3: tbl_df/tbl/data.frame/broomify)
#>  $ .rownames : chr [1:32] "Mazda RX4" "Mazda RX4 Wag" "Datsun 710" "Hornet 4 Drive" ...
#>  $ mpg       : num [1:32] 21 21 22.8 21.4 18.7 18.1 14.3 24.4 22.8 19.2 ...
#>  $ wt        : num [1:32] 2.62 2.88 2.32 3.21 3.44 ...
#>  $ qsec      : num [1:32] 16.5 17 18.6 19.4 17 ...
#>  $ am        : num [1:32] 1 1 1 0 0 0 0 0 0 0 ...
#>  $ .fitted   : num [1:32] 22.5 22.2 26.3 20.9 17 ...
#>  $ .se.fit   : Named num [1:32] 0.72 0.744 0.76 0.685 0.749 ...
#>   ..- attr(*, "names")= chr [1:32] "Mazda RX4" "Mazda RX4 Wag" "Datsun 710" "Hornet 4 Drive" ...
#>  $ .resid    : num [1:32] -1.47 -1.158 -3.481 0.543 1.69 ...
#>  $ .hat      : num [1:32] 0.0857 0.0914 0.0955 0.0776 0.0927 ...
#>  $ .sigma    : num [1:32] 2.49 2.49 2.4 2.5 2.48 ...
#>  $ .cooksd   : num [1:32] 0.00916 0.00614 0.05847 0.00111 0.0133 ...
#>  $ .std.resid: num [1:32] -0.625 -0.494 -1.489 0.23 0.722 ...
#>  - attr(*, "terms")=Classes 'terms', 'formula'  language mpg ~ wt + qsec + am
#>   .. ..- attr(*, "variables")= language list(mpg, wt, qsec, am)
#>   .. ..- attr(*, "factors")= int [1:4, 1:3] 0 1 0 0 0 0 1 0 0 0 ...
#>   .. .. ..- attr(*, "dimnames")=List of 2
#>   .. .. .. ..$ : chr [1:4] "mpg" "wt" "qsec" "am"
#>   .. .. .. ..$ : chr [1:3] "wt" "qsec" "am"
#>   .. ..- attr(*, "term.labels")= chr [1:3] "wt" "qsec" "am"
#>   .. ..- attr(*, "order")= int [1:3] 1 1 1
#>   .. ..- attr(*, "intercept")= int 1
#>   .. ..- attr(*, "response")= int 1
#>   .. ..- attr(*, ".Environment")=<environment: 0x562403f269e8> 
#>   .. ..- attr(*, "predvars")= language list(mpg, wt, qsec, am)
#>   .. ..- attr(*, "dataClasses")= Named chr [1:4] "numeric" "numeric" "numeric" "numeric"
#>   .. .. ..- attr(*, "names")= chr [1:4] "mpg" "wt" "qsec" "am"
#>  - attr(*, "broom_glance")= tibble [1 × 12] (S3: tbl_df/tbl/data.frame)
#>   ..$ r.squared    : num 0.85
#>   ..$ adj.r.squared: num 0.834
#>   ..$ sigma        : num 2.46
#>   ..$ statistic    : Named num 52.7
#>   .. ..- attr(*, "names")= chr "value"
#>   ..$ p.value      : Named num 1.21e-11
#>   .. ..- attr(*, "names")= chr "value"
#>   ..$ df           : Named num 3
#>   .. ..- attr(*, "names")= chr "numdf"
#>   ..$ logLik       : num -72.1
#>   ..$ AIC          : num 154
#>   ..$ BIC          : num 161
#>   ..$ deviance     : num 169
#>   ..$ df.residual  : int 28
#>   ..$ nobs         : int 32
#>  - attr(*, "broom_tidy")= tibble [4 × 5] (S3: tbl_df/tbl/data.frame)
#>   ..$ term     : chr [1:4] "(Intercept)" "wt" "qsec" "am"
#>   ..$ estimate : num [1:4] 9.62 -3.92 1.23 2.94
#>   ..$ std.error: num [1:4] 6.96 0.711 0.289 1.411
#>   ..$ statistic: num [1:4] 1.38 -5.51 4.25 2.08
#>   ..$ p.value  : num [1:4] 1.78e-01 6.95e-06 2.16e-04 4.67e-02
#>  - attr(*, "var_x")= chr [1:3] "wt" "qsec" "am"
#>  - attr(*, "var_y")= chr "mpg"
#>  - attr(*, "var_x_label")= chr [1:3] "wt***" "qsec***" "am*"
```
