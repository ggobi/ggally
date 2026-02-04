# Function switch

Function that allows you to call different functions based upon an
aesthetic variable value.

## Usage

``` r
fn_switch(types, mapping_val = "y")
```

## Arguments

- types:

  list of functions that follow the
  [`ggmatrix`](https://ggobi.github.io/ggally/dev/reference/ggmatrix.md)
  function standard:
  `function(data, mapping, ...){ #make ggplot2 object }`. One key should
  be a 'default' key for a default switch case.

- mapping_val:

  mapping value to switch on. Defaults to the 'y' variable of the
  aesthetics list.

## Examples

``` r
ggnostic_continuous_fn <- fn_switch(list(
  default = ggally_points,
  .fitted = ggally_points,
  .se.fit = ggally_nostic_se_fit,
  .resid = ggally_nostic_resid,
  .hat = ggally_nostic_hat,
  .sigma = ggally_nostic_sigma,
  .cooksd = ggally_nostic_cooksd,
  .std.resid = ggally_nostic_std_resid
))

ggnostic_combo_fn <- fn_switch(list(
  default = ggally_box_no_facet,
  fitted = ggally_box_no_facet,
  .se.fit = ggally_nostic_se_fit,
  .resid = ggally_nostic_resid,
  .hat = ggally_nostic_hat,
  .sigma = ggally_nostic_sigma,
  .cooksd = ggally_nostic_cooksd,
  .std.resid = ggally_nostic_std_resid
))
```
