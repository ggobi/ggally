# [`ggmatrix`](https://ggobi.github.io/ggally/dev/reference/ggmatrix.md) structure

View the condensed version of the
[`ggmatrix`](https://ggobi.github.io/ggally/dev/reference/ggmatrix.md)
object. The attribute "class" is ALWAYS altered to "\_class" to avoid
recursion.

## Usage

``` r
# S3 method for class 'ggmatrix'
str(object, ..., raw = FALSE)
```

## Arguments

- object:

  [`ggmatrix`](https://ggobi.github.io/ggally/dev/reference/ggmatrix.md)
  object to be viewed

- ...:

  passed on to the default `str` method

- raw:

  boolean to determine if the plots should be converted to text or kept
  as original objects
