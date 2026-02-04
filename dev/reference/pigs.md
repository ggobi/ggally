# United Kingdom Pig Production

This data contains about the United Kingdom Pig Production from the book
'Data' by Andrews and Herzberg. The original data can be on Statlib:
http://lib.stat.cmu.edu/datasets/Andrews/T62.1

## Usage

``` r
data(pigs)
```

## Format

A data frame with 48 rows and 8 variables

## Details

The time variable has been added from a combination of year and quarter

- time year + (quarter - 1) / 4

- year year of production

- quarter quarter of the year of production

- gilts number of sows giving birth for the first time

- profit ratio of price to an index of feed price

- s_per_herdsz ratio of the number of breeding pigs slaughtered to the
  total breeding herd size

- production number of pigs slaughtered that were reared for meat

- herdsz breeding herd size

## References

Andrews, David F., and Agnes M. Herzberg. Data: a collection of problems
from many fields for the student and research worker. Springer Science &
Business Media, 2012.
