
## Comments
#### 2025-07-17

I am updating GGally for support for the latest ggplot2 v4 release.

Best,
Barret


## R CMD check results

* 0 errors | 0 warnings | 1 note

```
* checking CRAN incoming feasibility ... [16s] NOTE
Maintainer: 'Barret Schloerke <schloerke@gmail.com>'

Found the following (possibly) invalid URLs:
  URL: https://www.oecd.org/en/data/datasets/pisa-2012-cba-database.html
    From: man/australia_PISA2012.Rd
    Status: 403
    Message: Forbidden
```

When visiting the link manually, the link works as expected. I believe they have a bot blocker that is causing this error.

## revdepcheck results

We checked 141 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 4 new problems
 * We failed to check 0 packages

Issues with CRAN packages are summarised below.

### New problems
(This reports the first line of each new failure)

* comparer
  checking tests ... ERROR

* ezEDA
  checking tests ... ERROR

* rbioacc
  checking tests ... ERROR

* tidycomm
  checking tests ... ERROR

### PRs to fix problems

Downstream PRs which have been given 2 weeks notice. All PRs were submitted on June 30th, 2025

* comparer: https://github.com/CollinErickson/comparer/pull/1
* ezEDA: https://github.com/kviswana/ezEDA/pull/3
* ggPMX: https://github.com/ggPMXdevelopment/ggPMX/pull/398
* tidycomm: https://github.com/joon-e/tidycomm/pull/61

### Fixes already performed
* rbioacc:
  * Failing checks already fixed in https://gitlab.in2p3.fr/mosaic-software/rbioacc/-/blob/e5822989514fd18fd04a35c73616e9dc79d5abcd/tests/testthat/test-correlation.R#L28-32
