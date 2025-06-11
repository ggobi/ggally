
## Comments
#### 2024-02-13

I am updating GGally for support for the latest ggplot2 release.

Best,
Barret


## R CMD check results

* 0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... [11s] NOTE
  Maintainer: ‘Barret Schloerke <schloerke@gmail.com>’

  Found the following (possibly) invalid URLs:
    URL: https://www.oecd.org/pisa/pisaproducts/database-cbapisa2012.htm
      From: man/australia_PISA2012.Rd
      Status: 403
      Message: Forbidden

When visiting the link manually, the link works as expected. I believe they have a bot blocker that is causing this error.


## revdepcheck results

We checked 159 reverse dependencies (128 from CRAN + 31 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 5 packages

Issues with CRAN packages are summarised below.

### Failed to check packages below due to installation compilation errors

* airGR
* fingerPro
* loon.ggplot
* robustbase
* simmr
