
## Comments
#### 2024-02-09

I am updating GGally for support for the latest ggplot2 release.

Best,
Barret


## R CMD check results

* 0 errors | 0 warnings | 1 note

  Maintainer: ‘Barret Schloerke <schloerke@gmail.com>’

  Found the following (possibly) invalid URLs:
    URL: https://www.oecd.org/pisa/pisaproducts/database-cbapisa2012.htm
      From: man/australia_PISA2012.Rd
      Status: 403
      Message: Forbidden

When visiting the link manually, the link works as expected. I believe they have a bot blocker that is causing this error.


## revdepcheck results

We checked 158 reverse dependencies (128 from CRAN + 30 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 3 packages

Issues with CRAN packages are summarised below.

### Failed to check

* fingerPro   (NA) - depends on rjags; Could not install rjags
* loon.ggplot (NA) - Trying to attached to a null device
* simmr       (NA) - depends on rjags; Could not install rjags
