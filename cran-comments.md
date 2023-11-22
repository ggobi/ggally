
## Comments
#### 2023-11-22

Thank you for being patient with me on submission time.

I have fixed the packageVersion issue by removing the unecessary code: https://github.com/ggobi/ggally/commit/9e7228c6d4219c515857426a982d1a5c1ff0cd13

The update also contains bug fixes and new datasets.

Please let me know if there is anything else I can do to help. Thank you again for your patience!!

Best,
Barret


#### 2023-11-07

Can you please change to "3.3"?

Please fix before 2023-11-21 to safely retain your package on CRAN.

Best
-k


#### 2023-07-18

[packageVersion() should only compare against strings.]

....

Please correct before 2023-08-16 to safely retain your package on CRAN.

....

Best
-k

**********************************

$GGally
$GGally$`GGally/R/gg-plots.R`
$GGally$`GGally/R/gg-plots.R`[[1]]
packageVersion("ggplot2") >= 3.3



## R CMD check results

* 0 errors | 0 warnings | 1 note

  Maintainer: ‘Barret Schloerke <schloerke@gmail.com>’

  Found the following (possibly) invalid URLs:
    URL: https://www.oecd.org/pisa/pisaproducts/database-cbapisa2012.htm
      From: man/australia_PISA2012.Rd
      Status: 403
      Message: Forbidden

When visiting it manually, the link works as expected. I believe they have a bot blocker that is causing this error.


## revdepcheck results

We checked 158 reverse dependencies (128 from CRAN + 30 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 3 packages

Issues with CRAN packages are summarised below.

### Failed to check

* fingerPro   (NA) - depends on rjags; Could not install rjags
* loon.ggplot (NA) - Trying to attached to a null device
* simmr       (NA) - depends on rjags; Could not install rjags
