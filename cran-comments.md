
## Comments

### 2020-6-3

I have removed `packagedocs` as a dependency.  This fixes the errors on CRAN check.

With the removal of `packagedocs` and switching to true vignettes, the package size has grown a bit due to the nature of having vignettes containing **many** `ggplot2` images.

Please let me know if there is anything else I can do.

Best,
Barret


### 2020-5-20

Package `lazyrmd` is orphaned.

Package `packagedocs` requires it, which is not allowed under the CRAN policy.

Packages `GGally` and `geofacet` suggest `packagedocs` but do not use it
conditionally.

We need one of

a) `lazyrmd` to be maintained (as we have asked before) or
b) `packagedocs` to work without lazyrmd or
c) `lazyrmd` and `packagedocs` to be archived and `geofacet`/`GGally` use a
different vignette builder.

Please resolve one way or the other before Jun 3.

--
Brian D. Ripley



## Test environments and R CMD check results

* local macOS install 10.15.5
  * R 4.0
* GitHub Actions - https://github.com/ggobi/ggally/pull/364/checks
  * macOS, windows - R devel
  * macOS, windows, ubuntu 16 - R 4.0
  * macOS, windows, ubuntu 16 - R 3.6

* win-builder
  * oldrelease
  * release
  * devel

#### R CMD check results

win-builder
* 0 errors | 0 warnings | 0 notes


Local & GitHub Actions
* 0 errors | 0 warnings | 1 notes

N  checking installed package size ...
     installed size is  7.8Mb
     sub-directories of 1Mb or more:
       doc   6.1Mb



## revdepcheck results

Link: https://github.com/ggobi/ggally/commit/8a6b8ffbf6f7187dc9503ac51c3f5f48b3f72a1c

We checked 98 reverse dependencies (77 from CRAN + 21 from BioConductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 1 new problems
 * We failed to check 0 packages

#### New revdepcheck problems

One package had an extra warning produced due to a import clash. A PR was made here: https://github.com/asmagen/robustSingleCell/pull/34
This is not a breaking change, so I did not deem it necessary to wait two weeks before submitting to CRAN (delaying a CRAN deadline).

* robustSingleCell
  * checking whether package ‘robustSingleCell’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘GGally::mean_sd’ by ‘ggpubr::mean_sd’ when loading ‘robustSingleCell’
      Warning: replacing previous import ‘GGally::median_iqr’ by ‘ggpubr::median_iqr’ when loading ‘robustSingleCell’
    ```
