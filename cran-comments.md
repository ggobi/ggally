
## Comments

### 2020-6-5

I have renamed the offending functions.

I have rerun package checks and revdep checks and updated the final results below. (tl/dr 0 revdep problems & 0 errors | 0 warnings | 0 notes)

Please let me know if there is anything else I can do.

Best,
Barret


### 2020-6-5

I talked with Joseph (co-author and author of the conflicting function names).  We are going to change our function names and resubmit.  This seems like the safest approach all around.

Thank you for your time and patience.

Best,
Barret


### 2020-6-5

Changes to worse in reverse depends:

Package: robustSingleCell
Check: whether package can be installed
New result: WARNING


  Found the following significant warnings:
    Warning: replacing previous import ‘GGally::mean_sd’ by ‘ggpubr::mean_sd’ when loading ‘robustSingleCell’
    Warning: replacing previous import ‘GGally::median_iqr’ by ‘ggpubr::median_iqr’ when loading ‘robustSingleCell’

- CRAN teams' auto-check service


### 2020-6-4

Changes since last submission:
* I removed the package vignettes (and will only host them on the package's website).
* I have altered the examples to only print the ggplot2 images if they are in an interactive session.
* I have made some of the longer running tests to be CI only. (On a weekly schedule)

The package size should be smaller (I did not receive any warnings) and the check time should be faster.

I have rerun package checks and revdep checks and updated the final results below.

Please let me know if there is anything else I can do.

Best,
Barret


### 2020-6-4

Thanks, we see:


   Size of tarball: 5726242 bytes

This is more than the 5MB CRAN threshhold and more than ever before?


Flavor: r-devel-windows-ix86+x86_64
Check: Overall checktime, Result: NOTE
   Overall checktime 14 min > 10 min

This is also rather extensive.


Can these be reduced?

Best,
Uwe Ligges


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
  * devel - Could not test. `Failed FTP upload: 550`

#### R CMD check results

* 0 errors | 0 warnings | 0 notes


## revdepcheck results

Link: https://github.com/ggobi/ggally/blob/rc-v2.0.0/revdep/README.md

We checked 98 reverse dependencies (77 from CRAN + 21 from BioConductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
