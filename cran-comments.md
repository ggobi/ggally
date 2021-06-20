
## Comments
#### 2021-06-20

I have fixed the failing tests due to an update in `network` package. `GGally` now requires the latest version of the `network` package and should pass the tests.

Thank you,
Barret

#### 2021-06-08

Dear maintainer,

Please see the problems shown on
<https://cran.r-project.org/web/checks/check_results_GGally.html>.

Please correct before 2021-06-22 to safely retain your package on CRAN.

Best,
-k



## Test environments and R CMD check results

* local macOS install 11.3.1
  * R 4.0
* GitHub Actions - https://github.com/ggobi/ggally/pull/419/checks
  * macOS, windows, ubuntu {16,20} - R release
  * macOS, windows, ubuntu {16,20} - R oldrelease

* win-builder
  * devel
  * release
  * oldrelease

#### R CMD check results

* 0 errors | 0 warnings | 0 notes


## revdepcheck results

We checked 125 reverse dependencies (100 from CRAN + 25 from BioConductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 2 packages

Issues with CRAN packages are summarised below.

### Failed to check

* egoTERGM    (NA)
  * Package is archived. I believe this is a false positive
* loon.ggplot (NA)
  * Could not install `loon`. The changes made for this release should have a negative effect on their code.
