
## Comments

### 2020-3-19

I've fixed the tests and the package now works with the latest (and prior) ggplot2 versions.

Please let me know if there is anything else I can do.

Best,
Barret


### 2020-3-6

Dear maintainer,

Please see the problems shown on
<https://cran.r-project.org/web/checks/check_results_GGally.html>.

Please correct before 2020-03-20 to safely retain your package on CRAN.

Best,
-k


#### Test environments and R CMD check results

* local macOS install 10.15.3
  * R 3.6.3
* travis-ci ubuntu
  * oldrelease, R version 3.5.3 (2017-01-27)
  * release, R version 3.6.2 (2017-01-27)
  * devel, R Under development (unstable) (2020-03-13 r77948)

* rhub
  * Windows Server 2008 R2 SP1, R-devel, 32/64 bit
  * Ubuntu Linux 16.04 LTS, R-release, GCC
  * Fedora Linux, R-devel, clang, gfortran

* win-builder
  * oldrelease
  * release
  * devel



#### R CMD check results

rhub - Fedora R-devel
* 0 errors | 0 warnings | 1 note
* checking package dependencies ... NOTE
  Package suggested but not available for checking: ‘scagnostics’

Everything else
* 0 errors | 0 warnings | 0 notes



## revdepcheck results

We checked 89 reverse dependencies (72 from CRAN + 17 from BioConductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
