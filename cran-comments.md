
## Comments
#### 2021-03-07

Have addressed the missing `emmeans` package error.

Have also included other small bug fixes. No new features / functions.

Thank you,
Barret

#### 2021-02-24

Dear maintainer,

Please see the problems shown on
<https://cran.r-project.org/web/checks/check_results_GGally.html>.

Please correct before 2021-03-10 to safely retain your package on CRAN.

Best,
-k



## Test environments and R CMD check results

* local macOS install 10.15.7
  * R 4.0
* GitHub Actions - https://github.com/ggobi/ggally/pull/397/checks
  * macOS, windows - R devel
  * macOS, windows, ubuntu 16 - R 4.0
  * macOS, windows, ubuntu 16 - R 3.6

* win-builder
  * oldrelease
  * release
  * devel

#### R CMD check results

* 0 errors | 0 warnings | 0 notes


## revdepcheck results

We checked 113 reverse dependencies (88 from CRAN + 25 from BioConductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 1 new problems
 * We failed to check 1 packages

Issues with CRAN packages are summarised below.

### New problems
(This reports the first line of each new failure)

* bootcluster
  checking whether package ‘bootcluster’ can be installed ... WARNING

  - Maintainer (Mingmei Tian <mingmeit@buffalo.edu>) was contacted on Dec 16th about an upcoming release of GGally.
    * A possible patch was provided over email.
    * Mingmei replied to the email and asked for time until Jan 1st.
    * I have not heard an update since.

### Failed to check

* loon.ggplot (NA)
