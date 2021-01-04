
## Comments

#### 2021-01-04

New features added. Time to share them to CRAN.

Please let me know if there is anything I can provide!

Thank you,
Barret


#### 2020-12-28

Sorry for the delay but would you please give me 2 more days to check the package?

- Mingmei

#### 2020-12-16

Reached out to {bootclust} maintainer Mingmei Tian <mingmeit@buffalo.edu> about not importing `sets::%>%` to avoid conflict with (possibly) many packages.

Waiting until >= 2020-12-29 to submit.



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
