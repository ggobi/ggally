
## Comments

### 2017-6-7

vortexR has been updated to handle the new version of GGally.  From my reverse dependency checks, there were no other affected packages.  <https://github.com/ggobi/ggally/blob/master/revdep/problem-diff.txt>

Best,
Barret


### 2017-6-7

On CRAN
- @carlopacioni vortexR maintainer


### 2017-6-4

vortexR maintainer contacted.


### 2017-6-4

~ Please give two weeks notice to affected packages.
- Uwe


### 2017-6-2

No revdep authors were emailed as it is only small bug fixes, url link fixes, and new functions.

Please let me know if there is anything I can do.  Thank you for your time.

Best,
Barret


#### Test environments and R CMD check results

* local OS X install
  * R version 3.4.0 (2017-04-21)
    Platform: x86_64-apple-darwin15.6.0 (64-bit)
    Running under: macOS Sierra 10.12.4
    * 0 errors | 0 warnings | 0 notes

* travis-ci
  * R version 3.4.0 (2017-04-21)
    Platform: x86_64-pc-linux-gnu (64-bit)
    Running under: Ubuntu precise (12.04.5 LTS)
    * 0 errors | 0 warnings | 0 notes

  * R Under development (unstable) (2017-05-24 r72734)
    Platform: x86_64-pc-linux-gnu (64-bit)
    Running under: Ubuntu precise (12.04.5 LTS)
    * 0 errors | 0 warnings | 0 notes

* win-builder (devel and release)
  * R version 3.4.0 (2017-04-21)
    * 0 errors | 0 warnings | 0 notes
  * R Under development (unstable) (2017-06-01 r72753)
    * 0 errors | 0 warnings | 0 notes


## Reverse dependencies

Checked on
  * R version 3.4.0 (2017-04-21)
    Platform: x86_64-apple-darwin15.6.0 (64-bit)
    Running under: macOS Sierra 10.12.4

No difference in test results due to GGally upgrade: <https://github.com/ggobi/ggally/blob/master/revdep/problem-diff.txt>


Errors not related to GGally upgrade:
* Failed to install dependencies for: MissingDataGUI, specmine
* ggbio: checking examples ... ERROR
* POUMM: checking re-building of vignette outputs ... WARNING
* TVTB: checking tests ... ERROR

* Check failed to finish in 20 minutes, so ignored
  * nzelect
