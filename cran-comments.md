
## Comments

### 2017-5-17

Altered test and added crosstalk as a suggests.  Passes on travis, local mac, and against plotly (reason for test).

- Barret


### 2017-5-17

$GGally
'::' or ':::' import not declared from: ‘crosstalk’
'library' or 'require' call not declared from: ‘crosstalk’

- Kurt

### 2017-5-17

Please let me know if there is anything else I can do!

Thank you for your time.

- Barret


#### Test environments and R CMD check results

* local OS X install
  * R version 3.5.0 (2018-04-23)
    Platform:  system   x86_64, darwin15.6.0
    Running under: macOS High ierra 10.13.4
    * 0 errors | 0 warnings | 0 notes

* travis-ci
  * Known travis issue of setting a _JAVA_OPTIONS value.  **These _JAVA_OPTIONS notes are false positives**

  * R version 3.5.0 (2017-01-27)
    Platform: x86_64-pc-linux-gnu (64-bit)
    Running under: Ubuntu 14.04.5 LTS
    * 0 errors | 0 warnings | 1 note
    * checking dependencies in R code ... NOTE
      Picked up _JAVA_OPTIONS: -Xmx2048m -Xms512m

  * R Under development (unstable) (2018-05-16 r74730)
    Platform: x86_64-pc-linux-gnu (64-bit)
    Running under: Ubuntu 14.04.5 LTS
    * 0 errors | 0 warnings | 1 note
    * checking dependencies in R code ... NOTE
      Picked up _JAVA_OPTIONS: -Xmx2048m -Xms512m

* rhub
  * fedora-clang-devel
    * http://builder.r-hub.io/status/GGally_1.3.3.tar.gz-9e96854baee648a5a65df453919ea45f
    * 0 errors | 0 warnings | 1 note
    * checking package dependencies ... NOTE
      Package suggested but not available for checking: ‘scagnostics’


* win-builder (devel and release)
  * R version 3.5.0 (2018-04-23)
    * 0 errors | 0 warnings | 0 notes
  * R Under development (unstable) (2018-05-15 r74727)
    * 0 errors | 0 warnings | 0 notes


## Reverse dependencies

### Checked on
|field    |value                        |
|:--------|:----------------------------|
|version  |R version 3.5.0 (2018-04-23) |
|os       |macOS High Sierra 10.13.4    |
|system   |x86_64, darwin15.6.0         |

No difference in test results from GGally upgrade: https://github.com/ggobi/ggally/blob/master/revdep/

### Couldn't check (1)

|package                    |version |error |warning |note |
|:--------------------------|:-------|:-----|:-------|:----|
|[LANDD](problems.md#landd) |1.1.0   |1     |        |     |


### Revdep Maintainers

Reverse dependency maintainers were not notified as no breaking changes occured.
