
## Comments

### 2017-8-2

No revdep authors were emailed as it is only small bug fixes or 'Suggest' dependency change for Brian.

Please let me know if there is anything I can do.  Thank you for your time.

Best,
Barret


### 2017-7-18

As §1.1.3.1 of the manual told you, packages in Suggests: should be used
conditionally.  The latest version of igraph will not install on Solaris
(hence packages strictly depending on it), and as you can see from its
CRAN checks page, your package now fails its check.

Please correct ASAP and definitely within a month.

--
Brian D. Ripley,



#### Test environments and R CMD check results

* local OS X install
  * R version 3.4.1 (2017-06-30)
    Platform: x86_64-apple-darwin15.6.0 (64-bit)
    Running under: macOS Sierra 10.12.5
    * 0 errors | 0 warnings | 0 notes

* travis-ci
  * R version 3.4.1 (2017-06-30)
    Platform: x86_64-pc-linux-gnu (64-bit)
    Running under: Ubuntu precise (12.04.5 LTS)
    * 0 errors | 0 warnings | 0 notes

  * R Under development (unstable) (2017-08-01 r73010)
    Platform: x86_64-pc-linux-gnu (64-bit)
    Running under: Ubuntu precise (12.04.5 LTS)
    * 0 errors | 0 warnings | 0 notes

* win-builder (devel and release)
  * R version 3.4.1 (2017-06-30)
    * 0 errors | 0 warnings | 0 notes
  * R Under development (unstable) (2017-07-31 r73003)
    * 0 errors | 0 warnings | 0 notes


## Reverse dependencies

Checked on
  * R version 3.4.1 (2017-06-30)
    Platform: x86_64-apple-darwin15.6.0 (64-bit)
    Running under: macOS Sierra 10.12.5

No difference in test results due to GGally upgrade: <https://github.com/ggobi/ggally/blob/master/revdep/problem-diff.txt>


Errors not related to GGally upgrade:

|package        |version | errors| warnings| notes|
|:--------------|:-------|------:|--------:|-----:|
|eechidna       |1.1     |      0|        1|     1| - vignette building
|nzelect        |0.3.3   |      0|        1|     0| - vignette building
|PopGenReport   |3.0.0   |      0|        1|     1| - due to missing package dependencies
|POUMM          |1.3.2   |      1|        0|     0| - can not install source package
|MissingDataGUI |0.2-5   |      1|        0|     0| - can not install dependencies
|specmine       |1.0     |      1|        0|     0| - can not install dependencies

* Side note, package 'Pi' took over 4 hours to check.
