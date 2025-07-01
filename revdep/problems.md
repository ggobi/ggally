# comparer

<details>

* Version: 0.2.4
* GitHub: https://github.com/CollinErickson/comparer
* Source code: https://github.com/cran/comparer
* Date/Publication: 2024-10-02 22:50:03 UTC
* Number of recursive dependencies: 128

Run `revdepcheck::cloud_details(, "comparer")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(comparer)
      Loading required package: GauPro
      Loading required package: mixopt
      Loading required package: dplyr
      
      Attaching package: 'dplyr'
    ...
      Finished running superbatches 
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 426 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test_ffexp.R:35:5'): ffexp ────────────────────────────────────────
      `pp` inherits from `'ggmatrix'/'GGally::ggmatrix'/'S7_object'` not `'character'`.
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 426 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘DiceOptim’
    ```

# ezEDA

<details>

* Version: 0.1.1
* GitHub: https://github.com/kviswana/ezEDA
* Source code: https://github.com/cran/ezEDA
* Date/Publication: 2021-06-29 04:40:10 UTC
* Number of recursive dependencies: 76

Run `revdepcheck::cloud_details(, "ezEDA")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ezEDA)
      > 
      > test_check("ezEDA")
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 86 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
      y[1]: "gg"
      Backtrace:
          ▆
       1. └─testthat::expect_that(class(p)[[1]], equals("gg")) at test_multi_measures_relationship.R:4:3
       2.   └─testthat (local) condition(object)
       3.     └─testthat::expect_equal(x, expected, ..., expected.label = label)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 86 ]
      Error: Test failures
      Execution halted
    ```

# rbioacc

<details>

* Version: 1.2.1
* GitHub: NA
* Source code: https://github.com/cran/rbioacc
* Date/Publication: 2024-02-27 01:40:02 UTC
* Number of recursive dependencies: 96

Run `revdepcheck::cloud_details(, "rbioacc")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(rbioacc)
      > 
      > test_check("rbioacc")
      
      SAMPLING FOR MODEL 'TK' NOW (CHAIN 1).
      Chain 1: 
    ...
      `expected`: TRUE 
      ── Failure ('test-correlation.R:32:5'): corrPlot ───────────────────────────────
      all(class(plt_MGSG_sto) == c("gg", "ggmatrix")) is not TRUE
      
      `actual`:   FALSE
      `expected`: TRUE 
      
      [ FAIL 5 | WARN 85 | SKIP 9 | PASS 75 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 94.5Mb
      sub-directories of 1Mb or more:
        libs  93.8Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# tidycomm

<details>

* Version: 0.4.1
* GitHub: https://github.com/joon-e/tidycomm
* Source code: https://github.com/cran/tidycomm
* Date/Publication: 2024-02-22 12:20:02 UTC
* Number of recursive dependencies: 141

Run `revdepcheck::cloud_details(, "tidycomm")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(tidycomm)
      
      Attaching package: 'tidycomm'
      
      The following object is masked from 'package:testthat':
      
    ...
      ── Failure ('test-tdcmm_visualize.R:41:3'): implemented visualize() calls return ggplot2 (gg) ──
      visualize(correlate(WoJ, ethics_1, ethics_2, ethics_3)) inherits from 'ggmatrix'/'GGally::ggmatrix'/'S7_object' not 'gg'.
      ── Failure ('test-tdcmm_visualize.R:48:3'): implemented visualize() calls return ggplot2 (gg) ──
      visualize(...) inherits from 'ggmatrix'/'GGally::ggmatrix'/'S7_object' not 'gg'.
      ── Failure ('test-tdcmm_visualize.R:59:3'): implemented visualize() calls return ggplot2 (gg) ──
      `v` inherits from 'ggmatrix'/'GGally::ggmatrix'/'S7_object' not 'gg'.
      
      [ FAIL 3 | WARN 0 | SKIP 0 | PASS 388 ]
      Error: Test failures
      Execution halted
    ```

