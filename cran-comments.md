
## Comments

#### 2025-08-18

The request has been fixed along with a couple other minor updates.

Please let me know if I can provide any other information.

Best,
Barret


#### 2025-07-22

Dear maintainer,

Please see the problems shown on
<https://cran.r-project.org/web/checks/check_results_GGally.html>.

Please correct before 2025-08-25 to safely retain your package on CRAN.

Do remember to look at any 'Additional issues'.

It seems we need to remind you of the CRAN policy:

'Packages which use Internet resources should fail gracefully with an informative message
if the resource is not available or has changed (and not give a check warning nor error).'

This needs correction whether or not the resource recovers.

The CRAN Team


For the record, the failure (on M1mac) is

 > ### Name: ggcorr
 > ### Title: Correlation matrix
 > ### Aliases: ggcorr
 >
 > ### ** Examples
 >
 > # Small function to display plots only if it's interactive
 > p_ <- GGally::print_if_interactive
 >
 > # Basketball statistics provided by Nathan Yau at Flowing Data.
 > dt <- read.csv("http://datasets.flowingdata.com/ppg2008.csv")

Warning in file(file, "rt") :
   URL 'http://datasets.flowingdata.com/ppg2008.csv': status was
'Couldn't resolve host name'
Error in file(file, "rt") :
   cannot open the connection to
'http://datasets.flowingdata.com/ppg2008.csv'
Calls: read.csv -> read.table -> file
Execution halted



## R CMD check results

* 0 errors | 0 warnings | 1 note

```
* checking CRAN incoming feasibility ... [16s] NOTE
Maintainer: 'Barret Schloerke <schloerke@gmail.com>'

Found the following (possibly) invalid URLs:
  URL: https://www.oecd.org/en/data/datasets/pisa-2012-cba-database.html
    From: man/australia_PISA2012.Rd
    Status: 403
    Message: Forbidden
```

When visiting the link manually, the link works as expected. I believe they have a bot blocker that is causing this error.

## revdepcheck results

We checked 142 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
