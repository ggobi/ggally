## Comments

#### 2026-11-06

This release adds support for the upcoming S7 release, which is planned for CRAN on 2026-11-16 (https://github.com/RConsortium/S7/issues/786).

It also fixes the r-devel NOTE about Rd files without `\usage` (`print.ggmatrix.Rd` and `str.ggmatrix.Rd`).

Please let me know if I can provide any other information.

Best,
Barret


## R CMD check results

* 0 errors | 0 warnings | 0 notes

## revdepcheck results

We checked 184 reverse dependencies (154 from CRAN + 30 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 7 packages

Issues with CRAN packages are summarised below.

### Failed to check

* airGR       (NA)
* BAS         (NA)
* fingerPro   (NA)
* jarbes      (NA)
* loon.ggplot (NA)
* morseDR     (NA)
* simmr       (NA)

All 7 failed to install on my machine with both the CRAN and dev versions of GGally, due to local system setup (compiler flags, JAGS, Tcl/Tk). They are not caused by this release.
