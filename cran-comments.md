## Resubmission

This is a resubmission which addresses "skipping" tests on noLD platform.

Original submission goal: maintenance update to address CRAN's and R-devel issues.

## Test environments

* Ubuntu 18.04 (local install), R-release (4.0.3) both default and with setting environment variable `_R_CHECK_DEPENDS_ONLY_` to `true`
* Ubuntu 16.04 (via Github Actions), R-oldrel (3.6.3)
* macOS 10.13.6 (via R-hub), R-release (4.0.3)
* Debian (via R-hub), R-devel (2021-01-09 r79815)
* Debian "noLD" version (via R-hub "debian-gcc-devel-nold"), R-devel (2021-01-09 r79815)
* win-builder, R-release (4.0.3) and R-devel (2021-01-13 r79826)

## R CMD check results

0 errors | 0 warnings | 0 notes
