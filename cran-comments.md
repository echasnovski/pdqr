## Resubmission

This is a resubmission. As asked by Martina Schmirl in response to initial submission, in this version I have:

* Didn't add any "references describing the methods" in my package because I believe they are a combination of my unpublished work and conclusions from general mathematical knowledge, that was a result of internet search (such as "mean", "mode", "MAD", etc.). Adding links to basic definitions seems to be redundant.

* Removed usage of `set.seed()` inside functions.

* Replaced `\dontrun()` with `\donttest()` in examples, where it is reasonable. I replaced it in 'man/form_estimate.Rd', as the code is executable but takes around 6 seconds to run on my computer. I chose to not replace it in 'man/as-pdqr.Rd' as the code inside it gives an error, and "## Not run:" comment indicates that.

## Test environments

* Local Ubuntu 18.04 install, R 3.6.1
* Debian, development version (2019-11-02 r77358)
* macOS 10.11 on R-hub, R 3.6.0
* win-builder, release and development version (2019-11-02 r77358)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
