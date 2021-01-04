# pdqr 0.3.0

* New function `region_distance()` computes distance between pair of regions.
* `summ_center()` gets new method "midrange" with corresponding new `summ_midrange()` function. It returns a mean of left and right edges of support.
* `summ_distance()` gets new method "avgdist". It computes average distance between sample values from inputs.
* New single-number option "pdqr.approx_discrete_n_grid" may be used to tweak degree of granularity of how continuous pdqr-function is approximated with discrete one during some complicated tasks (like in `summ_distance()` with method "avgdist").

# pdqr 0.2.1

* Outputs of `new_p()` and `new_q()` now work more accurately in case of dirac-like functions.
* `form_mix()` now uses new algorithm of discontinuity approximation, which enables proper handling of distributions with "touching" supports.
* New boolean option "pdqr.assert_args" may be used to turn off sanity checks of function arguments, which will somewhat increase general execution speed.
* Minor fixes to remove CRAN "additional issues".

# pdqr 0.2.0

* `form_estimate()` gets argument `estimate` renamed to `stat` to avoid possibly confusing word duplication.
* `form_retype()` gets default value `NULL` of `type`, which means taking opposite of input's type. This usage seems to be the most common.
* `form_retype()` gets new **default** method "value" that preserves ratio of d-function values at certain "x" points. It is more robust than previous options.
* `summ_spread()` gets new method "range" with corresponding new `summ_range()`
function. It returns a range length of "x" values within region of positive probability.
* New functions `form_recenter()` and `form_respread()` are implemented. They change center and spread of distribution (with linear transformation) to be equal to some desired value.

# pdqr 0.1.0

* Initial release.
