# pdqr 0.2.0

* `form_estimate()` gets argument `estimate` renamed to `stat` to avoid possibly confusing word duplication.
* `form_retype()` gets default value `NULL` of `type`, which means taking opposite of input's type. This usage seems to be the most common.
* `form_retype()` gets new **default** method "value" that preserves ratio of d-function values at certain "x" points. It is more robust than previous options.
* `summ_spread()` gets new method "range" with corresponding new `summ_range()`
function. It returns a range length of "x" values within region of positive probability.
* New functions `form_recenter()` and `form_respread()` are implemented. They change center and spread of distribution (with linear transformation) to be equal to some desired value.

# pdqr 0.1.0

* Initial release.
