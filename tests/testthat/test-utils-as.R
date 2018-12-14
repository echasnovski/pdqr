context("test-utils-as-distr")


# distr_from_meta ---------------------------------------------------------
# Tested in tests for `as_*()` (about working with present "x" in metadata)


# as_distr_impl_def -------------------------------------------------------
# Tested in tests for construction of `*_fun` from user-defined function


# as_distr_impl_r ---------------------------------------------------------
# Tested in tests for conversion to `*_fun` from `r_fun`


# warn_conversion_from_p_raw ----------------------------------------------
# Tested in `as_q()` and `as_r()`


# detect_support_raw ------------------------------------------------------
# Tested in several places in case `type` = "raw":
# - `as_p()` tests for converting from 'd_fun'.
# - `as_d()` default method.
