#' Summarize distribution with dispersion
#'
#' Functions to compute dispersion of distribution (i.e. "how wide it is
#' spread"). `summ_dispersion()` is a wrapper for respective `summ_*()`
#' functions (from this page) with default arguments.
#'
#' @inheritParams summ_center
#' @param method Method of center computation. Should be one of "sd", "var",
#'   "iqr", "mad".
#'
#' @details `summ_sd()` computes distribution's standard deviation.
#'
#' `summ_var()` computes distribution's variance.
#'
#' `summ_iqr()` computes distribution's interquartile range. Essentially, it is
#' a `as_q(f)(0.75) - as_q(f)(0.25)`.
#'
#' `summ_mad()` computes distribution's *median* absolute deviation around the
#' distribution's *median*.
#'
#' @return All functions return a single number representing a dispersion of
#'   distribution.
#'
#' @seealso [summ_center()] for computing distribution's center, [summ_moment()]
#'   for general moments.
#'
#' @examples
#' # Type "infin"
#' d_norm <- as_d(dnorm)
#'   # The same as `summ_dispersion(d_norm, method = "sd")`
#' summ_sd(d_norm)
#' summ_var(d_norm)
#' summ_iqr(d_norm)
#' summ_mad(d_norm)
#'
#' # Type "fin"
#' d_pois <- as_d(dpois, lambda = 10)
#' summ_sd(d_pois)
#' summ_var(d_pois)
#' summ_iqr(d_pois)
#' summ_mad(d_pois)
#'
#' @name summ_dispersion
NULL

#' @rdname summ_dispersion
#' @export
summ_dispersion <- function(f, method = "sd") {
  # `f` is validated inside `summ_*()` calls
  assert_type(method, is_string)
  assert_in_set(method, c("var", "sd", "iqr", "mad"))

  switch(
    method,
    var = summ_var(f),
    sd = summ_sd(f),
    iqr = summ_iqr(f),
    mad = summ_mad(f)
  )
}

#' @rdname summ_dispersion
#' @export
summ_sd <- function(f) {
  # `f` is validated inside `summ_var()`
  sqrt(summ_var(f))
}

#' @rdname summ_dispersion
#' @export
summ_var <- function(f) {
  # `f` is validated inside `summ_mean()`
  # `max(*, 0)` is used to take into account numerical representation accuracy
  max(-summ_mean(f)^2 + raw_moment(f, order = 2), 0)
}

#' @rdname summ_dispersion
#' @export
summ_iqr <- function(f) {
  assert_pdqr_fun(f)

  quarts <- as_q(f)(c(0.25, 0.75))

  quarts[2] - quarts[1]
}

#' @rdname summ_dispersion
#' @export
summ_mad <- function(f) {
  # `f` is validated inside `summ_median(f)`
  med <- summ_median(f)

  summ_median(abs(f - med))
}
