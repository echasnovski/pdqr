# Assert object type ------------------------------------------------------
#' Assert object type
#'
#' Throw an error in case object is not of desired type.
#'
#' @param x An object to check.
#' @param predicate A function to perform check. A good idea is to use function
#'   named `is.*()` or `is_*()` with possible `<package>::` prefix.
#' @param type A string for desired type. If `NULL`, type is taken from parsing
#'   original name of supplied `predicate`: all alphanumeric with '_' and '.'
#'   characters (until the name end) after the first appearance of either `is.`
#'   or `is_`. In case of a doubt supply `type` explicitly.
#' @param allow_null If `TRUE` then error isn't thrown if `x` is `NULL`, no
#'   matter what `predicate(x)` returns.
#'
#' @examples
#' \dontrun{
#' x <- 1
#' assert_type(x, is.numeric)
#' assert_type(x, is.logical)
#' assert_type(
#'   x,
#'   function(x) {is.character(x) && (length(x) == 1)},
#'   "character of length 1"
#' )
#' }
#'
#' @keywords internal
#' @noRd
assert_type <- function(x, predicate, type = NULL, allow_null = FALSE) {
  x_name <- deparse(substitute(x))
  if (is.null(type)) {
    predicate_name <- deparse(substitute(predicate))
    type <- parse_type(predicate_name)
  }

  is_pred_true <- (allow_null && is.null(x)) || isTRUE(predicate(x))

  if (!is_pred_true) {
    # Not using "must be of type" because of 'tibble' and 'string' cases
    stop_collapse(
      "`", x_name, "` must be '", type, "', not '", get_type(x), "'."
    )
  }

  x
}

# This function is needed because `typeof()` on data frame returns "list"
get_type <- function(x) {
  if (is.data.frame(x)) {
    return("data.frame")
  }

  typeof(x)
}

parse_type <- function(f_name) {
  regmatches(
    f_name,
    regexec("is[_\\.]([[:alnum:]_\\.]+)$", f_name)
  )[[1]][2]
}


# Assertion for "pdqr" function -------------------------------------------
assert_pdqr_fun <- function(f) {
  f_name <- paste0("`", deparse(substitute(f)), "`")

  if (!is.function(f)) {
    stop_collapse(f_name, " should be function.")
  }
  if (!inherits(f, "pdqr_fun")) {
    stop_collapse(f_name, ' should inherit from "pdqr_fun".')
  }
  if (!inherits(f, c("p_fun", "d_fun", "q_fun", "r_fun"))) {
    stop_collapse(
      f_name, ' should inherit from one of classes: ',
      '"p_fun", "d_fun", "q_fun", "r_fun".'
    )
  }
  if (!has_meta_type(f)) {
    stop_collapse(
      f_name, ' should have proper "type" metadata ("raw" or "smooth").'
    )
  }
  if (!has_meta_support(f)) {
    stop_collapse(
      f_name, ' should have proper "support" metadata (numeric vector ',
      'of length 2 with non-decreasing finite elements).'
    )
  }
  if (has_meta(f, "x") && !is.numeric(meta(f, "x"))) {
    stop_collapse('"x" metadata in ', f_name, ' should be numeric.')
  }

  f
}
