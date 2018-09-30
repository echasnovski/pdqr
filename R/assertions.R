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
#'
#' @examples
#' \dontrun{
#' x <- 1
#' assert_type(x, is.numeric)
#' assert_type(x, is.logical)
#' assert_type(x, rlang::is_string, "character of length 1")
#' }
#'
#' @keywords internal
#' @noRd
assert_type <- function(x, predicate, type = NULL) {
  x_name <- deparse(rlang::enexpr(x))
  if (is.null(type)) {
    predicate_name <- deparse(rlang::enexpr(predicate))
    type <- parse_type(predicate_name)
  }

  if (!isTRUE(predicate(x))) {
    # Not using "must be of type" because of 'tibble' and 'string' cases
    stop_glue("`{x_name}` must be '{type}', not '{get_type(x)}'.")
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


# Targeted assertions -----------------------------------------------------
assert_common_args <- function(x, type, attach_sample) {
  assert_type(x, is.numeric)

  assert_type(type, rlang::is_string)
  if (!(type %in% c("raw", "smooth"))) {
    stop_glue('`type` should be one of "raw" or "smooth", not {type}.')
  }

  assert_type(
    attach_sample, function(x) {identical(x, TRUE) || identical(x, FALSE)},
    "`TRUE` or `FALSE`"
  )

  x
}
