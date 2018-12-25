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


# Assert missing arguments ------------------------------------------------
assert_missing_args <- function(f_name, ...) {
  dots <- list(...)
  missing_args <- names(Filter(isTRUE, dots))

  if (length(missing_args) > 0) {
    stop_collapse(
      'To define ', f_name, '-function supply the following arguments: ',
      paste0('`', missing_args, '`', collapse = ", "), '.'
    )
  }

  TRUE
}


# Assertions for "pdqr" functions -----------------------------------------
assert_pdqr_fun <- function(f) {
  f_name <- paste0("`", deparse(substitute(f)), "`")

  if (!is.function(f)) {
    stop_collapse(f_name, " should be function.")
  }

  if (!inherits(f, "pdqr")) {
    stop_collapse(f_name, ' should inherit from "pdqr".')
  }
  if (!inherits(f, c("p", "d", "q", "r"))) {
    stop_collapse(
      f_name, ' should inherit from one of classes: "p", "d", "q", "r".'
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

  if (!("x_tbl" %in% names(meta(f)))) {
    stop_collapse(f_name, ' should have "x_tbl" metadata.')
  }

  if (is.null(meta(f, "x_tbl"))) {
    if (meta(f, "type") == "raw") {
      stop_collapse(
        f_name, ' should have non-`NULL` "x_tbl" metadata if `type` is "raw".'
      )
    }
  } else {
    assert_x_tbl(meta(f, "x_tbl"), type = meta(f, "type"))
  }

  TRUE
}

assert_distr_type <- function(type) {
  type_name <- paste0("`", deparse(substitute(type)), "`")

  if (!is_string(type)) {
    stop_collapse(type_name, " must be 'string', not '", get_type(type), "' .")
  }
  if (!(type %in% c("raw", "smooth"))) {
    stop_collapse(
      type_name, ' should be one of "raw" or "smooth", not "', type, '".'
    )
  }

  TRUE
}

assert_support <- function(support) {
  support_name <- paste0("`", deparse(substitute(support)), "`")

  if (!(is.numeric(support) && (length(support) == 2))) {
    stop_collapse(
      support_name, " should be 'numeric with length 2', not '",
      get_type(support), "'."
    )
  }
  if (anyNA(support)) {
    stop_collapse(support_name, " should not have missing values.")
  }
  if (support[1] > support[2]) {
    stop_collapse(
      "First value in ", support_name, " should be not bigger than second one."
    )
  }
  if (any(is.infinite(support))) {
    stop_collapse(support_name, " should have only finite elements.")
  }

  TRUE
}

assert_x_tbl <- function(x, type) {
  x_name <- paste0("`", deparse(substitute(x)), "`")

  switch(
    type,
    raw = assert_x_tbl_raw(x, x_name),
    smooth = assert_x_tbl_smooth(x, x_name)
  )
}

assert_x_tbl_raw <- function(x, x_name) {
  if (!is.data.frame(x)) {
    stop_collapse(x_name, " should be a data frame.")
  }
  if (!(("x" %in% names(x)) && is.numeric(x[["x"]]))) {
    stop_collapse(x_name, ' should have numeric column "x".')
  }
  if (!any(c("prob", "n") %in% names(x))) {
    stop_collapse(x_name, ' should have one of "prob" or "n" columns.')
  }

  # If "n" is present then it should satisfy certain conditions.
  # If "n" is not present then "prob" should satisfy those conditions.
  if ("n" %in% names(x)) {
    assert_probish(x[["n"]], '"n"', x_name)
  } else {
    assert_probish(x[["prob"]], '"prob"', x_name)
  }

  TRUE
}

assert_probish <- function(vec, col_name, x_name) {
  stop_start_chr <- paste0(col_name, " column in ", x_name)

  if (!is.numeric(vec)) {
    stop_collapse(stop_start_chr, " should be numeric.")
  }
  if (any(vec < 0)) {
    stop_collapse(stop_start_chr, " should not have negative values.")
  }
  if (sum(vec) <= 0) {
    stop_collapse(stop_start_chr, " should have positive sum.")
  }

  TRUE
}

assert_x_tbl_smooth <- function(x, x_name) {
  if (!is.data.frame(x)) {
    stop_collapse(x_name, " should be a data frame.")
  }
  if (nrow(x) < 2) {
    stop_collapse(x_name, " should have at least 2 rows.")
  }
  if (!(("x" %in% names(x)) && is.numeric(x[["x"]]))) {
    stop_collapse(x_name, ' should have numeric column "x".')
  }
  if (!(("y" %in% names(x)) && is.numeric(x[["y"]]))) {
    stop_collapse(x_name, ' should have numeric column "y".')
  }
  if (any(x[["y"]] < 0)) {
    stop_collapse('"y" column in ', x_name, ' should not have negative values.')
  }
  if (!any(x[["y"]] > 0)) {
    stop_collapse(
      '"y" column in ', x_name, ' should have at least one positive value.'
    )
  }

  TRUE
}

assert_tot_prob <- function(tot_prob) {
  if (is_near(tot_prob, 0)) {
    stop_collapse("Total probability on supplied `support` is zero.")
  }

  TRUE
}
