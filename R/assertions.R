# Assert object type ------------------------------------------------------
#' Assert object type
#'
#' Throw an error in case object is not of desired type.
#'
#' @param x An object to check.
#' @param predicate A function to perform check. A good idea is to use function
#'   named `is.*()` or `is_*()` with possible `<package>::` prefix.
#' @param type_name A string for desired type name. If `NULL`, type is taken
#'   from parsing original name of supplied `predicate`: all alphanumeric with
#'   '_' and '.' characters (until the name end) after the first appearance of
#'   either `is.` or `is_`. In case of a doubt supply `type` explicitly.
#' @param x_name String to be used as variable name instead of supplied one
#'   (default).
#' @param allow_null If `TRUE` then error isn't thrown if `x` is `NULL`, no
#'   matter what `predicate(x)` returns.
#' @param ... Arguments to be passed to `predicate`.
#'
#' @examples
#' \donttest{
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
assert_type <- function(x, predicate, type_name = NULL, x_name = NULL,
                        allow_null = FALSE, ...) {
  if (dont_assert()) {
    return(TRUE)
  }

  if (is.null(x_name)) {
    x_name <- deparse(substitute(x))
  }
  x_name <- enbacktick(x_name)

  if (is.null(type_name)) {
    predicate_name <- deparse(substitute(predicate))
    type_name <- parse_type(predicate_name)
  }

  is_pred_true <- (allow_null && is.null(x)) || isTRUE(predicate(x, ...))

  if (!is_pred_true) {
    # Not using "must be of type" because of 'tibble' and 'string' cases
    stop_collapse(
      x_name, " must be '", type_name, "', not '", get_type(x), "'."
    )
  }

  TRUE
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


# Assert object being in set ----------------------------------------------
assert_in_set <- function(x, set, x_name = NULL, quote_set = TRUE,
                          allow_null = FALSE) {
  if (dont_assert()) {
    return(TRUE)
  }

  if (is.null(x_name)) {
    x_name <- deparse(substitute(x))
  }
  x_name <- enbacktick(x_name)

  if (!is_in_set(x, set, allow_null = allow_null)) {
    # Create string for set enumeration
    if (quote_set) {
      set_str <- paste0('"', set, '"', collapse = ", ")
    } else {
      set_str <- paste0(set, collapse = ", ")
    }

    # Create string for input `x`
    if (is.null(x)) {
      x_str <- "NULL"
    } else if (quote_set) {
      x_str <- paste0('"', x, '"')
    } else {
      x_str <- as.character(x)
    }

    # Create suggestion string
    if (is.character(set) && is.character(x)) {
      suggested_elem <- match_in_set(x, set)
      suggestion_str <- paste0(' Did you mean "', suggested_elem, '"?')
    } else {
      suggestion_str <- ""
    }

    stop_collapse(
      x_name, " should be one of: ", set_str, " (instead of ", x_str, ").",
      suggestion_str
    )
  }

  TRUE
}

is_in_set <- function(x, set, allow_null = FALSE) {
  if (is.null(x)) {
    return(allow_null)
  }

  x %in% set
}

match_in_set <- function(x, set) {
  x_low <- tolower(x)
  set_low <- tolower(set)
  x_pmatch <- pmatch(x = x_low, table = set_low)

  if (!is.na(x_pmatch)[1]) {
    # Try partial match first
    set[x_pmatch]
  } else {
    # If there is no partial match, return the closest set element
    string_dist <- utils::adist(x_low, set_low)[1, ]

    set[which.min(string_dist)]
  }
}


# Assert missing arguments ------------------------------------------------
assert_missing <- function(x, value_name) {
  if (dont_assert()) {
    return(TRUE)
  }

  if (missing(x)) {
    x_name <- enbacktick(deparse(substitute(x)))

    error_missing(var_name = x_name, value_name = value_name)
  }

  TRUE
}

error_missing <- function(var_name, value_name) {
  stop_collapse("Argument ", var_name, " is missing. Supply ", value_name, ".")
}


# Assertions for pdqr-functions -------------------------------------------
assert_pdqr_fun <- function(f, f_name = NULL) {
  if (dont_assert()) {
    return(TRUE)
  }

  if (is.null(f_name)) {
    f_name <- deparse(substitute(f))
  }
  f_name <- enbacktick(f_name)

  if (missing(f)) {
    error_missing(var_name = f_name, value_name = "pdqr-function")
  }

  err_header <- paste0(f_name, " is not pdqr-function. ")

  if (!is.function(f)) {
    stop_collapse(err_header, "It should be function.")
  }

  if (!inherits(f, "pdqr")) {
    stop_collapse(err_header, 'It should inherit from "pdqr".')
  }
  if (!inherits(f, c("p", "d", "q", "r"))) {
    stop_collapse(
      err_header, 'It should inherit from one of classes: "p", "d", "q", "r".'
    )
  }

  if (!has_meta_type(f)) {
    stop_collapse(
      err_header, 'It should have proper "type" metadata ("discrete" or ',
      '"continuous").'
    )
  }

  if (!has_meta_support(f)) {
    stop_collapse(
      err_header, 'It should have proper "support" metadata (numeric vector ',
      'of length 2 with non-decreasing finite elements).'
    )
  }

  if (is.null(meta_x_tbl(f))) {
    stop_collapse(err_header, 'It should have "x_tbl" metadata.')
  }

  assert_x_tbl(meta_x_tbl(f), type = meta_type(f), err_header = err_header)

  # Extra properties for "good" "x_tbl" metadata
  assert_x_tbl_meta(meta_x_tbl(f), type = meta_type(f), err_header = err_header)

  TRUE
}

assert_pdqr_type <- function(type, allow_null = FALSE) {
  if (dont_assert()) {
    return(TRUE)
  }

  type_name <- deparse(substitute(type))

  assert_type(type, is_string, x_name = type_name, allow_null = allow_null)
  assert_in_set(
    type, c("discrete", "continuous"), x_name = type_name,
    allow_null = allow_null
  )

  TRUE
}

assert_support <- function(support, allow_na = FALSE) {
  if (dont_assert()) {
    return(TRUE)
  }

  support_name <- enbacktick(deparse(substitute(support)))

  if (!(is.numeric(support) && (length(support) == 2))) {
    stop_collapse(
      support_name, " should be 'numeric with length 2', not '",
      get_type(support), "'."
    )
  }
  if (!allow_na && anyNA(support)) {
    stop_collapse(support_name, " should not have missing values.")
  }
  if (!anyNA(support) && (support[1] > support[2])) {
    stop_collapse(
      "First value in ", support_name, " should be not bigger than second one."
    )
  }
  if (any(is.infinite(support))) {
    stop_collapse(support_name, " should have only finite elements.")
  }

  TRUE
}

assert_x_tbl <- function(x_tbl, type, err_header = "") {
  if (dont_assert()) {
    return(TRUE)
  }

  x_tbl_name <- enbacktick(deparse(substitute(x_tbl)))

  if (!is.data.frame(x_tbl)) {
    stop_collapse(err_header, x_tbl_name, " should be a data frame.")
  }

  assert_num_col(x_tbl[["x"]], '"x"', x_tbl_name, err_header = err_header)

  switch(
    type,
    discrete = assert_x_tbl_dis(x_tbl, x_tbl_name, err_header = err_header),
    continuous = assert_x_tbl_con(x_tbl, x_tbl_name, err_header = err_header)
  )
}

assert_x_tbl_dis <- function(x_tbl, x_tbl_name, err_header = "") {
  # There is no checking of `dont_assert()` because this is a helper function

  if (!("prob" %in% names(x_tbl))) {
    stop_collapse(err_header, x_tbl_name, ' should have "prob" column.')
  }

  prob <- x_tbl[["prob"]]
  assert_num_col(prob, '"prob"', x_tbl_name, err_header = err_header)

  if (any(prob < 0)) {
    stop_collapse(
      err_header, '"prob" column in ', x_tbl_name, " should not have negative ",
      "values."
    )
  }
  if (sum(prob) <= 0) {
    stop_collapse(
      err_header, '"prob" column in ', x_tbl_name, " should have positive sum."
    )
  }

  TRUE
}

assert_x_tbl_con <- function(x_tbl, x_tbl_name, err_header = "") {
  # There is no checking of `dont_assert()` because this is a helper function

  if (nrow(x_tbl) < 2) {
    stop_collapse(err_header, x_tbl_name, " should have at least 2 rows.")
  }

  if (anyDuplicated(x_tbl[["x"]]) != 0) {
    stop_collapse(
      err_header, '"x" column in ', x_tbl_name, ' should not have duplicate ",
      "values.'
    )
  }

  assert_num_col(x_tbl[["y"]], '"y"', x_tbl_name, err_header = err_header)

  if (any(x_tbl[["y"]] < 0)) {
    stop_collapse(
      err_header, '"y" column in ', x_tbl_name, ' should not have negative ",
      "values.'
    )
  }
  if (!any(x_tbl[["y"]] > 0)) {
    stop_collapse(
      err_header, '"y" column in ', x_tbl_name, ' should have at least one ",
      "positive value.'
    )
  }

  TRUE
}

assert_x_tbl_meta <- function(x_tbl, type, err_header = "") {
  # There is no checking of `dont_assert()` because this is a helper function

  if (is.unsorted(x_tbl[["x"]])) {
    stop_collapse(
      err_header, '"x" column in "x_tbl" metadata should be sorted ",
      "increasingly.'
    )
  }
  if (!("cumprob" %in% names(x_tbl))) {
    stop_collapse(err_header, '"x_tbl" metadata should have "cumprob" column.')
  }

  if (type == "discrete") {
    if (!("prob" %in% names(x_tbl))) {
      stop_collapse(
        err_header, '"x_tbl" metadata should have "prob" column if `type` is ',
        '"discrete".'
      )
    }
    if (!is_near(sum(x_tbl[["prob"]]), 1)) {
      stop_collapse(
        err_header, '"prob" column in "x_tbl" metadata should sum to 1.'
      )
    }
    if (anyDuplicated(x_tbl[["x"]]) != 0) {
      stop_collapse(
        err_header, '"x" column in "x_tbl" metadata should not have duplicate ',
        'values.'
      )
    }
  }

  if (type == "continuous") {
    if (!is_near(trapez_integral(x_tbl[["x"]], x_tbl[["y"]]), 1)) {
      stop_collapse(
        err_header, 'Total integral from "x_tbl" metadata columns should be 1 ',
        'if `type` is "continuous".'
      )
    }
  }

  TRUE
}

assert_num_col <- function(vec, col_name, x_tbl_name, err_header = "") {
  # There is no checking of `dont_assert()` because this is a helper function

  if (is.null(vec)) {
    stop_collapse(err_header, x_tbl_name, " should have column ", col_name, ".")
  }

  stop_start_chr <- paste0(col_name, " column in ", x_tbl_name)

  if (!is.numeric(vec)) {
    stop_collapse(err_header, stop_start_chr, " should be numeric.")
  }
  if (!all(is.finite(vec))) {
    stop_collapse(
      err_header, stop_start_chr, " should have only finite values and no ",
      "`NA`s."
    )
  }

  TRUE
}


# Warnings for pdqr-functions ---------------------------------------------
warning_boolean_pdqr_fun <- function(f = NULL, f_name = NULL) {
  if (is.null(f_name)) {
    f_name <- enbacktick(deparse(substitute(f)))
  }

  warning_collapse(
    f_name, ' is not a "boolean" pdqr-function (type "discrete" with "x" ',
    'values equal to 0 and 1). Proceed with caution.'
  )
}


# Assertion options -------------------------------------------------------
dont_assert <- function() {
  !isTRUE(getOption("pdqr.assert_args"))
}
