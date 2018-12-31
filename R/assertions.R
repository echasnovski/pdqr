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
#' @param allow_null If `TRUE` then error isn't thrown if `x` is `NULL`, no
#'   matter what `predicate(x)` returns.
#' @param ... Arguments to be passed to `predicate`.
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
assert_type <- function(x, predicate, type_name = NULL, allow_null = FALSE,
                        ...) {
  x_name <- deparse(substitute(x))
  if (is.null(type_name)) {
    predicate_name <- deparse(substitute(predicate))
    type_name <- parse_type(predicate_name)
  }

  is_pred_true <- (allow_null && is.null(x)) || isTRUE(predicate(x, ...))

  if (!is_pred_true) {
    # Not using "must be of type" because of 'tibble' and 'string' cases
    stop_collapse(
      "`", x_name, "` must be '", type_name, "', not '", get_type(x), "'."
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

  if (is.null(meta(f, "x_tbl"))) {
    stop_collapse(f_name, ' should have "x_tbl" metadata.')
  }

  assert_x_tbl(meta(f, "x_tbl"), type = meta(f, "type"))

  # Extra properties for "good" "x_tbl" metadata
  assert_x_tbl_meta(meta(f, "x_tbl"), type = meta(f, "type"))

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

assert_x_tbl <- function(x_tbl, type) {
  x_tbl_name <- paste0("`", deparse(substitute(x_tbl)), "`")

  if (!is.data.frame(x_tbl)) {
    stop_collapse(x_tbl_name, " should be a data frame.")
  }

  assert_num_col(x_tbl[["x"]], '"x"', x_tbl_name)

  switch(
    type,
    raw = assert_x_tbl_raw(x_tbl, x_tbl_name),
    smooth = assert_x_tbl_smooth(x_tbl, x_tbl_name)
  )
}

assert_x_tbl_raw <- function(x_tbl, x_tbl_name) {
  if (!("prob" %in% names(x_tbl))) {
    stop_collapse(x_tbl_name, ' should have "prob" column.')
  }

  prob <- x_tbl[["prob"]]
  assert_num_col(prob, '"prob"', x_tbl_name)

  if (any(prob < 0)) {
    stop_collapse(
      '"prob" column in ', x_tbl_name, " should not have negative values."
    )
  }
  if (sum(prob <= 0)) {
    stop_collapse('"prob" column in ', x_tbl_name, " should have positive sum.")
  }

  TRUE
}

assert_x_tbl_smooth <- function(x_tbl, x_tbl_name) {
  if (nrow(x_tbl) < 2) {
    stop_collapse(x_tbl_name, " should have at least 2 rows.")
  }

  assert_num_col(x_tbl[["y"]], '"y"', x_tbl_name)

  if (any(x_tbl[["y"]] < 0)) {
    stop_collapse(
      '"y" column in ', x_tbl_name, ' should not have negative values.'
    )
  }
  if (!any(x_tbl[["y"]] > 0)) {
    stop_collapse(
      '"y" column in ', x_tbl_name, ' should have at least one positive value.'
    )
  }

  TRUE
}

assert_x_tbl_meta <- function(x_tbl, type) {
  if (is.unsorted(x_tbl[["x"]])) {
    stop_collapse(
      '"x" column in "x_tbl" metadata should be sorted increasingly.'
    )
  }
  if (!("cumprob" %in% names(x_tbl))) {
    stop_collapse('"x_tbl" metadata should have "cumprob" column.')
  }

  if (type == "raw") {
    if (!("prob" %in% names(x_tbl))) {
      stop_collapse(
        '"x_tbl" metadata should have "prob" column if `type` is "raw".'
      )
    }
    if (!is_near(sum(x_tbl[["prob"]]), 1)) {
      stop_collapse(
        '"prob" column in "x_tbl" metadata should sum to 1.'
      )
    }
  }

  if (type == "smooth") {
    if (!is_near(trapez_integral(x_tbl[["x"]], x_tbl[["y"]]), 1)) {
      stop_collapse(
        'Total integral from "x_tbl" metadata columns should be 1 if ',
        '`type` is "smooth".'
      )
    }
  }

  TRUE
}

assert_num_col <- function(vec, col_name, x_tbl_name) {
  if (is.null(vec)) {
    stop_collapse(x_tbl_name, " should have column ", col_name, ".")
  }

  stop_start_chr <- paste0(col_name, " column in ", x_tbl_name)

  if (!is.numeric(vec)) {
    stop_collapse(stop_start_chr, " should be numeric.")
  }
  if (!all(is.finite(vec))) {
    stop_collapse(
      stop_start_chr, " should have only finite values and no `NA`s."
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
