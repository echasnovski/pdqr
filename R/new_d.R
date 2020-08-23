#' @rdname new-pdqr
#' @include new_p.R
#' @export
new_d <- function(x, type, ...) {
  distr_impl(
    pdqr_class = "d",
    impl_funs = list(discrete = new_d_dis, continuous = new_d_con),
    x = x, type = type, ...
  )
}

new_d_dis <- function(x_tbl) {
  type <- "discrete"
  support <- range(x_tbl[["x"]])

  function(x) {
    # Not using `assert_type()` for speed reasons
    if (!is.numeric(x)) {
      stop_collapse("`x` must be 'numeric', not '", get_type(x), "'.")
    }

    res <- numeric(length(x))

    x_not_na <- !is.na(x)
    res[!x_not_na] <- NA
    x <- x[x_not_na]

    x_ind <- match(round(x, digits = 10), x_tbl[["x"]], nomatch = NA)
    x_ind_matched <- !is.na(x_ind)
    res[x_not_na][x_ind_matched] <- x_tbl[["prob"]][x_ind[x_ind_matched]]

    res
  }
}

new_d_con <- function(x_tbl) {
  type <- "continuous"
  support <- range(x_tbl[["x"]])

  # Using custom function instead of `stats::approxfun()` to avoid creating
  # copies of `x_tbl[["x"]]` and `x_tbl[["y"]]`. It is slower but at acceptable
  # level.
  # For speed a better solution (which doesn't pass R CMD CHECK) would be:
  # function(v) {
  #   stats:::.approxfun(
  #     x = x_tbl[["x"]], y = x_tbl[["y"]], v = v,
  #     method = 1, yleft = 0, yright = 0, f = 0
  #   )
  # }
  function(x) {
    # Not using `assert_type()` for speed reasons
    if (!is.numeric(x)) {
      stop_collapse("`x` must be 'numeric', not '", get_type(x), "'.")
    }

    res <- numeric(length(x))

    x_not_na <- !is.na(x)
    res[!x_not_na] <- NA
    x <- x[x_not_na]

    x_vec <- x_tbl[["x"]]
    y_vec <- x_tbl[["y"]]

    is_inside <- (x >= support[1]) & (x <= support[2])

    # `rightmost.closed = TRUE` is to account for case `x` equals `support[2]`
    x_ind <- findInterval(x[is_inside], x_vec, rightmost.closed = TRUE)
    slopes <- (y_vec[x_ind + 1] - y_vec[x_ind]) /
      (x_vec[x_ind + 1] - x_vec[x_ind])
    res[x_not_na][is_inside] <- slopes * (x[is_inside] - x_vec[x_ind]) +
      y_vec[x_ind]

    res
  }
}

#' @rdname methods-print
#' @export
print.d <- function(x, ...) {
  x_type <- meta_type(x)

  if (!is.null(x_type) && (x_type == "discrete")) {
    pdqr_print(x, "Probability mass")
  } else {
    pdqr_print(x, "Density")
  }
}
