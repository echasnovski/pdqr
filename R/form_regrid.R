#' Change grid of pdqr-function
#'
#' Modify grid of pdqr-function (rows of ["x_tbl" metadata][meta_x_tbl()]) to
#' increase (upgrid) or decrease (downgrid) granularity using method of choice.
#' Upgridding might be useful in order to obtain more information during certain
#' type of transformations. Downgridding might be useful for decreasing amount
#' of used memory for storing pdqr-function without losing much information.
#'
#' @param f A pdqr-function.
#' @param n_grid A desired number of grid elements in output.
#' @param method Regrid method. Should be one of "x" or "q".
#'
#' @details The goal here is to create pdqr-function which is reasonably similar
#'   to `f` and has `n_grid` rows in "x_tbl" metadata.
#'
#' General algorithm of regridding is as follows:
#' - **Compute reference grid**. For method "x" it is a sequence of equidistant
#' points between edges of `f`'s [support][meta_support()]. For method "q" -
#' sequence of quantiles for equidistant probabilities from 0 to 1. Lengths of
#' reference grids for both methods are `n_grid`.
#' - **Adjust `f`'s grid to reference one**. This is done depending on `f`'s
#' [type][meta_type()] and which kind or regridding is done (upgridding is the
#' case when `n_grid` is strictly more than number of rows in "x_tbl" metadata,
#' downgridding - when it is strictly less):
#'     - Type "discrete":
#'         - UPgridding "discrete" functions is not possible as it is assumed
#'         that input "discrete" functions can't have any "x" values other then
#'         present ones. In this case input is returned, the only case when
#'         output doesn't have desired `n_grid` rows in "x_tbl" metadata.
#'         - DOWNgridding "discrete" functions is done by computing nearest
#'         match of reference grid to `f`'s one and collapsing (by summing
#'         probabilities) all "x" values from input to the nearest matched ones.
#'         Here "computing nearest match" means that every element of reference
#'         grid is one-one matched with subset of unique values from `f`'s "x"
#'         elements. Matching is done in greedy iterative fashion in order to
#'         minimize total distance between reference grid and matched subset.
#'         **Note** that this can result in not optimal (with not minimum total
#'         distance) match and can take a while to compute in some cases.
#'     - Type "continuous":
#'         - UPgridding "continuous" functions is done by adding rows to "x_tbl"
#'         metadata with "x" values equal to those elements of reference grid
#'         which are the furthest away from input "x" grid as a set. Distance
#'         from point to set is meant as minimum of distances between point and
#'         all points of set. Values of "y" and "cumprob" columns are taken as
#'         values of corresponding to `f` d- and p-functions.
#'         - DOWNgridding "continuous" functions is done by computing nearest
#'         match of reference grid to `f`'s one (as for "discrete" type) and
#'         removing all unmatched rows from "x_tbl" metadata.
#'
#' Special cases of `n_grid`:
#' - If `n_grid` is the same as number of rows in "x_tbl" metadata, then input
#' `f` is returned.
#' - If `n_grid` is 1, appropriate `new_*()` function is used with single
#' numeric input equal to distribution's median.
#'
#' @return A pdqr-function with modified grid.
#'
#' @seealso [form_resupport()] for changing support of pdqr-function.
#'
#' [form_retype()] for changing type of pdqr-function.
#'
#' @family form functions
#'
#' @examples
#' # Type "discrete"
#' d_dis <- new_d(data.frame(x = 1:10, prob = 1:10 / 55), type = "discrete")
#'
#' # Downgridding
#' meta_x_tbl(form_regrid(d_dis, n_grid = 4))
#' meta_x_tbl(form_regrid(d_dis, n_grid = 4, method = "q"))
#'
#' # Upgridding for "discrete" type isn't possible. Input is returned
#' identical(d_dis, form_regrid(d_dis, n_grid = 100))
#'
#' # Type "continuous"
#' # Downgridding
#' d_norm <- as_d(dnorm)
#' plot(d_norm)
#' lines(form_regrid(d_norm, n_grid = 10), col = "blue")
#' lines(form_regrid(d_norm, n_grid = 10, method = "q"), col = "green")
#'
#' # Upgridding
#' d_con <- new_d(data.frame(x = 1:3, y = rep(0.5, 3)), type = "continuous")
#' meta_x_tbl(form_regrid(d_con, n_grid = 6))
#'
#' # Pdqr-function with center at median is returned in case `n_grid` is 1
#' form_regrid(d_dis, n_grid = 1)
#' # Dirac-like function is returned
#' form_regrid(d_con, n_grid = 1)
#' @export
form_regrid <- function(f, n_grid, method = "x") {
  assert_pdqr_fun(f)
  assert_missing(n_grid, "grid size")
  assert_type(
    n_grid, is_single_number,
    type_name = "single positive number",
    min_val = 1
  )
  assert_method(method, methods_regrid)

  # Speed optimization (skips possibly expensive assertions)
  disable_asserting_locally()

  n_grid <- as.integer(n_grid)

  # Early regridding
  early_return <- early_regrid(f, n_grid)
  if (!is.null(early_return)) {
    return(early_return)
  }

  # Actual regridding
  ref_grid <- compute_grid(f, n_grid, method)

  adjust_to_grid(f, ref_grid)
}

methods_regrid <- c("x", "q")

early_regrid <- function(f, n_grid) {
  # Return dirac-like function at median if `n_grid == 1`
  if (n_grid == 1) {
    med <- as_q(f)(0.5)

    return(new_pdqr_by_ref(f)(med, meta_type(f)))
  }

  # Return input if `n_grid` is the same as number of present points or it is a
  # case of UPgridding a "discrete" pdqr-function
  n_f_x_tbl <- nrow(meta_x_tbl(f))
  is_equal_size <- n_grid == n_f_x_tbl
  is_dis_increasing <- (meta_type(f) == "discrete") && (n_grid > n_f_x_tbl)
  if (is_equal_size || is_dis_increasing) {
    return(f)
  }

  # If there are no early returns, return `NULL`
  NULL
}

compute_grid <- function(f, n_grid, method) {
  switch(
    method,
    x = compute_grid_x(f, n_grid),
    q = compute_grid_q(f, n_grid)
  )
}

compute_grid_x <- function(f, n_grid) {
  seq_between(meta_support(f), length.out = n_grid)
}

compute_grid_q <- function(f, n_grid) {
  # Note that this might have duplicate values if `f` is of `type` "discrete"
  as_q(f)(seq(0, 1, length.out = n_grid))
}

adjust_to_grid <- function(f, ref_grid) {
  switch(
    meta_type(f),
    discrete = adjust_to_grid_dis(f, ref_grid),
    continuous = adjust_to_grid_con(f, ref_grid)
  )
}

adjust_to_grid_dis <- function(f, ref_grid) {
  # If this function executes, it means that this is DOWNgridding (decreasing
  # granularity), i.e. `length(ref_grid)` is strictly less than
  # `nrow(meta_x_tbl(f))`.
  f_x_tbl <- meta_x_tbl(f)[, c("x", "prob")]
  x <- f_x_tbl[["x"]]

  # Uniquely match `ref_grid` with `x` in terms of "nearest" elements
  res_x <- x[find_nearest_match(ref_grid, x)]

  # Collapse surplus of `x` into the nearest `res_x`
  f_x_tbl[["x"]] <- res_x[find_nearest_ind(x, res_x)]

  new_pdqr_by_ref(f)(f_x_tbl, "discrete")
}

adjust_to_grid_con <- function(f, ref_grid) {
  f_x_tbl <- meta_x_tbl(f)[, c("x", "y")]
  x <- f_x_tbl[["x"]]
  n_grid_surplus <- length(ref_grid) - length(x)

  if (n_grid_surplus > 0) {
    # Case of UPgridding. Add to `f_x_tbl` rows with "x" equal to `ref_grid`
    # points furtherst to the set of `x`.
    furtherst_grid_inds <- find_neigh_subset(
      ref_grid, x, n_subset = n_grid_surplus, type = "max"
    )
    furtherst_grid <- ref_grid[furtherst_grid_inds]

    d_f <- as_d(f)

    # Here "x" column is not ordered, which should be imputed during `new_*()`
    x_tbl <- data.frame(
      x = c(x, furtherst_grid),
      y = c(f_x_tbl[["y"]], d_f(furtherst_grid))
    )
  } else {
    # Case of DOWNgridding. Uniquely match `ref_grid` with `x` in terms of
    # "nearest" elements.
    closest_x_inds <- find_nearest_match(ref_grid, x)

    x_tbl <- f_x_tbl[closest_x_inds, ]
  }

  # Special test to catch case when all y-values at new grid is zero. Other
  # violations of proper "x_tbl" structure are impossible because input `f` is
  # tested to be a proper pdqr-function.
  if (is_zero(sum(x_tbl[["y"]]))) {
    stop_collapse(
      'All y-values in `form_regrid()` output\'s "x_tbl" are zero. ',
      "Try different `n_grid`."
    )
  }

  new_pdqr_by_ref(f)(x_tbl, "continuous")
}
