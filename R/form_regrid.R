#' Change grid of pdqr-function
#'
#' Modify grid of pdqr-function to increase or decrease granularity using method
#' of choice.
#'
#' @param f A pdqr-function.
#' @param n_grid A desired number of grid elements in output.
#' @param method Regrid method. One of "x" or "q".
#'
#' @return A pdqr-function with modified grid.
#'
#' @examples
#' # Pdqr-function with type "infin"
#'   # Downgridding
#' d_norm <- as_d(dnorm)
#' plot(d_norm)
#' lines(form_regrid(d_norm, n_grid = 10), col = "blue")
#' lines(form_regrid(d_norm, n_grid = 10, method = "q"), col = "green")
#'
#'   # Upgridding
#' d_infin <- new_d(data.frame(x = 1:3, y = rep(0.5, 3)), type = "infin")
#' meta_x_tbl(form_regrid(d_infin, n_grid = 6))
#'
#' # Pdqr-function with type "fin"
#' d_fin <- new_d(data.frame(x = 1:10, prob = 1:10/55), type = "fin")
#'
#'   # Downgridding
#' meta_x_tbl(form_regrid(d_fin, n_grid = 4))
#' meta_x_tbl(form_regrid(d_fin, n_grid = 4, method = "q"))
#'
#'   # Upgridding for "fin" type isn't possible. Input is returned
#' identical(d_fin, form_regrid(d_fin, n_grid = 100))
#'
#' @export
form_regrid <- function(f, n_grid, method = "x") {
  assert_pdqr_fun(f)
  assert_type(
    n_grid, is_single_number,
    type_name = "single positive number",
    min_val = 1
  )
  assert_type(method, is_string)
  assert_in_set(method, c("x", "q"))

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

early_regrid <- function(f, n_grid) {
  # Return dirac-like function at median if `n_grid == 1`
  if (n_grid == 1) {
    med <- as_q(f)(0.5)

    return(new_pdqr_by_ref(f)(med, meta_type(f)))
  }

  # Return input if `n_grid` is the same as number of present points or it is a
  # case of UPgridding a "fin" pdqr-function
  n_f_x_tbl <- nrow(meta_x_tbl(f))
  is_equal_size <- n_grid == n_f_x_tbl
  is_fin_increasing <- (meta_type(f) == "fin") && (n_grid > n_f_x_tbl)
  if (is_equal_size || is_fin_increasing) {
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
  # Note that this might have duplicate values if `f` is of `type` "fin"
  as_q(f)(seq(0, 1, length.out = n_grid))
}

adjust_to_grid <- function(f, ref_grid) {
  switch(
    meta_type(f),
    fin = adjust_to_grid_fin(f, ref_grid),
    infin = adjust_to_grid_infin(f, ref_grid)
  )
}

adjust_to_grid_fin <- function(f, ref_grid) {
  # If this function executes, it means that this is DOWNgridding (decreasing
  # granularity), i.e. `length(ref_grid)` is strictly less than
  # `nrow(meta_x_tbl(f))`.
  f_x_tbl <- meta_x_tbl(f)[, c("x", "prob")]
  x <- f_x_tbl[["x"]]

  # Uniquely match `ref_grid` with `x` in terms of "nearest" elements
  res_x <- x[find_nearest_match(ref_grid, x)]

  # Collapse surplus of `x` into the nearest `res_x`
  f_x_tbl[["x"]] <- res_x[find_nearest_ind(x, res_x)]

  new_pdqr_by_ref(f)(f_x_tbl, "fin")
}

adjust_to_grid_infin <- function(f, ref_grid) {
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

  new_pdqr_by_ref(f)(x_tbl, "infin")
}
