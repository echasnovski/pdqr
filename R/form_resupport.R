# form_resupport ----------------------------------------------------------
#' Change support of pdqr-function
#'
#' Modify support of pdqr-function using method of choice.
#'
#' @param f A pdqr-function.
#' @param support Numeric vector with two increasing (or non-decreasing, see
#'   Details) elements describing support of the output. Values can be `NA`, in
#'   which case corresponding edge(s) are taken from `f`'s support.
#' @param method Resupport method. One of "reflect", "trim", "winsor", "linear".
#'
#' @details Method "reflect" takes a density "tails" to the left of `support[1]`
#' and to the right of `support[2]` and reflects them inside `support`. It means
#' that values of density inside and outside of supplied `support` are added
#' together in "symmetric fashion":
#' `d(x) = d_f(x) + d_f(l - (x-l)) + d_f(r + (r-x))`, where `d_f` is density of
#' input, `d` is density of output, `l` and `r` are left and right edges of
#' input `support`. This option is useful for repairing support of
#' [new_*()][new-pdqr]'s output, as by default kernel density estimation in
#' `density()` adds tails to the range of input `x` values. For example, if
#' there is a need to ensure that distribution has only positive values, one can
#' do `form_resupport(f, c(0, NA), method = "reflect")`. **Notes**:
#' - For "discrete" pdqr-functions that might result into creating new "x"
#' values of distribution.
#' - Reflection over `support[1]` is done only if it is strictly greater than
#' `f`'s left edge of support. Reflection over `support[2]` - if `f`'s right
#' edge is strictly smaller.
#'
#' Method "trim" removes density "tails" outside of `support`, normalizes the
#' rest and creates appropriate pdqr-function.
#'
#' Method "winsor" makes all density "tails" outside of input `support`
#' "squashed" inside it in "dirac-like" fashion. It means that probability from
#' both tails is moved inside `support` and becomes concentrated in `1e-8`
#' neighborhood of nearest edge. This models a singular dirac distributions at
#' the edges of `support`. **Note** that `support` can represent single point,
#' in which case output has single element if `f`'s type is "discrete" or is a
#' dirac-like distribution in case of "continuous" type.
#'
#' Method "linear" transforms `f`'s support linearly to be input `support`. For
#' example, if `f`'s support is \[0; 1\] and `support` is `c(-1, 1)`, linear
#' resupport is equivalent to `2*f - 1`. **Note** that `support` can represent
#' single point with the same effect as in "winsor" method.
#'
#' @return A pdqr-function with modified support and the same
#'   [class][meta_class()] and [type][meta_type()] as `f`.
#'
#' @seealso [form_regrid()] for changing grid (rows of "x_tbl" metadata) of
#'   pdqr-function.
#'
#' [form_retype()] for changing type of pdqr-function.
#'
#' @family form functions
#'
#' @examples
#' set.seed(101)
#' d_norm <- as_d(dnorm)
#' d_dis <- new_d(data.frame(x = 1:4, prob = 1:4 / 10), "discrete")
#'
#' # Method "reflect"
#' plot(d_norm)
#' lines(form_resupport(d_norm, c(-2, 1.5), "reflect"), col = "blue")
#'
#' # For "discrete" functions it might create new values
#' meta_x_tbl(form_resupport(d_dis, c(NA, 2.25), "reflect"))
#'
#' # This is often useful to ensure constraints after `new_()`
#' x <- runif(1e4)
#' d_x <- new_d(x, "continuous")
#' plot(d_x)
#' lines(form_resupport(d_x, c(0, NA), "reflect"), col = "red")
#' lines(form_resupport(d_x, c(0, 1), "reflect"), col = "blue")
#'
#' # Method "trim"
#' plot(d_norm)
#' lines(form_resupport(d_norm, c(-2, 1.5), "trim"), col = "blue")
#'
#' # Method "winsor"
#' plot(d_norm)
#' lines(form_resupport(d_norm, c(-2, 1.5), "winsor"), col = "blue")
#'
#' # Method "linear"
#' plot(d_norm)
#' lines(form_resupport(d_norm, c(-2, 1.5), "linear"), col = "blue")
#' @export
form_resupport <- function(f, support, method = "reflect") {
  assert_pdqr_fun(f)
  assert_missing(support, "vector for support")
  assert_support(support, allow_na = TRUE)
  assert_method(method, methods_resupport)

  # Speed optimization (skips possibly expensive assertions)
  disable_asserting_locally()

  if (all(is.na(support))) {
    return(f)
  }

  supp <- coalesce_pair(support, meta_support(f))
  if (supp[1] > supp[2]) {
    stop_collapse(
      "After imputing `NA`s `support` equals (",
      supp[1], ", ", supp[2], ") which is not proper."
    )
  }

  switch(
    method,
    reflect = resupport_reflect(f, supp),
    trim = resupport_trim(f, supp),
    winsor = resupport_winsor(f, supp),
    linear = resupport_linear(f, supp)
  )
}

methods_resupport <- c("reflect", "trim", "winsor", "linear")


# "reflect" ---------------------------------------------------------------
resupport_reflect <- function(f, support) {
  f_supp <- meta_support(f)
  f_x_tbl <- meta_x_tbl(f)

  # Sum up densities for possible reflections
  x_tbl_list <- list(f_x_tbl)

  if (support[1] > f_supp[1]) {
    x_tbl_list <- c(x_tbl_list, list(reflect_x_tbl(f_x_tbl, support[1])))
  }
  if (support[2] < f_supp[2]) {
    x_tbl_list <- c(x_tbl_list, list(reflect_x_tbl(f_x_tbl, support[2])))
  }

  x_tbl <- stack_x_tbl(x_tbl_list)
  res <- new_pdqr_by_ref(f)(x_tbl, meta_type(f))

  # Trim total sum to supplied support
  form_resupport(res, support, "trim")
}


# "trim" ------------------------------------------------------------------
resupport_trim <- function(f, support) {
  switch(
    meta_type(f),
    discrete = resupport_trim_dis(f, support),
    continuous = resupport_trim_con(f, support)
  )
}

resupport_trim_dis <- function(f, support) {
  res_x_tbl <- filter_x_tbl(meta_x_tbl(f), support)
  res_x_tbl <- res_x_tbl[, c("x", "prob")]

  if (sum(res_x_tbl[["prob"]]) <= 0) {
    stop_resupport_zero_tot_prob()
  }

  new_pdqr_by_ref(f)(res_x_tbl, "discrete")
}

resupport_trim_con <- function(f, support) {
  d_f <- as_d(f)
  edge_y <- d_f(support)

  # Add new rows to "x_tbl" metadata of `f` to capture "trimming" behavior.
  # However, if edge of `support` is outside of `f`'s support, then it is not
  # added. This is needed to not change shape of distribution.
  x_tbl_plus <- union_inside_x_tbl(
    x_tbl_orig = meta_x_tbl(f),
    x_tbl_new = data.frame(x = support, y = edge_y)
  )
  res_x_tbl <- filter_x_tbl(x_tbl_plus, support)

  if (trapez_integral(res_x_tbl[["x"]], res_x_tbl[["y"]]) <= 0) {
    stop_resupport_zero_tot_prob()
  }

  new_pdqr_by_ref(f)(res_x_tbl, "continuous")
}


# "winsor" ----------------------------------------------------------------
resupport_winsor <- function(f, support) {
  if (support[1] == support[2]) {
    return(new_pdqr_by_ref(f)(support[1], meta_type(f)))
  }

  switch(
    meta_type(f),
    discrete = resupport_winsor_dis(f, support),
    continuous = resupport_winsor_con(f, support)
  )
}

resupport_winsor_dis <- function(f, support) {
  f_x_tbl <- meta_x_tbl(f)
  x <- f_x_tbl[["x"]]
  x[x <= support[1]] <- support[1]
  x[x >= support[2]] <- support[2]
  f_x_tbl[["x"]] <- x

  new_pdqr_by_ref(f)(f_x_tbl, "discrete")
}

resupport_winsor_con <- function(f, support, h = 1e-8) {
  p_f <- as_p.pdqr(f)
  f_x_tbl <- meta_x_tbl(f)
  f_supp <- meta_support(f)

  # Early return extreme cases
  if (support[1] >= f_supp[2]) {
    return(new_pdqr_by_ref(f)(support[1], meta_type(f)))
  }
  if (support[2] <= f_supp[1]) {
    return(new_pdqr_by_ref(f)(support[2], meta_type(f)))
  }

  x_tbl <- f_x_tbl

  # Winsor left
  if (support[1] > f_supp[1]) {
    x_tbl <- add_x_tbl_knots(x_tbl, support[1] + c(0, h))
    x_tbl <- filter_x_tbl(x_tbl, c(support[1], f_supp[2]))
    tail_prob <- p_f(support[1])
    x_tbl <- increase_tail_weight(x_tbl, tail_prob, "left")
  }

  # Winsor right
  if (support[2] < f_supp[2]) {
    x_tbl <- add_x_tbl_knots(x_tbl, support[2] - c(h, 0))
    x_tbl <- filter_x_tbl(x_tbl, c(f_supp[1], support[2]))
    tail_prob <- 1 - p_f(support[2])
    x_tbl <- increase_tail_weight(x_tbl, tail_prob, "right")
  }

  new_pdqr_by_ref(f)(x_tbl, "continuous")
}

increase_tail_weight <- function(x_tbl, by_prob, edge) {
  n <- nrow(x_tbl)
  x <- x_tbl[["x"]]
  y <- x_tbl[["y"]]

  if (edge == "left") {
    present_prob <- (y[1] + y[2]) * (x[2] - x[1]) / 2
    to_prob <- present_prob + by_prob
    y[1] <- 2 * to_prob / (x[2] - x[1]) - y[2]
  } else if (edge == "right") {
    present_prob <- (y[n - 1] + y[n]) * (x[n] - x[n - 1]) / 2
    to_prob <- present_prob + by_prob
    y[n] <- 2 * to_prob / (x[n] - x[n - 1]) - y[n - 1]
  }

  data.frame(x = x, y = y)
}


# "linear" ----------------------------------------------------------------
resupport_linear <- function(f, support) {
  if (support[1] == support[2]) {
    # Return dirac-like function
    return(new_pdqr_by_ref(f)(support[1], meta_type(f)))
  }

  f_supp <- meta_support(f)

  if (f_supp[1] == f_supp[2]) {
    stop_collapse(
      "Can't resupport from single point support to interval one."
    )
  }

  res_x_tbl <- meta_x_tbl(f)
  res_x_tbl[["x"]] <- extrap_lin(
    x_1 = f_supp[1], x_2 = f_supp[2],
    y_1 = support[1], y_2 = support[2],
    x_target = res_x_tbl[["x"]]
  )

  new_pdqr_by_ref(f)(res_x_tbl, meta_type(f))
}


# Other -------------------------------------------------------------------
stop_resupport_zero_tot_prob <- function() {
  stop_collapse(
    "Output of `form_resupport()` will not have positive total probability."
  )
}
