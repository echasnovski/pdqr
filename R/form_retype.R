#' Change type of pdqr-function
#'
#' Modify [type][meta_type()] of pdqr-function using method of choice.
#'
#' @param f A pdqr-function.
#' @param type A desired type of output. Should be one of "discrete" or
#'   "continuous". If `NULL` (default), it is chosen as an opposite of `f`'s
#'   type.
#' @param method Retyping method. Should be one of "value", "piecelin", "dirac".
#'
#' @details If type of `f` is equal to input `type` then `f` is returned.
#'
#' Method "value" uses renormalized columns of `f`'s "x_tbl" metadata as values
#' for output's "x_tbl" metadata. In other words, it preserves ratios between
#' values of d-function at certain "x" points. Its main advantages are that this
#' method can work well with any pdqr type and that two consecutive conversions
#' return the same function. Conversion algorithm is as follows:
#' - Retyping from "continuous" to `type` "discrete" is done by creating
#' pdqr-function of corresponding class with the following "x_tbl" metadata: "x"
#' column is the same as in `f`; "prob" column is equal to `f`'s "y" column
#' after renormalization (so that their sum is 1).
#' - Retyping from "discrete" to `type` "continuous" is done in the same
#' fashion: "x" column is the same; "y" column is equal to `f`'s "prob" column
#' after renormalization (so that total integral of piecewise-linear density is
#' equal to 1).
#'
#' Method "piecelin" (default) should be used mostly for converting from
#' "continuous" to "discrete" type. It uses the fact that 'pdqr' densities are
#' piecewise-linear (linear in intervals between values of "x" column of
#' ["x_tbl" metadata][meta_x_tbl()]) on their [support][meta_support()]:
#' - Retyping from "continuous" to `type` "discrete" is done by computing "x"
#' values as centers of interval masses with probabilities equal to interval
#' total probabilities.
#' - Retyping from "discrete" to `type` "continuous" is made approximately by
#' trying to compute "x" grid, for which "x" values of input distribution are
#' going to be centers of mass. Algorithm is approximate and might result into a
#' big errors in case of small number of "x" values or if they are not
#' "suitable" for this kind of transformation.
#'
#' Method "dirac" is used mostly for converting from "discrete" to "continuous"
#' type (for example, in `form_mix()` in case different types of input
#' pdqr-functions). It works in the following way:
#' - Retyping from "continuous" to `type` "discrete" works only if "x_tbl"
#' metadata represents a mixture of dirac-like distributions. In that case it is
#' transformed to have "x" values from centers of those dirac-like distributions
#' with corresponding probabilities.
#' - Retyping from "discrete" to `type` "continuous" works by transforming each
#' "x" value from "x_tbl" metadata into dirac-like distribution with total
#' probability taken from corresponding value of "prob" column. Output
#' essentially represents a mixture of dirac-like distributions.
#'
#' @return A pdqr-function with type equal to input `type`.
#'
#' @seealso [form_regrid()] for changing grid (rows of "x_tbl" metadata) of
#'   pdqr-function.
#'
#' [form_resupport()] for changing support of pdqr-function.
#'
#' @family form functions
#'
#' @examples
#' my_con <- new_d(data.frame(x = 1:5, y = c(1, 2, 3, 2, 1)/9), "continuous")
#' meta_x_tbl(my_con)
#'
#'   # By default, conversion is done to the opposite type
#' my_dis <- form_retype(my_con)
#' meta_x_tbl(my_dis)
#'
#' # Default retyping (with method "value") is accurate when doing consecutive
#' # retyping
#' my_con_2 <- form_retype(my_dis, "continuous")
#' meta_x_tbl(my_con_2)
#'
#' # Method "dirac"
#' my_dirac <- form_retype(my_dis, "continuous", method = "dirac")
#' meta_x_tbl(my_dirac)
#'
#' # Method "piecelin"
#'   # From "continuous" to "discrete" (preferred direction)
#' my_dis_piece <- form_retype(my_con, "discrete", method = "piecelin")
#' meta_x_tbl(my_dis_piece)
#'   # Conversion from "discrete" to "continuous" is very approximate
#' my_con_piece <- form_retype(my_dis_piece, "continuous", method = "piecelin")
#' meta_x_tbl(my_con_piece)
#'
#' plot(my_con, main = 'Approximate nature of method "piecelin"')
#' lines(my_con_piece, col = "blue")
#'
#' @export
form_retype <- function(f, type = NULL, method = "value") {
  assert_pdqr_fun(f)
  assert_pdqr_type(type, allow_null = TRUE)
  assert_type(method, is_string)
  assert_in_set(method, c("piecelin", "dirac", "value"))

  if (is.null(type)) {
    type <- switch(
      meta_type(f),
      discrete = "continuous",
      continuous = "discrete"
    )
  }

  switch(
    type,
    discrete = retype_dis(f, method),
    continuous = retype_con(f, method)
  )
}

retype_dis <- function(f, method) {
  if (meta_type(f) == "discrete") {
    return(f)
  }

  switch(
    method,
    value = retype_dis_value(f),
    piecelin = retype_dis_piecelin(f),
    dirac = retype_dis_dirac(f)
  )
}

retype_dis_value <- function(f) {
  x_tbl <- meta_x_tbl(f)
  # Renormalization of "prob" column will be done inside `new_*()` function
  new_x_tbl <- data.frame(x = x_tbl[["x"]], prob = x_tbl[["y"]])

  new_pdqr_by_ref(f)(new_x_tbl, type = "discrete")
}

retype_dis_piecelin <- function(f) {
  x_tbl <- meta_x_tbl(f)
  n <- nrow(x_tbl)

  x_lag <- x_tbl[["x"]][-n]
  x_lead <- x_tbl[["x"]][-1]
  y_lag <- x_tbl[["y"]][-n]
  y_lead <- x_tbl[["y"]][-1]
  y_sum <- y_lag + y_lead

  # Output `x` values are computed as intervals' centers of mass
  x_mass <- (x_lag * (y_lag + y_sum) + x_lead * (y_lead + y_sum)) / (3 * y_sum)
    # If interval has zero probability then its centre is set to the middle
  x_mass_bad <- !is.finite(x_mass)
  x_mass[x_mass_bad] <- (x_lag[x_mass_bad] + x_lead[x_mass_bad]) / 2

  # Output probabilities are computed as probabilites (mass) of intervals
  prob <- diff(x_tbl[["cumprob"]])

  # Creating pdqr-function
  pdqr_fun <- new_pdqr_by_ref(f)

  pdqr_fun(data.frame(x = x_mass, prob = prob), "discrete")
}

retype_dis_dirac <- function(f) {
  x_tbl <- meta_x_tbl(f)
  # Ensure presense of zero densities
  x_tbl <- ground_x_tbl(x_tbl)

  # One output "discrete" value is computed as mean of two consequtive "x"s with
  # zero density. Each "x" corresponds to only one "discrete" value counting
  # from left: x_1 and x_2 are used to compute first "discrete" value; x_3 and
  # x_4 - second, and so on. Probability of "discrete" value is computed as
  # difference in cumulative probabilities between corresponding right and left
  # "x" values.
  y_is_zero <- is_zero(x_tbl[["y"]])
  n_y_zero <- sum(y_is_zero)
  dis_groups <- rep(seq_len(n_y_zero), each = 2, length.out = n_y_zero)

  new_x <- tapply(x_tbl[["x"]][y_is_zero], dis_groups, mean)
  # Remove dimnames
  new_x <- as.vector(new_x)

  new_prob <- tapply(
    x_tbl[["cumprob"]][y_is_zero],
    dis_groups,
    function(cumprob) {
      # This custom function is used instead of `diff()` if, for some reason,
      # there are odd number of zero density rows in `x_tbl`. In that case
      # output probability is zero.
      cumprob[length(cumprob)] - cumprob[1]
    })
  # Remove dimnames
  new_prob <- as.vector(new_prob)

  new_pdqr_by_ref(f)(data.frame(x = new_x, prob = new_prob), "discrete")
}

retype_con <- function(f, method) {
  if (meta_type(f) == "continuous") {
    return(f)
  }

  switch(
    method,
    value = retype_con_value(f),
    piecelin = retype_con_piecelin(f),
    dirac = retype_con_dirac(f)
  )
}

retype_con_value <- function(f) {
  x_tbl <- meta_x_tbl(f)
  # Renormalization of "y" column will be done inside `new_*()` function
  new_x_tbl <- data.frame(x = x_tbl[["x"]], y = x_tbl[["prob"]])

  new_pdqr_by_ref(f)(new_x_tbl, type = "continuous")
}

retype_con_piecelin <- function(f) {
  # Note that `f` has already passed `assert_pdqr_fun()` which means that "x"
  # column in "x_tbl" metadata is sorted and has no duplicate values
  x_tbl <- meta_x_tbl(f)

  n <- nrow(x_tbl)
  if (n < 4) {
    stop_collapse(
      'For conversion to "continuous" type `form_retype()` needs at least 4 ',
      'unique "x" values.'
    )
  }

  x <- x_tbl[["x"]]
  prob <- x_tbl[["prob"]]

  # Values of `x` grid (except first and last elements) of "continuous" output
  # is approximated as convex combination of nearest "centers of mass":
  # `x_mid = (1-alpha)*x_mass_left + alpha*x_mass_right`. Here `alpha` is
  # approximated based on two separate assumptions:
  # *Assumption 1*: if locally `y_left` (`y` value in "true" `x` to the left of
  # `x_mid`) = `y_mid` = `y_right` (by analogy with `y_left`), then
  # `alpha = prob_left / (prob_left + prob_right)`.
  # *Assumption 2*: if locally "true" values of `x` are equidistant then `alpha`
  # lie inside [1/3; 2/3] interval.
  # Final approximation is formed by combining these conclusions
  prob_sum <- prob[1:(n-1)] + prob[2:n]
  alpha <- pmin(pmax(prob[1:(n-1)] / prob_sum, 1/3), 2/3)
  alpha[!is.finite(alpha)] <- 0.5

  x_grid <- numeric(n + 1)
  x_grid[2:n] <- (1 - alpha) * x[1:(n-1)] + alpha * x[2:n]
  # First and last `x` are approximated so that first and last `x` triplets are
  # equidistant
  x_grid[1] <- x_grid[2] - (x_grid[3] - x_grid[2])
  x_grid[n+1] <- x_grid[n] + (x_grid[n] - x_grid[n-1])

  # Output `y` grid is approximated in 'pdqr' fashion
  p_grid <- c(0, cumsum(prob))
  y <- y_from_p_grid(x_grid, p_grid)

  # Creating pdqr-function
  pdqr_fun <- new_pdqr_by_ref(f)

  pdqr_fun(data.frame(x = x_grid, y = y), "continuous")
}

retype_con_dirac <- function(f, h = 1e-8) {
  x_tbl <- meta_x_tbl(f)
  x <- x_tbl[["x"]]
  half_diff_x <- diff(x) / 2

  # Vector of "dirac" radius values
  left_h_vec <- pmin(h, c(h, half_diff_x))
  right_h_vec <- pmin(h, c(half_diff_x, h))
  h_vec <- pmin(left_h_vec, right_h_vec)

  y_zero <- rep(0, length(x))
  new_x <- c(x - h_vec,                       x, x + h_vec)
  new_y <- c(   y_zero, x_tbl[["prob"]] / h_vec,    y_zero)

  new_pdqr_by_ref(f)(data.frame(x = new_x, y = new_y), "continuous")
}
