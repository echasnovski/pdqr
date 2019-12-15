context("test-utils")


# Input data --------------------------------------------------------------
x <- 1
null <- NULL


# is_near -----------------------------------------------------------------
test_that("is_near works", {
  expect_equal(
    is_near(1, 1 + 0.001 * 0:20, tol = 0.01),
    c(rep(TRUE, 10), rep(FALSE, 11))
  )
})


# is_zero -----------------------------------------------------------------
test_that("is_zero works", {
  expect_equal(
    is_zero(c(-1, -1e-11, -1e-13, 0, 1e-13, 1e-11, 1)),
    c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE)
  )
})


# is_string ---------------------------------------------------------------
test_that("is_string works", {
  expect_true(is_string("a"))
  expect_false(is_string(c("a", "b")))
  expect_false(is_string(1))
})


# is_single_color ---------------------------------------------------------
test_that("is_single_color works", {
  expect_true(is_single_color("black"))
  expect_true(is_single_color("#000000"))
  expect_true(is_single_color("#00000033"))
  # Number in palette
  expect_true(is_single_color(1))
  # Seems to be the same as previous one
  expect_true(is_single_color("01"))
  # `col2rgb()` accepts `NA`s
  expect_true(is_single_color(NA_character_))

  expect_false(is_single_color(c("black", "red")))
  expect_false(is_single_color("a"))
})


# is_single_number --------------------------------------------------------
test_that("is_single_number works", {
  expect_true(is_single_number(1))
  expect_true(is_single_number(0L))
  expect_false(is_single_number(1:2))
  expect_false(is_single_number("a"))
  expect_false(is_single_number(Inf))
  expect_false(is_single_number(NA_real_))
})

test_that("is_single_number checks value bounds", {
  expect_true(is_single_number(1, min_val = 0))
  expect_true(is_single_number(1, min_val = 1))
  expect_false(is_single_number(1, min_val = 2))

  expect_true(is_single_number(1, max_val = 2))
  expect_true(is_single_number(1, max_val = 1))
  expect_false(is_single_number(1, max_val = 0))

  expect_true(is_single_number(1, min_val = 0, max_val = 2))
  expect_true(is_single_number(1, min_val = 1, max_val = 1))
  expect_false(is_single_number(NA_real_, min_val = 1, max_val = 1))
})


# is_truefalse ------------------------------------------------------------
test_that("is_truefalse works", {
  expect_true(is_truefalse(TRUE))
  expect_true(is_truefalse(FALSE))
  expect_false(is_truefalse(NA))
  expect_false(is_truefalse(c(TRUE, TRUE)))
  expect_false(is_truefalse(1))
})


# neigh_dist --------------------------------------------------------------
test_that("neigh_dist works",  {
  vec      <- c(  2,   1, 2.5,  2.6, -100,  100)

  min_dist <- c(0.5,   1, 0.1,  0.1,  101, 97.4)
  max_dist <- c(  1, 101, 0.5, 97.4,  101, 97.4)

  expect_equal(neigh_dist(vec, "min"), min_dist)
  expect_equal(neigh_dist(vec, "max"), max_dist)

  # Output doesn't depend on order of input
  vec_ord <- order(vec)
  expect_equal(neigh_dist(vec[vec_ord], "min"), min_dist[vec_ord])
  expect_equal(neigh_dist(vec[vec_ord], "max"), max_dist[vec_ord])

  # Default distance is used
  expect_equal(
    neigh_dist(vec, "max", def_dist = 1e5),
    c(1, 101, 0.5, 97.4, 1e5, 1e5)
  )
})


# compute_d_con_ylim ------------------------------------------------------
# Tested in `plot.d()`


# recycle_vec -------------------------------------------------------------
test_that("recycle_vec works", {
  expect_equal(recycle_vec(1:2, 2), 1:2)
  expect_equal(recycle_vec(1, 3), c(1, 1, 1))

  input <- 1:3
  expect_error(recycle_vec(input, 2), "`input`.*length 1 or 2")
})


# inversing ---------------------------------------------------------------
test_that("inversing works", {
  square <- function(x) {x^2}
  inv_square <- inversing(square, c(0.5, 10))
  x_vec <- sample(seq(0.5, 10, by = 0.01))

  max_error <- max(abs(inv_square(x_vec) - sqrt(x_vec)))
  expect_true(max_error <= 10^(-4))
})

test_that("inversing linearly imputes infinite values", {
  f_inv <- inversing(function(x) {1 / x}, c(0, 1), n_grid = 11)
  expect_equal(f_inv(10:15), seq(0.1, 0, by = -0.02))
})

test_that("inversing accepts extra arguments for input function", {
  f_inv <- inversing(qunif, c(0, 1), min = 10, max = 11)
  f_ref <- function(q) {punif(q, min = 10, max = 11)}
  expect_close_f(f_inv, f_ref, seq(-9, 12, by = 0.001))
})

test_that("inversing accepts extra arguments for `approxfun()`", {
  f_inv <- inversing(
    qunif, c(0, 1), approxfun_args = list(yleft = -100, yright = 100)
  )
  expect_equal(
    f_inv(c(-1, -0.001, 0, 1, 1.001, 2)), c(-100, -100, 0, 1, 100, 100)
  )
})


# impute_inf --------------------------------------------------------------
test_that("impute_inf works", {
  expect_equal(
    # All equations are y = x
    impute_inf(1:6, c(1, Inf, 3, 4, Inf, 6), "y"), 1:6
  )
  expect_equal(
    # All equations are y = x
    impute_inf(1:7, c(Inf, 2, Inf, 4, Inf, 6, Inf), "y"), 1:7
  )
  expect_equal(
    # Left: y = x, right: y = -2x + 10
    impute_inf(1:5, c(1, 2, Inf, 2, 0), "y"), c(1, 2, 4, 2, 0)
  )
  expect_equal(
    # Left: y = x, right: y = -x + 9
    impute_inf(c(1, 2, 3, 7, 9), c(1, 2, Inf, 2, 0), "y"), c(1, 2, 6, 2, 0)
  )
  expect_equal(
    # Left: y = x, right: y = x + 7
    impute_inf(c(1, 2, 3, 7, 9), c(1, 2, Inf, 0, 2), "y"), c(1, 2, 3, 0, 2)
  )

  expect_error(impute_inf(1:3, c(NA, 1, 3), "`a`"), "[Aa]ll.*`a`.*number.*NA")
  expect_error(impute_inf(1:3, c(1, 2, Inf), "`a`"), "`a`.*3 finite values")
})


# impute_linearly ---------------------------------------------------------
# Tested in `impute_inf()`


# extrap_lin --------------------------------------------------------------
test_that("extrap_lin works", {
  # True are: y = -x, y = 10, y = 2x + 2
  expect_equal(
    extrap_lin(
      x_1 = c(-1,  0, 1), x_2 = c( 2,  3,  4),
      y_1 = c( 1, 10, 4), y_2 = c(-2, 10, 10),
      x_target = c(-10, 0, 10)
    ),
    c(10, 10, 22)
  )
})


# stretch_range -----------------------------------------------------------
test_that("stretch_range works", {
  expect_equal(stretch_range(c(0, 1)), c(-10^(-6), 1 + 10^(-6)))
  expect_equal(stretch_range(c(0, 1), 1), c(-1, 2))
  expect_equal(stretch_range(c(0, 1), c(1, 2)), c(-1, 3))
})


# seq_between -------------------------------------------------------------
test_that("seq_between works",  {
  expect_equal(
    seq_between(c(-1, 10), length.out = 101),
    seq(from = -1, to = 10, length.out = 101)
  )
  expect_equal(
    seq_between(c(-1, 10), by = 0.03),
    seq(from = -1, to = 10, by = 0.03)
  )
})


# dotprod -----------------------------------------------------------------
test_that("dotprod works", {
  expect_equal(dotprod(1:10, 10:1), sum((1:10) * (10:1)))
  expect_equal(dotprod(c(1, NA, 3, NA), c(1, 2, NA, NA)), 1)
})


# coalesce_pair -----------------------------------------------------------
test_that("coalesce_pair works", {
  expect_equal(
    coalesce_pair(c(1, NA, NA, 4), c(NA, 2, NA, 3)),
    c(1, 2, NA, 4)
  )
})


# enbacktick --------------------------------------------------------------
test_that("enbacktick works", {
  expect_equal(enbacktick(c("a", "b", "c")), c("`a`", "`b`", "`c`"))
})


# integrate_safely --------------------------------------------------------
test_that("integrate_safely integrates when `stats::integrate()` can't", {
  expect_silent(integrate_safely(fam_beta_inf$d, -1e8, 1e8))
})

test_that("integrate_safely throws informative error", {
  f <- function(x) {rep(Inf, length.out = length(x))}
  expect_error(integrate_safely(f, -1, 1), "Can't")
})


# inf_to_na ---------------------------------------------------------------
test_that("inf_to_na works", {
  expect_equal(inf_to_na(1:10), 1:10)
  expect_equal(inf_to_na(c(Inf, NA, -Inf, 1)), c(NA, NA, NA, 1))
})


# all_same ----------------------------------------------------------------
test_that("all_same works", {
  expect_true(all_same(rep(pi, 3)))
  expect_false(all_same(c(1, 2, 1)))
  expect_false(all_same(1 + c(0, 1e-10)))
  expect_true(all_same(1 + c(0, 1e-10), tolerance = 0.1))
})


# alternate ---------------------------------------------------------------
test_that("alternate works", {
  expect_equal(alternate(1:3, (-3):(-1)), c(1, -3, 2, -2, 3, -1))
  expect_equal(alternate(numeric(0), numeric(0)), numeric(0))
  expect_equal(
    alternate(c(NA_real_, 2), c(1L, NA_integer_)), c(NA_real_, 1, 2, NA_real_)
  )
})


# c_dedupl ----------------------------------------------------------------
test_that("c_dedupl works", {
  input_1 <- list(1, 2)
  expect_equal(c_dedupl(input_1), input_1)

  input_2 <- list(1, 2, c = 3)
  expect_equal(c_dedupl(input_2), input_2)

  input_3 <- list(a = 1, 2, c = 3, a = 4)
  expect_equal(c_dedupl(input_3), input_3[-4])

  expect_equal(c_dedupl(a = 2, input_2, c = 4), list(a = 2, 1, 2, c = 3))
  expect_equal(c_dedupl(c = 2, list(a = 1, c = 3)), list(c = 2, a = 1))
})


# swap --------------------------------------------------------------------
test_that("swap works", {
  expect_equal(
    swap(list(a = 1, b = 2, c = 3), "a", "b"), list(a = 2, b = 1, c = 3)
  )
})


# add_class ---------------------------------------------------------------
test_that("add_class works", {
  input <- structure(1, class = c("a", "b"))
  expect_equal(add_class(input, "c"), structure(1, class = c("c", "a", "b")))
  expect_equal(add_class(input, "a"), input)
  expect_equal(
    add_class(input, c("c", "a")), structure(1, class = c("c", "a", "b"))
  )
  expect_equal(
    add_class(input, c("c", "a", "b")),
    structure(1, class = c("c", "a", "b", "a", "b"))
  )
})


# copy_attrs --------------------------------------------------------------
test_that("copy_attrs works", {
  input <- structure(1, a = 2)
  output <- copy_attrs(input, structure("a", b = 3))
  expect_equal(attributes(output), list(b = 3))
})


# find_nearest_ind --------------------------------------------------------
test_that("find_nearest_ind works", {
  x <- c(1, -100, 10000, 1.5, 2.01, 1.99, 1)
  set <- c(2, 1, 2, 2, 2.5, -1, 1000)

  expect_equal(
    find_nearest_ind(x, set),
    c(2, 6, 7, 2, 4, 1, 2)
  )

  # If there are duplicate values in `set` then the first one is used for
  # smaller `x` values and the last one for the bigger `x`.
  expect_equal(find_nearest_ind(c(1.9, 2.1), c(2, 2)), c(1, 2))

  # If there are two equidistant `set` points, then the smaller is returned
  expect_equal(find_nearest_ind(1.5, c(1, 2)), 1)

  # Case of length one `set`
  expect_equal(find_nearest_ind(1:10, 3), rep(1, 10))
})


# find_nearest_match ------------------------------------------------------
test_that("find_nearest_match works", {
  expect_equal(
    find_nearest_match(
      x = c(1, 2, 3, 4, 5, 6),
      set = c(-3, -2, -1, 1.5, 3.5, 5.5, 10, 20)
    ),
    # Match steps (in terms of elements, not indicies).
    # Step 1: 1 <-> 1.5, 3 <-> 3.5, 5 <-> 5.5
    # Step 2: 2 <-> -1, 6 <-> 10 (closest in `set` accept those "used" in
    # previous step)
    # Step 3: 4 <-> -2
    c(4, 3, 5, 2, 6, 7)
  )

  expect_equal(
    find_nearest_match(c(1, 10, 10, 10, 10), 1:10),
    # Steps. Step 1: 1 <-> 1, 10 <-> 10. Step 2: 10 <-> 9. Step 3: 10 <-> 8.
    # Step 4: 10 <-> 7.
    c(1, 10, 9, 8, 7)
  )

  # Output should be equal to that from `find_nearest_ind()` if it is unique
  x <- 1:6
  set <- x + 0.1
  expect_equal(find_nearest_match(x, set), find_nearest_ind(x, set))

  # Example of more adequacy of nearest matching instead of "nearest subset":
  # indicies from `set` with the least distances to `x` (as a set).
  expect_equal(
    find_nearest_match(
      x = 1:3,
      # Here "nearest subset" will produce `c(1, 2, 3)` which is not very good
      # because it doesn't really capture a "structure" of a `set` (to be fair,
      # this is an arguable statement).
      set = c(0.98, 0.99, 1.01, 2.2, 3.1)
    ),
    c(2, 4, 5)
  )
  # Example of non-optimality of exact matching and more adequacy of
  # nearest matching concept over nearest index concept (in terms of finding
  # nearest match)
  expect_equal(
    find_nearest_match(x = 1:3, set = c(-2, 2, 6)),
    # More optimal solution (in terms of minimizing sum of distances) would be
    # c(1, 2, 3): it produces total distance of 6 instead of 8 here.
    # Also, output of `find_nearest_ind(x, set)` produces `c(2, 2, 2)` which is
    # not good because of non-unique values.
    c(2, 1, 3)
  )
})

test_that("find_nearest_match throws error if `set` is shorter than `x`", {
  expect_error(find_nearest_match(1:10, 1:3), "`set`.*`x`")
})


# find_neigh_subset -------------------------------------------------------
test_that("find_neigh_subset works", {
  x <- c(0.98, 0.99, 1.01, 2.2, 3.1)
  set <- 1:3

  # Using `setequal()` because order doesn't matter
  expect_true(setequal(find_neigh_subset(x, set, n_subset = 2), 2:3))
  expect_true(setequal(find_neigh_subset(x, set, n_subset = 4), c(1:3, 5)))

  expect_true(setequal(
    find_neigh_subset(x, set, n_subset = 2, type = "max"), 4:5
  ))
  expect_true(setequal(
    find_neigh_subset(x, set, n_subset = 4, type = "max"), c(1:2, 4:5)
  ))
})

test_that("find_neigh_subset throws error if `n_subset` is incorrect", {
  expect_error(find_neigh_subset(1:5, 1:10, n_subset = 10), "`n_subset`")
})


# stop_collapse -----------------------------------------------------------
test_that("stop_collapse works", {
  expect_error(
    stop_collapse("x = ", x, ", null = ", null),
    "x = 1, null = NULL"
  )
})


# warning_collapse --------------------------------------------------------
test_that("warning_collapse works", {
  expect_warning(
    warning_collapse("x = ", x, ", null = ", null),
    "x = 1, null = NULL"
  )
})


# message_collapse --------------------------------------------------------
test_that("message_collapse works", {
  expect_message(
    message_collapse("x = ", x, ", null = ", null),
    "x = 1, null = NULL"
  )
})


# collapse_nullable -------------------------------------------------------
test_that("collapse_nullable works", {
  expect_equal(
    collapse_nullable("x = ", x, ", null = ", null),
    "x = 1, null = NULL"
  )

  expect_error(collapse_nullable(1:2, c("a", "b")), "length 1")
})


# capture_null ------------------------------------------------------------
test_that("capture_null works", {
  expect_equal(capture_null(NULL), "NULL")
  expect_equal(capture_null(1), 1)
  expect_equal(capture_null("a"), "a")
})


# pdqr_approx_error -------------------------------------------------------
test_that("pdqr_approx_error works", {
  skip_if_noLD()

  d_unif <- as_d(dunif)
  approx_error_d <- pdqr_approx_error(d_unif, dunif, gran = 10)
  expect_named(approx_error_d, c("grid", "error", "abserror"))
  expect_equal(
    approx_error_d[["grid"]],
    granulate_grid(d_unif, gran = 10)
  )
  expect_true(all(approx_error_d[["error"]] == 0))
  expect_true(
    all(approx_error_d[["abserror"]] == abs(approx_error_d[["error"]]))
  )

  p_norm <- as_p(pnorm)
  approx_error_p <- pdqr_approx_error(p_norm, pnorm, gran = 10)
  expect_named(approx_error_p, c("grid", "error", "abserror"))
  expect_equal(
    approx_error_p[["grid"]],
    granulate_grid(p_norm, gran = 10)
  )
  expect_true(max(approx_error_p[["abserror"]]) < 1e-5)
  expect_true(
    all(approx_error_d[["abserror"]] == abs(approx_error_d[["error"]]))
  )

  q_norm <- as_q(qnorm)
  approx_error_q <- pdqr_approx_error(
    q_norm, qnorm, gran = 10, remove_infinity = FALSE
  )
  expect_named(approx_error_q, c("grid", "error", "abserror"))
  expect_equal(
    approx_error_q[["grid"]],
    granulate_grid(q_norm, gran = 10)
  )
  expect_true(median(approx_error_q[["abserror"]]) < 1e-4)
  expect_true(
    all(approx_error_d[["abserror"]] == abs(approx_error_d[["error"]]))
  )
})

test_that("pdqr_approx_error uses `...` as extra arguments for `ref_f`", {
  d_unif <- as_d(dunif, min = 1, max = 11)
  approx_error <- pdqr_approx_error(d_unif, dunif, min = 1, max = 11)
  expect_true(all(approx_error[["error"]] == 0))
})

test_that("pdqr_approx_error uses `gran` argument", {
  d_unif <- as_d(dunif)
  approx_error <- pdqr_approx_error(d_unif, dunif, gran = 1)
  expect_equal(approx_error[["grid"]], granulate_grid(d_unif, gran = 1))
})

test_that("pdqr_approx_error uses `remove_infinity` argument", {
  skip_if_noLD()

  d_beta <- as_d(dbeta, shape1 = 0.5, shape2 = 0.5)
  approx_error_d <- pdqr_approx_error(
    d_beta, dbeta, shape1 = 0.5, shape2 = 0.5, remove_infinity = FALSE
  )
  expect_true(is.infinite(max(approx_error_d[["abserror"]])))

  q_norm <- as_q(qnorm)
  approx_error_q <- pdqr_approx_error(q_norm, qnorm, remove_infinity = FALSE)
  expect_true(is.infinite(max(approx_error_q[["abserror"]])))
})

test_that("pdqr_approx_error validates input", {
  d_unif <- as_d(dunif)
  expect_error(pdqr_approx_error("a", dunif), "`f`.*not pdqr-function")
  expect_error(
    pdqr_approx_error(as_r(runif), runif), "`f`.*p-, d-, or q-function"
  )
  expect_error(
    pdqr_approx_error(d_unif),
    "`ref_f`.*missing.*reference distribution function"
  )
  expect_error(pdqr_approx_error(d_unif, "a"), "`ref_f`.*function")
  expect_error(pdqr_approx_error(d_unif, dunif, gran = "a"), "`gran`.*number")
  expect_error(
    pdqr_approx_error(d_unif, dunif, gran = 0.5), "`gran`.*more than 1"
  )
  expect_error(pdqr_approx_error(d_unif, dunif, gran = 1:2), "`gran`.*single")
  expect_error(
    pdqr_approx_error(d_unif, dunif, remove_infinity = "a"),
    "`remove_infinity`.*TRUE.*FALSE"
  )
})


# granulate_grid ----------------------------------------------------------
test_that("granulate_grid works", {
  cur_d <- new_d(data.frame(x = 1:3, y = c(1, 1, 1)/2), "continuous")
  expect_equal(granulate_grid(cur_d, 1), 1:3)
  expect_equal(granulate_grid(cur_d, 10), seq(1, 3, length.out = 21))

  expect_equal(granulate_grid(as_p(cur_d), 1), 1:3)
  expect_equal(granulate_grid(as_p(cur_d), 10), seq(1, 3, length.out = 21))

  expect_equal(granulate_grid(as_q(cur_d), 1), c(0, 0.5, 1))
  expect_equal(granulate_grid(as_q(cur_d), 10), seq(0, 1, length.out = 21))
})


# enpoint -----------------------------------------------------------------
test_that("enpoint works with p-functions", {
  # Type "discrete"
  x_tbl <- meta_x_tbl(p_dis)
  expect_equal(enpoint(p_dis), data.frame(x = x_tbl$x, p = x_tbl$cumprob))

  # Type "continuous"
  x_seq <- seq_between(meta_support(p_con), length.out = 3)
  expect_equal(
    enpoint(p_con, n_points = 3),
    data.frame(x = x_seq, p = p_con(x_seq))
  )
})

test_that("enpoint works with d-functions", {
  # Type "discrete"
  x_tbl <- meta_x_tbl(d_dis)
  expect_equal(enpoint(d_dis), data.frame(x = x_tbl$x, prob = x_tbl$prob))

  # Type "continuous"
  x_seq <- seq_between(meta_support(d_con), length.out = 3)
  expect_equal(
    enpoint(d_con, n_points = 3),
    data.frame(x = x_seq, y = d_con(x_seq))
  )
})

test_that("enpoint works with q-functions", {
  # Type "discrete"
  x_tbl <- meta_x_tbl(q_dis)
  expect_equal(enpoint(q_dis), data.frame(p = x_tbl$cumprob, x = x_tbl$x))

  # Type "continuous"
  p_seq <- seq_between(c(0, 1), length.out = 3)
  expect_equal(
    enpoint(q_con, n_points = 3),
    data.frame(p = p_seq, x = q_con(p_seq))
  )
})

test_that("enpoint works with r-functions", {
  set.seed(101)

  # Type "discrete"
  output <- enpoint(r_dis, n_points = 10)
  expect_named(output, c("n", "x"))
  expect_equal(output[["n"]], seq_len(10))
  expect_equal(mean(output[["x"]]), summ_mean(r_dis), tolerance = 0.9)

  # Type "continuous"
  output <- enpoint(r_con, n_points = 10)
  expect_named(output, c("n", "x"))
  expect_equal(output[["n"]], seq_len(10))
  expect_equal(mean(output[["x"]]), summ_mean(r_con), tolerance = 0.5)
})

test_that("enpoint uses `n_points` argument", {
  expect_equal(nrow(enpoint(p_con, n_points = 3)), 3)
  expect_equal(nrow(enpoint(d_con, n_points = 14)), 14)
  expect_equal(nrow(enpoint(q_con, n_points = 15)), 15)
  expect_equal(nrow(enpoint(r_dis,   n_points = 92)), 92)
  expect_equal(nrow(enpoint(r_con, n_points = 6)), 6)
})

test_that("enpoint validates input", {
  expect_error(enpoint("a"), "`f`.*not pdqr-function")
  expect_error(enpoint(d_dis, n_points = "a"), "`n_points`.*number")
  expect_error(enpoint(d_dis, n_points = 10:11), "`n_points`.*single")
  expect_error(enpoint(d_dis, n_points = 0.5), "`n_points`.*more than 1")
})


# enpoint_p ---------------------------------------------------------------
# Tested in `enpoint()`


# enpoint_d ---------------------------------------------------------------
# Tested in `enpoint()`


# enpoint_q ---------------------------------------------------------------
# Tested in `enpoint()`


# enpoint_r ---------------------------------------------------------------
# Tested in `enpoint()`
