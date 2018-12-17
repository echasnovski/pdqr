# Input sample data -------------------------------------------------------
x_raw <- c(4, 1, 2, 6, 8, 6, 7, 3, 2, 3, 2, 6, 7, 8, 1, 9, 2, 9, 5, 7)
x_raw_raw_tbl <- data.frame(
  x = as.numeric(1:9),
  prob = c(0.1, 0.2, 0.1, 0.05, 0.05, 0.15, 0.15, 0.1, 0.1),
  n    = c(  2,   4,   2,    1,    1,    3,    3  , 2,   2)
)
x_raw_cumprobs <- cumsum(x_raw_raw_tbl[["prob"]])
x_raw_support <- c(1, 9)

x_smooth <- c(
  1.47, 0.11, 0.04,  0.37, -0.43, 0.17, -0.18, 0.85, -0.05, 0.17,
  -1.27, 1.04, 1.06, -1.87, -1.71, -0.3, -0.26, 0.62,  1.42, 0.36
)
x_smooth_support <- c(-2.91865392160928, 2.51865392160928)


# Input test vectors ------------------------------------------------------
set.seed(13579)

x_raw_vec <- sample(unique(x_raw))
x_raw_vec_seq <- sample(
  seq(x_raw_support[1] - 0.1, x_raw_support[2] + 0.1, length.out = 1000)
)
# Adding `x_raw_vec` is needed as behavior in actual values of `x_raw` is
# often important.
x_raw_vec_ext <- c(x_raw_vec_seq, x_raw_vec)

x_smooth_vec <- sample(
  seq(x_smooth_support[1], x_smooth_support[2], length.out = 1000)
)
x_smooth_vec_ext <- sample(
  seq(
    x_smooth_support[1] - 0.1, x_smooth_support[2] + 0.1, length.out = 1000
  )
)

x_custom <- c(runif(998), 0, 1)
# Range [0.05, 0.95] instead of [0, 1] will be useful when testing conversion
  # from "r" class. This is because custom functions have finite support [0, 1]
  # and `type = "smooth"`. Current behaviour is to extend output range a little
  # bit (consequence of using `density()`), so result on the edge of true
  # support may differ a lot.
x_custom_trunc <- runif(1000, 0.05, 0.95)
x_custom_inner <- setdiff(x_custom, c(0, 1))

p_vec <- sample(0:1000 / 1000)
p_vec_trunc <- p_vec[(p_vec >= 0.05) & (p_vec <= 0.95)]
# "Wholed" vectors of probabilities is needed for estimation tests of `as_q`,
  # as behavior around actual raw values is not precise.
p_vec_wholed <- setdiff(p_vec, c(0, x_raw_cumprobs))
p_vec_bigwholed <- Filter(function(p) {
  all(abs(p - x_raw_cumprobs) >= 0.01)
}, p_vec)


# Constructed distribution functions --------------------------------------
# Used in `as_*()` tests for adjusting to support
construct_adj_raw <- function(raw_fun, as_fun, ...) {
  res <- raw_fun
  attributes(res) <- NULL

  as_fun(res, type = "raw", support = c(2, 6), ...)
}

construct_adj_smooth <- function(smooth_fun, as_fun, ...) {
  res <- smooth_fun
  attributes(res) <- NULL

  as_fun(res, type = "smooth", support = c(0, 1), ...)
}

# p-functions
p_raw <- new_p(x_raw, "raw")
adj_p_raw <- construct_adj_raw(p_raw, as_p)

p_smooth <- new_p(x_smooth, "smooth")
adj_p_smooth <- construct_adj_smooth(p_smooth, as_p)

user_p <- function(q) {pbeta(q, 1, 2)}
p_custom <- structure(
  user_p, class = c("p", "pdqr", "function"),
  meta = list(support = c(0, 1), type = "smooth")
)

# d-functions
d_raw <- new_d(x_raw, "raw")
adj_d_raw <- expect_warning(construct_adj_raw(d_raw, as_d))

d_smooth <- new_d(x_smooth, "smooth")
adj_d_smooth <- construct_adj_smooth(d_smooth, as_d)

user_d <- function(x) {dbeta(x, 1, 2)}
d_custom <- structure(
  user_d, class = c("d", "pdqr", "function"),
  meta = list(support = c(0, 1), type = "smooth")
)

# q-functions
q_raw <- new_q(x_raw, "raw")
adj_q_raw <- construct_adj_raw(q_raw, as_q)

q_smooth <- new_q(x_smooth, "smooth")
adj_q_smooth <- construct_adj_smooth(q_smooth, as_q)

user_q <- function(p) {qbeta(p, 1, 2)}
q_custom <- structure(
  user_q, class = c("q", "pdqr", "function"),
  meta = list(support = c(0, 1), type = "smooth")
)

# r-functions
r_raw <- new_r(x_raw, "raw")
adj_r_raw <- construct_adj_raw(r_raw, as_r, warn_not_adjusted = FALSE)

r_smooth <- new_r(x_smooth, "smooth")
adj_r_smooth <- construct_adj_smooth(r_smooth, as_r, warn_not_adjusted = FALSE)

user_r <- function(n) {rbeta(n, 1, 2)}
r_custom <- structure(
  user_r, class = c("r", "pdqr", "function"),
  meta = list(support = c(0, 1), type = "smooth")
)
