# Input sample data -------------------------------------------------------
x_fin <- c(4, 1, 2, 6, 8, 6, 7, 3, 2, 3, 2, 6, 7, 8, 1, 9, 2, 9, 5, 7)
x_fin_x_tbl <- data.frame(
  x = as.numeric(1:9),
  prob    = c(0.1, 0.2, 0.1, 0.05, 0.05, 0.15, 0.15, 0.1, 0.1),
  cumprob = c(0.1, 0.3, 0.4, 0.45,  0.5, 0.65,  0.8, 0.9, 1)
)
x_fin_cumprobs <- cumsum(x_fin_x_tbl[["prob"]])
x_fin_support <- c(1, 9)

x_smooth <- c(
  1.47, 0.11, 0.04,  0.37, -0.43, 0.17, -0.18, 0.85, -0.05, 0.17,
  -1.27, 1.04, 1.06, -1.87, -1.71, -0.3, -0.26, 0.62,  1.42, 0.36
)
x_smooth_support <- c(-2.91865392160928, 2.51865392160928)
x_smooth_x_tbl <- density_piecelin(x_smooth)
x_smooth_x_tbl[["cumprob"]] <- trapez_part_integral(
  x_smooth_x_tbl[["x"]], x_smooth_x_tbl[["y"]]
)


# Input test vectors ------------------------------------------------------
set.seed(13579)

x_fin_vec <- sample(unique(x_fin))
x_fin_vec_seq <- sample(
  seq(x_fin_support[1] - 0.1, x_fin_support[2] + 0.1, length.out = 1000)
)
# Adding `x_fin_vec` is needed as behavior in actual values of `x_fin` is
# often important.
x_fin_vec_ext <- c(x_fin_vec_seq, x_fin_vec)

x_smooth_vec <- sample(
  seq(x_smooth_support[1], x_smooth_support[2], length.out = 1000)
)
x_smooth_vec_ext <- sample(
  seq(
    x_smooth_support[1] - 0.1, x_smooth_support[2] + 0.1, length.out = 1000
  )
)

x_custom_trunc <- runif(1000, 0.05, 0.95)

p_vec <- sample(0:1000 / 1000)

# Sequences for testing `as_q()` methods
  # **Note** that step is ~1e-4, but in `as_d()` and `as_p()` tests it is ~1e-5.
  # This is done because of slower `as_q.default()` performance.
p_seq <- seq(0, 1, length.out = 1e4)

# Sequences of probabilities used to test quantile functions that go to infinity
# on one or both edges. Corresponds to unbounded (from left and/or right)
# support, i.e. left, right, or both tails.
p_isnt_small <- p_seq >= 0.001
p_isnt_big <- p_seq <= 0.999
p_seq_ltail <- p_seq[p_isnt_small]
p_seq_rtail <- p_seq[p_isnt_big]
p_seq_btail <- p_seq[p_isnt_small & p_isnt_big]


# Constructed distribution functions --------------------------------------
custom_x_tbl <- data.frame(x = seq(0, 1, by = 0.0001))
custom_x_tbl[["y"]] <- dbeta(custom_x_tbl[["x"]], 1, 2)

# p-functions
p_fin <- new_p(x_fin, "fin")
p_smooth <- new_p(x_smooth, "smooth")
user_p <- function(q) {pbeta(q, 1, 2)}
p_custom <- new_p(custom_x_tbl)

# d-functions
d_fin <- new_d(x_fin, "fin")
d_smooth <- new_d(x_smooth, "smooth")
user_d <- function(x) {dbeta(x, 1, 2)}
d_custom <- new_d(custom_x_tbl)

# q-functions
q_fin <- new_q(x_fin, "fin")
q_smooth <- new_q(x_smooth, "smooth")
user_q <- function(p) {qbeta(p, 1, 2)}
q_custom <- new_q(custom_x_tbl)

# r-functions
r_fin <- new_r(x_fin, "fin")
r_smooth <- new_r(x_smooth, "smooth")
user_r <- function(n) {rbeta(n, 1, 2)}
r_custom <- new_r(custom_x_tbl)
