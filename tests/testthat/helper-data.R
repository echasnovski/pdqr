# Input sample data -------------------------------------------------------
x_dis <- c(4, 1, 2, 6, 8, 6, 7, 3, 2, 3, 2, 6, 7, 8, 1, 9, 2, 9, 5, 7)
x_dis_x_tbl <- data.frame(
  x = as.numeric(1:9),
  prob    = c(0.1, 0.2, 0.1, 0.05, 0.05, 0.15, 0.15, 0.1, 0.1),
  cumprob = c(0.1, 0.3, 0.4, 0.45,  0.5, 0.65,  0.8, 0.9, 1)
)
x_dis_cumprobs <- cumsum(x_dis_x_tbl[["prob"]])
x_dis_support <- c(1, 9)

x_con <- c(
  1.47, 0.11, 0.04,  0.37, -0.43, 0.17, -0.18, 0.85, -0.05, 0.17,
  -1.27, 1.04, 1.06, -1.87, -1.71, -0.3, -0.26, 0.62,  1.42, 0.36
)
x_con_support <- c(-2.91865392160928, 2.51865392160928)
x_con_x_tbl <- density_piecelin(x_con)
x_con_x_tbl[["cumprob"]] <- trapez_part_integral(
  x_con_x_tbl[["x"]], x_con_x_tbl[["y"]]
)


# Input test vectors ------------------------------------------------------
set.seed(13579)

x_dis_vec <- sample(unique(x_dis))
x_dis_vec_seq <- sample(
  seq(x_dis_support[1] - 0.1, x_dis_support[2] + 0.1, length.out = 1000)
)
# Adding `x_dis_vec` is needed as behavior in actual values of `x_dis` is
# often important.
x_dis_vec_ext <- c(x_dis_vec_seq, x_dis_vec)

x_con_vec <- sample(
  seq(x_con_support[1], x_con_support[2], length.out = 1000)
)
x_con_vec_ext <- sample(
  seq(
    x_con_support[1] - 0.1, x_con_support[2] + 0.1, length.out = 1000
  )
)

x_custom_trunc <- runif(1000, 0.05, 0.95)

p_vec <- sample(0:1000 / 1000)

# Sequences for testing `as_q()` methods
## **Note** that step is ~1e-4, but in `as_d()` and `as_p()` tests it is ~1e-5.
## This is done because of slower `as_q.default()` performance.
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

create_user_pdqr <- function(custom_pdqr, ...) {
  function(t) {
    custom_pdqr(t, ...)
  }
}

# p-functions
p_dis <- new_p(x_dis, "discrete")
p_con <- new_p(x_con, "continuous")
user_p <- create_user_pdqr(pbeta, 1, 2)
p_custom <- new_p(custom_x_tbl, "continuous")

# d-functions
d_dis <- new_d(x_dis, "discrete")
d_con <- new_d(x_con, "continuous")
user_d <- create_user_pdqr(dbeta, 1, 2)
d_custom <- new_d(custom_x_tbl, "continuous")

# q-functions
q_dis <- new_q(x_dis, "discrete")
q_con <- new_q(x_con, "continuous")
user_q <- create_user_pdqr(qbeta, 1, 2)
q_custom <- new_q(custom_x_tbl, "continuous")

# r-functions
r_dis <- new_r(x_dis, "discrete")
r_con <- new_r(x_con, "continuous")
user_r <- create_user_pdqr(rbeta, 1, 2)
r_custom <- new_r(custom_x_tbl, "continuous")
