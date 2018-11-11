# Input sample data -------------------------------------------------------
x_raw <- c(4, 1, 2, 6, 8, 6, 7, 3, 2, 3, 2, 6, 7, 8, 1, 9, 2, 9, 5, 7)
x_raw_distr_tbl <- data.frame(
  x = as.numeric(1:9), prob = c(0.1, 0.2, 0.1, 0.05, 0.05, 0.15, 0.15, 0.1, 0.1)
)
x_raw_cumprobs <- cumsum(x_raw_distr_tbl[["prob"]])
x_raw_support <- c(1, 9)

x_smooth <- c(
  1.47, 0.11, 0.04,  0.37, -0.43, 0.17, -0.18, 0.85, -0.05, 0.17,
  -1.27, 1.04, 1.06, -1.87, -1.71, -0.3, -0.26, 0.62,  1.42, 0.36
)
x_smooth_support <- c(-2.91875392160928, 2.51875392160928)


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
  # from `r_fun` class. This is because custom functions have finite support
  # [0, 1] and `type = "smooth"`. Current behaviour is to extend output range a
  # little bit (consequence of using `density()`), so result on the edge of true
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
p_raw <- p_fun(x_raw, "raw")
p_raw_withx <- p_fun(x_raw, "raw", attach_x = TRUE)
p_raw_nox <- p_fun(x_raw, "raw", attach_x = FALSE)

p_smooth <- p_fun(x_smooth, "smooth")
p_smooth_withx <- p_fun(x_smooth, "smooth", attach_x = TRUE)
p_smooth_nox <- p_fun(x_smooth, "smooth", attach_x = FALSE)

user_p <- function(q) {pbeta(q, 1, 2)}
p_custom <- structure(
  user_p, class = c("p_fun", "pdqr_fun", "function"),
  meta = list(support = c(0, 1), type = "smooth")
)

d_raw <- d_fun(x_raw, "raw")
d_raw_withx <- d_fun(x_raw, "raw", attach_x = TRUE)
d_raw_nox <- d_fun(x_raw, "raw", attach_x = FALSE)

d_smooth <- d_fun(x_smooth, "smooth")
d_smooth_withx <- d_fun(x_smooth, "smooth", attach_x = TRUE)
d_smooth_nox <- d_fun(x_smooth, "smooth", attach_x = FALSE)

user_d <- function(x) {dbeta(x, 1, 2)}
d_custom <- structure(
  user_d, class = c("d_fun", "pdqr_fun", "function"),
  meta = list(support = c(0, 1), type = "smooth")
)

q_raw <- q_fun(x_raw, "raw")
q_raw_withx <- q_fun(x_raw, "raw", attach_x = TRUE)
q_raw_nox <- q_fun(x_raw, "raw", attach_x = FALSE)

q_smooth <- q_fun(x_smooth, "smooth")
q_smooth_withx <- q_fun(x_smooth, "smooth", attach_x = TRUE)
q_smooth_nox <- q_fun(x_smooth, "smooth", attach_x = FALSE)

user_q <- function(p) {qbeta(p, 1, 2)}
q_custom <- structure(
  user_q, class = c("q_fun", "pdqr_fun", "function"),
  meta = list(support = c(0, 1), type = "smooth")
)

r_raw <- r_fun(x_raw, "raw")
r_raw_withx <- r_fun(x_raw, "raw", attach_x = TRUE)
r_raw_nox <- r_fun(x_raw, "raw", attach_x = FALSE)

r_smooth <- r_fun(x_smooth, "smooth")
r_smooth_withx <- r_fun(x_smooth, "smooth", attach_x = TRUE)
r_smooth_nox <- r_fun(x_smooth, "smooth", attach_x = FALSE)

user_r <- function(n) {rbeta(n, 1, 2)}
r_custom <- structure(
  user_r, class = c("r_fun", "pdqr_fun", "function"),
  meta = list(support = c(0, 1), type = "smooth")
)
