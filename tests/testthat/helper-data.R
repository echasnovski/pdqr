# Input sample data -------------------------------------------------------
x_raw <- c(4, 1, 2, 6, 8, 6, 7, 3, 2, 3, 2, 6, 7, 8, 1, 9, 2, 9, 5, 7)
x_raw_distr_tbl <- data.frame(
  x = as.numeric(1:9), prob = c(0.1, 0.2, 0.1, 0.05, 0.05, 0.15, 0.15, 0.1, 0.1)
)
x_raw_domain_in <- c(1, 9)

x_smooth <- c(
  1.47, 0.11, 0.04,  0.37, -0.43, 0.17, -0.18, 0.85, -0.05, 0.17,
  -1.27, 1.04, 1.06, -1.87, -1.71, -0.3, -0.26, 0.62,  1.42, 0.36
)
x_smooth_domain_in <- c(-2.91875392160928, 2.51875392160928)


# Input test vectors ------------------------------------------------------
set.seed(13579)

x_smooth_vec <- sample(
  seq(x_smooth_domain_in[1], x_smooth_domain_in[2], length.out = 1000)
)
x_smooth_vec_ext <- sample(
  seq(
    x_smooth_domain_in[1] - 0.1, x_smooth_domain_in[2] + 0.1, length.out = 1000
  )
)

x_raw_vec <- sample(unique(x_raw))
x_raw_vec_ext <- sample(
  seq(x_raw_domain_in[1] - 0.1, x_raw_domain_in[2] + 0.1, length.out = 1000)
)

p_vec <- sample(0:1000 / 1000)


# Constructed distribution functions --------------------------------------
p_raw <- p_fun(x_raw, "raw")
p_raw_withx <- p_fun(x_raw, "raw", attach_x = TRUE)
p_raw_nox <- p_fun(x_raw, "raw", attach_x = FALSE)

p_smooth <- p_fun(x_smooth, "smooth")
p_smooth_withx <- p_fun(x_smooth, "smooth", attach_x = TRUE)
p_smooth_nox <- p_fun(x_smooth, "smooth", attach_x = FALSE)

d_raw <- d_fun(x_raw, "raw")
d_raw_withx <- d_fun(x_raw, "raw", attach_x = TRUE)
d_raw_nox <- d_fun(x_raw, "raw", attach_x = FALSE)

d_smooth <- d_fun(x_smooth, "smooth")
d_smooth_withx <- d_fun(x_smooth, "smooth", attach_x = TRUE)
d_smooth_nox <- d_fun(x_smooth, "smooth", attach_x = FALSE)

q_raw <- q_fun(x_raw, "raw")
q_raw_withx <- q_fun(x_raw, "raw", attach_x = TRUE)
q_raw_nox <- q_fun(x_raw, "raw", attach_x = FALSE)

q_smooth <- q_fun(x_smooth, "smooth")
q_smooth_withx <- q_fun(x_smooth, "smooth", attach_x = TRUE)
q_smooth_nox <- q_fun(x_smooth, "smooth", attach_x = FALSE)

r_raw <- r_fun(x_raw, "raw")
r_raw_withx <- r_fun(x_raw, "raw", attach_x = TRUE)
r_raw_nox <- r_fun(x_raw, "raw", attach_x = FALSE)

r_smooth <- r_fun(x_smooth, "smooth")
r_smooth_withx <- r_fun(x_smooth, "smooth", attach_x = TRUE)
r_smooth_nox <- r_fun(x_smooth, "smooth", attach_x = FALSE)
