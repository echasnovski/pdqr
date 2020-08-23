set.seed(18345)

# Helpers -----------------------------------------------------------------
curry <- function(f, ...) {
  function(t) {
    f(t, ...)
  }
}

# To use instead of `max()` in case of infinity is involved
quan999 <- function(x) {
  stats::quantile(x, probs = 0.999)
}
quan99 <- function(x) {
  stats::quantile(x, probs = 0.99)
}
quan90 <- function(x) {
  stats::quantile(x, probs = 0.90)
}


# Distributions -----------------------------------------------------------
# Standard normal. Originally unbounded support
fam_norm <- list(
  p = stats::pnorm, d = stats::dnorm, q = stats::qnorm, r = stats::rnorm,
  support = c(-10, 10), grid = seq(-10, 10, length.out = 1e5)
)

# Normal with different parameters. Unsimmetrical support.
fam_norm_2 <- list(
  p = curry(stats::pnorm, mean = 10, sd = 0.1),
  d = curry(stats::dnorm, mean = 10, sd = 0.1),
  q = curry(stats::qnorm, mean = 10, sd = 0.1),
  r = curry(stats::rnorm, mean = 10, sd = 0.1),
  support = c(9, 10.5),
  grid = seq(9, 10.5, length.out = 1e5)
)

# Exponential. Support is bounded from left and unbounded from right.
fam_exp <- list(
  p = stats::pexp, d = stats::dexp, q = stats::qexp, r = stats::rexp,
  support = c(0, 20), grid = seq(0, 20, length.out = 1e5)
)

# "Reversed" exponential. Support is unbounded from left and bounded from right.
fam_exp_rev <- list(
  p = function(q) {
    1 - stats::pexp(-q)
  },
  d = function(x) {
    stats::dexp(-x)
  },
  q = function(p) {
    -stats::qexp(1 - p)
  },
  r = function(n) {
    -stats::rexp(n)
  },
  support = c(-20, 0), grid = seq(-20, 0, length.out = 1e5)
)

# Beta distribution. Support is bounded.
fam_beta <- list(
  p = curry(stats::pbeta, shape1 = 1, shape2 = 3),
  d = curry(stats::dbeta, shape1 = 1, shape2 = 3),
  q = curry(stats::qbeta, shape1 = 1, shape2 = 3),
  r = curry(stats::rbeta, shape1 = 1, shape2 = 3),
  support = c(0, 1),
  grid = seq(0, 1, length.out = 1e5)
)

# Beta distribution. Density goes to infinity on edges.
fam_beta_inf <- list(
  p = curry(stats::pbeta, shape1 = 0.7, shape2 = 0.3),
  d = curry(stats::dbeta, shape1 = 0.7, shape2 = 0.3),
  q = curry(stats::qbeta, shape1 = 0.7, shape2 = 0.3),
  r = curry(stats::rbeta, shape1 = 0.7, shape2 = 0.3),
  support = c(0, 1),
  # Step away a little from edges where density goes to infinity
  grid = seq(0.001, 1 - 0.001, length.out = 1e5)
)

# Beta-based distribution. Has infinity density point inside support.
# Constructed from `fam_beta_inf` density by swapping halves of support.
fam_beta_midinf <- list(
  p = function(q) {
    res <- numeric(length(q))

    q_ind <- findInterval(q, c(0, 0.5, 1))
    res[q_ind == 1] <- fam_beta_inf$p(q[q_ind == 1] + 0.5) - fam_beta_inf$p(0.5)
    res[q_ind == 2] <- fam_beta_inf$p(q[q_ind == 2] - 0.5) +
      diff(fam_beta_inf$p(c(0.5, 1)))
    res[(q == 1) | (q_ind == 3)] <- 1

    res
  },
  d = function(x) {
    res <- numeric(length(x))

    x_ind <- findInterval(x, c(0, 0.5, 1))
    res[x_ind == 1] <- fam_beta_inf$d(x[x_ind == 1] + 0.5)
    res[x_ind == 2] <- fam_beta_inf$d(x[x_ind == 2] - 0.5)
    res[x == 1] <- fam_beta_inf$d(0.5)

    res
  },
  q = function(p) {
    mid_image <- diff(fam_beta_inf$p(c(0.5, 1)))
    res <- numeric(length(p))

    p_ind <- findInterval(x = p, vec = c(0, mid_image, 1))
    res[p_ind == 1] <- fam_beta_inf$q(p[p_ind == 1] + 1 - mid_image) -
      fam_beta_inf$q(1 - mid_image)
    res[p_ind == 2] <- fam_beta_inf$q(p[p_ind == 2] - mid_image) +
      diff(fam_beta_inf$q(c(1 - mid_image, 1)))
    res[p == 1] <- 1

    res
  },
  r = function(n) {
    res <- numeric(n)

    smpl <- fam_beta_inf$r(n)
    smpl_ind <- findInterval(smpl, c(0, 0.5, 1))
    res[smpl_ind == 1] <- smpl[smpl_ind == 1] + 0.5
    res[smpl_ind == 2] <- smpl[smpl_ind == 2] - 0.5
    res[smpl == 1] <- 0.5

    res
  },
  support = c(0, 1),
  # Step away a little from 0.5 where density goes to infinity
  grid = c(
    seq(0, 0.499, length.out = 0.5e5), seq(0.501, 1, length.out = 0.5e5)
  )
)

# Chi square distribution. Support is unbounded from right.
fam_chisq <- list(
  p = curry(stats::pchisq, df = 2),
  d = curry(stats::dchisq, df = 2),
  q = curry(stats::qchisq, df = 2),
  r = curry(stats::rchisq, df = 2),
  support = c(0, 37),
  grid = seq(0, 37, length.out = 1e5)
)

# Chi square distribution. Density goes to infinity on one support edge and
# the other edge is infinity (unbounded).
fam_chisq_inf <- list(
  p = curry(stats::pchisq, df = 1),
  d = curry(stats::dchisq, df = 1),
  q = curry(stats::qchisq, df = 1),
  r = curry(stats::rchisq, df = 1),
  support = c(0, 35),
  # Step away a little from left edge where density goes to infinity
  grid = seq(0.001, 35, length.out = 1e5)
)

# Mixture of two normal. Important type of bimodal distribution.
r_mix_norm <- function(n) {
  smpl_1 <- rnorm(n, mean = -3)
  smpl_2 <- rnorm(n, sd = 0.5)
  smpl_unif <- runif(n)

  ifelse(smpl_unif <= 0.1, smpl_1, smpl_2)
}
mix_norm_smpl <- r_mix_norm(1e5)
p_sequence <- seq(0, 1, length.out = 1e5)
q_seq <- quantile(mix_norm_smpl, probs = p_sequence)

fam_mix_norm <- list(
  p = function(q) {
    0.1 * pnorm(q, mean = -3) + 0.9 * pnorm(q, sd = 0.5)
  },
  d = function(x) {
    0.1 * dnorm(x, mean = -3) + 0.9 * dnorm(x, sd = 0.5)
  },
  q = stats::approxfun(p_sequence, q_seq, method = "linear"),
  r = r_mix_norm,
  support = c(-13, 10),
  grid = seq(-13, 10, length.out = 1e5)
)

# Mixture of two uniform. Has segment of exactly zero density. Also "bimodal".
# Also density is discontinuous.
fam_mix_unif <- list(
  p = function(q) {
    0.2 * punif(q) + 0.8 * punif(q, min = 2, max = 4)
  },
  d = function(x) {
    0.2 * dunif(x) + 0.8 * dunif(x, min = 2, max = 4)
  },
  q = function(p) {
    res <- numeric(length(p))
    p_ind <- findInterval(p, c(0, 0.2, 1), left.open = TRUE)
    res[p_ind == 0] <- NaN
    res[is_near(p, 0) & (p >= 0)] <- p[is_near(p, 0) & (p >= 0)] / 0.2
    res[p_ind == 1] <- p[p_ind == 1] / 0.2
    res[p_ind == 2] <- 2.5 * p[p_ind == 2] + 1.5
    res[p == 3] <- NaN

    res
  },
  r = function(n) {
    smpl_1 <- runif(n)
    smpl_2 <- runif(n, min = 2, max = 4)
    smpl_unif <- runif(n)

    ifelse(smpl_unif <= 0.2, smpl_1, smpl_2)
  },
  support = c(0, 4),
  grid = seq(0, 4, length.out = 1e5)
)

# Uniform with too wide support. Discontinuous at beginning and end.
fam_unif <- list(
  p = stats::punif, d = stats::dunif, q = stats::qunif, r = stats::runif,
  support = c(-0.5, 1.2), grid = seq(-0.5, 1.2, length.out = 1e5)
)


# List of distributions ---------------------------------------------------
fam_list <- list(
  fam_norm = fam_norm,
  fam_norm_2 = fam_norm_2,
  fam_exp = fam_exp,
  fam_exp_rev = fam_exp_rev,
  fam_beta = fam_beta,
  fam_beta_inf = fam_beta_inf,
  fam_beta_midinf = fam_beta_midinf,
  fam_chisq = fam_chisq,
  fam_chisq_inf = fam_chisq_inf,
  fam_mix_norm = fam_mix_norm,
  fam_mix_unif = fam_mix_unif,
  fam_unif = fam_unif
)
