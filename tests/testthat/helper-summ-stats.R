# Single distribution stats -----------------------------------------------
# "fin" functions
stat_binom <- list(
  d_fun = as_d(stats::dbinom, size = 10, prob = 0.3),
  mean = 3, median = 3, mode = 3,
  var = 2.1, sd = sqrt(2.1),
  skewness = 0.4/sqrt(2.1), ex_kurtosis = -0.26/2.1,
  entropy = 0.5*log(2*pi*exp(1) * 2.1)
)

stat_pois <- list(
  d_fun = as_d(stats::dpois, lambda = 5),
  mean = 5, median = stats::qpois(0.5, lambda = 5), mode = 4,
  var = 5, sd = sqrt(5),
  skewness = 1/sqrt(5), ex_kurtosis = 1/5,
  entropy = 0.5*log(2*pi*exp(1) * 5) - 1/60 - 1/600 - 19/45000
)

# "infin" functions

# Beta stats
# # Variance
# a*b / ((a+b)^2 * (a+b+1))
# # Skewness
# 2 * (b-a) * sqrt(a+b+1) / ((a+b+2)*sqrt(a*b))
# # Ex. kurtosis
# 6 * ((a-b)^2*(a+b+1) - a*b*(a+b+2)) / (a*b*(a+b+2)*(a+b+3))
# # Entropy
# log(beta(a, b)) - (a-1)*psigamma(a) - (b-1)*psigamma(b) +
#   (a+b-2)*psigamma(a+b)
stat_beta <- list(
  d_fun = as_d(stats::dbeta, shape1 = 1, shape2 = 3),
  mean = 0.25, median = stats::qbeta(0.5, 1, 3), mode = 0,
  var = 3/80, sd = sqrt(3/80),
  skewness = 4*sqrt(5) / (6*sqrt(3)), ex_kurtosis = 2 / 21,
  entropy = log(beta(1, 3)) - 2*psigamma(3) + 2*psigamma(4)
)

stat_beta_inf <- list(
  d_fun = as_d(stats::dbeta, shape1 = 0.7, shape2 = 0.3),
  mean = 0.7, median = stats::qbeta(0.5, 0.7, 0.3), mode = 1,
  var = 0.105, sd = sqrt(0.105),
  skewness = -0.8*sqrt(2) / (3*sqrt(0.21)), ex_kurtosis = -1.86/2.52,
  entropy = log(beta(0.7, 0.3)) + 0.3*psigamma(0.7) + 0.7*psigamma(0.3) -
    psigamma(1)
)

stat_chisq <- list(
  d_fun = as_d(stats::dchisq, df = 2),
  mean = 2, median = stats::qchisq(0.5, 2), mode = 0,
  var = 4, sd = 2,
  skewness = 2, ex_kurtosis = 6, entropy = 1 + log(2)
)

stat_chisq_inf <- list(
  d_fun = as_d(stats::dchisq, df = 1),
  mean = 1, median = stats::qchisq(0.5, 1), mode = 0,
  var = 2, sd = sqrt(2),
  skewness = sqrt(8), ex_kurtosis = 12, entropy = 0.5 + log(2*gamma(0.5)) +
    0.5*psigamma(0.5)
)

stat_exp <- list(
  d_fun = as_d(stats::dexp),
  mean = 1, median = log(2), mode = 0,
  var = 1, sd = 1,
  skewness = 2, ex_kurtosis = 6, entropy = 1
)

stat_norm <- list(
  d_fun = as_d(stats::dnorm),
  mean = 0, median = 0, mode = 0,
  var = 1, sd = 1,
  skewness = 0, ex_kurtosis = 0, entropy = 0.5*log(2*pi*exp(1))
)

stat_norm_2 <- list(
  d_fun = as_d(stats::dnorm, mean = 10, sd = 0.1),
  mean = 10, median = 10, mode = 10,
  var = 0.01, sd = 0.1,
  skewness = 0, ex_kurtosis = 0, entropy = 0.5*log(2*pi*exp(1)*0.01)
)

stat_unif <- list(
  d_fun = as_d(stats::dunif),
  mean = 0.5, median = 0.5, mode = 0,
  var = 1/12, sd = sqrt(1/12),
  skewness = 0, ex_kurtosis = -6/5, entropy = 0
)

stat_list <- list(
  # "fin"
  binom = stat_binom,
  pois = stat_pois,
  # "infin"
  beta = stat_beta,
  beta_inf = stat_beta_inf,
  chisq = stat_chisq,
  chisq_inf = stat_chisq_inf,
  exp = stat_exp,
  norm = stat_norm,
  norm_2 = stat_norm_2,
  unif = stat_unif
)