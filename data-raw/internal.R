# Honored distributions ---------------------------------------------------
# These are all continuous distributions from 'stats' package
stats_distrs <- c(
  "beta", "cauchy", "chisq", "exp", "f", "gamma", "lnorm", "norm", "t", "unif",
  "weibull"
)

stats_distrs_vec <- rep(stats_distrs, each = 4)

stats_distr_funs <- paste0(
  rep(c("p", "d", "q", "r"), times = length(stats_distrs)),
  stats_distrs_vec
)

stats_distr_d_funs <- paste0("d", stats_distrs_vec)
stats_distr_q_funs <- paste0("q", stats_distrs_vec)

#' Data frame of honored distributions
#'
#' Contains information (in "long"-ish form) about all `p*()`, `d*()`, `q*()`,
#' `r*()` distribution functions that will receive special treatment inside
#' `as_*.default()`.
#'
#' It has the following columns:
#' - **package** <chr> - Name of a package where p-, d-, q-, r-function is
#' stored.
#' - **distr** <chr> - Name of distribution. Usually, it is a function name
#' without "pdqr" prefix.
#' - **fun** <chr> - Name of distribution function.
#' - **d_fun** <chr> - Name of d-function of the distribution.
#' - **q_fun** <chr> - Name of q-function of the distribution.
#'
#' @noRd
honored_distrs <- data.frame(
  package = "stats",
  distr   = stats_distrs_vec,
  fun     = stats_distr_funs,
  d_fun   = stats_distr_d_funs,
  q_fun   = stats_distr_q_funs,
  stringsAsFactors = FALSE
)


# Creation of internal data -----------------------------------------------
usethis::use_data(honored_distrs, internal = TRUE)
