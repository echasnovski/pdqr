summ_moment <- function(f, order, central = FALSE, standard = FALSE,
                        absolute = FALSE) {
  assert_pdqr_fun(f)
  assert_type(
    order, is_single_number,
    type_name = "single non-negative number", min_val = 0
  )
  assert_type(central, is_truefalse, "`TRUE` or `FALSE`")
  assert_type(absolute, is_truefalse, "`TRUE` or `FALSE`")
  assert_type(standard, is_truefalse, "`TRUE` or `FALSE`")

  if (central) {
    f <- f - summ_mean(f)
  }

  if (standard) {
    f_sd <- summ_sd(f)
    if (f_sd == 0) {
      return(Inf)
    }

    # This leverages the fact that `sd(f) = sd(f - m)` for any `m`.
    f <- f / f_sd
  }

  if (absolute) {
    f <- abs(f)
  }

  raw_moment(f, order)
}

summ_skewness <- function(f) {
  summ_moment(f, order = 3, central = TRUE, standard = TRUE)
}

summ_kurtosis <- function(f, excess = TRUE) {
  assert_type(excess, is_truefalse, "`TRUE` or `FALSE`")

  res <- summ_moment(f, order = 4, central = TRUE, standard = TRUE)

  if (excess) {
    res <- res - 3
  }

  res
}
