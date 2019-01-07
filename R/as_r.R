as_r <- function(f, ...) {
  UseMethod("as_r")
}

as_r.default <- function(f, support = NULL, n_grid = 10001, n_sample = 10000,
                         ...) {
  assert_as_def_args(f, support, n_grid)

  # Create sample
  smpl <- f(n_sample)
  # Detect support
  support <- detect_support_r(smpl, format_support(support))
  # Create density function from sample
  d_f <- new_d(smpl, type = "smooth", ...)
  # Adjust to supplied support
  d_f <- as_d(unpdqr(d_f), support = support, n_grid = n_grid)

  # Convert to r-function
  as_r(d_f)
}

as_r.pdqr <- function(f, ...) {
  assert_pdqr_fun(f)

  res <- new_r(x = meta(f, "x_tbl"), type = meta(f, "type"))

  # Ensure that output has maximum available support (usually equal to
  # `meta(f, "support")`)
  ensure_support(res, meta(f, "support"))
}

detect_support_r <- function(smpl, supp) {
  mean_diff <- mean(diff(sort(smpl)))
  smpl_support <- range(smpl) + mean_diff * c(-1, 1)

  supp <- coalesce_pair(supp, smpl_support)

  if (!is_support(supp) || (supp[1] == supp[2])) {
    stop_collapse(
      "Detected support isn't proper. Usually this is because input random ",
      "generation function isn't proper."
    )
  }

  supp
}
