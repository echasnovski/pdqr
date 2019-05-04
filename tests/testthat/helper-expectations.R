expect_distr_fun <- function(input, pdqr_class, type) {
  expect_true(is.function(input))
  expect_is(input, pdqr_class)
  expect_is(input, "pdqr")
  expect_named(meta_all(input), c("class", "type", "support", "x_tbl"))
  expect_equal(meta_class(input), pdqr_class)
  expect_equal(meta_type(input), type)
  expect_true(is_support(meta_support(input)))
  expect_true(is_x_tbl(meta_x_tbl(input), type = meta_type(input)))
  expect_true(is_x_tbl_meta(meta_x_tbl(input), type = meta_type(input)))
}

expect_close_f <- function(f_1, f_2, grid, stat_f = max, thres = 10^(-6)) {
  stat <- stat_f(abs(f_1(grid) - f_2(grid)))

  expect_true(stat <= thres)
}

expect_not_close_f <- function(f_1, f_2, grid, stat_f = max, thres = 10^(-6)) {
  stat <- stat_f(abs(f_1(grid) - f_2(grid)))

  expect_true(stat > thres)
}

expect_close_r_f <- function(f_1, f_2, n_sample = 10000,
                             mean_thres = 0.1, sd_thres = 0.05) {
  smpl_1 <- f_1(n_sample)
  smpl_2 <- f_2(n_sample)

  expect_true(abs(mean(smpl_1) - mean(smpl_2)) <= mean_thres)
  expect_true(abs(sd(smpl_1) - sd(smpl_2)) <= sd_thres)
}

expect_not_close_r_f <- function(f_1, f_2, n_sample = 10000,
                                 mean_thres = 0.1, sd_thres = 0.05) {
  smpl_1 <- f_1(n_sample)
  smpl_2 <- f_2(n_sample)

  expect_false(
    (abs(mean(smpl_1) - mean(smpl_2)) <= mean_thres) &&
      (abs(sd(smpl_1) - sd(smpl_2)) <= sd_thres)
  )
}

expect_equal_distr <- function(f_1, f_2, grid, thres = 10^(-8),
                               meta_not_check = character(0)) {
  expect_close_f(f_1, f_2, grid, thres = thres)
  expect_equal_meta(f_1, f_2, meta_not_check)
}

expect_equal_r_distr <- function(f_1, f_2, n_sample = 10000,
                                 mean_thres = 0.1, sd_thres = 0.05,
                                 meta_not_check = character(0)) {
  expect_close_r_f(f_1, f_2, n_sample, mean_thres, sd_thres)
  expect_equal_meta(f_1, f_2, meta_not_check)
}

expect_equal_stat <- function(summ_fun, stat_data, entry, thres = 1e-6, ...) {
  summ_stat <- summ_fun(stat_data[["d_fun"]], ...)

  expect_equal(summ_stat, stat_data[[entry]], tolerance = thres)
}

expect_equal_meta <- function(f_1, f_2, meta_not_check = character(0)) {
  expect_equal(class(f_1), class(f_2))

  meta_1 <- meta_all(f_1)
  meta_2 <- meta_all(f_2)
  meta_names_1 <- setdiff(names(meta_1), meta_not_check)
  meta_names_2 <- setdiff(names(meta_2), meta_not_check)
  expect_equal(meta_1[meta_names_1], meta_2[meta_names_2])
}

expect_x_tbl_imputation <- function(f) {
  # Type "fin"
  n_fin <- nrow(x_fin_x_tbl)

    # Reordering rows
  bad_fin_input_1 <- x_fin_x_tbl[n_fin:1, ]
  output_1 <- f(bad_fin_input_1, "fin")
  expect_equal(meta_x_tbl(output_1), x_fin_x_tbl)

    # Reordering columns
  bad_fin_input_2 <- x_fin_x_tbl[, c("cumprob", "x", "prob")]
  output_2 <- f(bad_fin_input_2, "fin")
  expect_equal(meta_x_tbl(output_2), x_fin_x_tbl)

    # Normalising "prob" column
  bad_fin_input_3 <- x_fin_x_tbl
  bad_fin_input_3[["prob"]] <- bad_fin_input_3[["prob"]] * 10
  output_3 <- f(bad_fin_input_3, "fin")
  expect_equal(meta_x_tbl(output_3), x_fin_x_tbl)

    # Recomputing "cumprob" column
  bad_fin_input_4 <- x_fin_x_tbl
  bad_fin_input_4[["cumprob"]] <- bad_fin_input_4[["cumprob"]] * 10
  output_4 <- f(bad_fin_input_4, "fin")
  expect_equal(meta_x_tbl(output_4), x_fin_x_tbl)

    # Collapsing duplicate `x` values
  bad_fin_input_5 <- data.frame(x = c(1, -2, 1, 3), prob = c(0, 0.6, 0.3, 0.1))
  output_5 <- f(bad_fin_input_5, "fin")
  expect_equal(
    meta_x_tbl(output_5),
    data.frame(
      x = c(-2, 1, 3), prob = c(0.6, 0.3, 0.1), cumprob = c(0.6, 0.9, 1)
    )
  )

  # Type "infin"
  n_infin <- nrow(x_infin_x_tbl)

    # Reordering rows
  bad_infin_input_1 <- x_infin_x_tbl[n_infin:1, ]
  output_1 <- new_d(bad_infin_input_1, "infin")
  expect_equal(meta_x_tbl(output_1), x_infin_x_tbl)

    # Reordering columns
  bad_infin_input_2 <- x_infin_x_tbl[, c("y", "x")]
  output_2 <- f(bad_infin_input_2, "infin")
  expect_equal(meta_x_tbl(output_2), x_infin_x_tbl)

    # Normalizing
  bad_infin_input_3 <- x_infin_x_tbl
  bad_infin_input_3[["y"]] <- bad_infin_input_3[["y"]] * 10
  output_3 <- new_d(bad_infin_input_3, "infin")
  expect_equal(meta_x_tbl(output_3), x_infin_x_tbl)

    # Recomputing "cumprob" column
  bad_infin_input_4 <- x_infin_x_tbl
  bad_infin_input_4[["cumprob"]] <- bad_infin_input_4[["cumprob"]] * 10
  output_4 <- f(bad_infin_input_4, "infin")
  expect_equal(meta_x_tbl(output_4), x_infin_x_tbl)
}

expect_ref_x_tbl <- function(f, x_tbl) {
  expect_equal(meta_x_tbl(f)[, 1:2], x_tbl[, 1:2])
}

expect_equal_x_tbl <- function(f_1, f_2) {
  expect_equal(meta_x_tbl(f_1), meta_x_tbl(f_2))
}

expect_pdqr_print <- function(f, fin_name, infin_name = fin_name) {
  supp_regex <- "Support: ~*\\[[-0-9\\.]+, [-0-9\\.]+\\]"

  f_fin <- f(x_fin, type = "fin")
  n_fin <- length(unique(x_fin))
  expect_output(
    print(f_fin),
    regex_scatter(fin_name, "finite number", supp_regex, n_fin, "elements")
  )

  f_infin <- f(x_infin, type = "infin")
  n_infin <- nrow(x_infin_x_tbl)
  expect_output(
    print(f_infin),
    regex_scatter(
      infin_name, "infinite number", supp_regex, n_infin-1, "intervals"
    )
  )

  # Test of approximation sign in support printing
  f_pi_1 <- f(data.frame(x = c(0, pi), y = c(1, 1)/pi), "infin")
  expect_output(print(f_pi_1), "Support: ~\\[0, ")
  f_pi_2 <- f(data.frame(x = c(pi, 4), y = c(1, 1)/(4-pi)), "infin")
  expect_output(print(f_pi_2), "Support: ~\\[3\\.14159, ")
  f_pi_3 <- f(data.frame(x = pi + 0:1, y = c(1, 1)), "infin")
  expect_output(print(f_pi_3), "Support: ~\\[3\\.14159, ")

  f_pi_4 <- f(c(0, pi), "fin")
  expect_output(print(f_pi_4), "Support: ~\\[0, ")
  f_pi_5 <- f(c(pi, 4), "fin")
  expect_output(print(f_pi_5), "Support: ~\\[3\\.14159, ")
  f_pi_6 <- f(pi + 0:1, "fin")
  expect_output(print(f_pi_6), "Support: ~\\[3\\.14159, ")

  # Special printing of "boolean" pdqr-functions
  f_bool_1 <- f(data.frame(x = c(0, 1), prob = c(0.7, 0.3)), "fin")
  expect_output(
    print(f_bool_1),
    regex_scatter(
      fin_name, "finite number",
      "Support: \\[0, 1\\] \\(2 elements, probability of 1: 0.3\\)"
    )
  )
  f_bool_2 <- f(data.frame(x = c(0, 1), prob = c(1, 0)), "fin")
  expect_output(
    print(f_bool_2), "probability of 1: 0.0"
  )
  f_bool_3 <- f(data.frame(x = c(0, 1), prob = c(0, 1)), "fin")
  expect_output(
    print(f_bool_3), "probability of 1: 1.0"
  )
  # Rounding
  f_bool_4 <- f(data.frame(x = c(0, 1), prob = c(1/3, 2/3)), "fin")
  expect_output(
    print(f_bool_4), "probability of 1: ~0.66667"
  )
}

regex_scatter <- function(...) {
  paste0(c(...), collapse = ".*")
}
