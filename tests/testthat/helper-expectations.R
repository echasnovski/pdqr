expect_distr_fun <- function(input, distr_type, type) {
  expect_true(is.function(input))
  expect_is(input, distr_type)
  expect_is(input, "pdqr")
  expect_named(meta(input), c("support", "type", "x_tbl"))
  expect_equal(meta(input, "type"), type)
  expect_true(is_support(meta(input, "support")))
  expect_true(is_x_tbl(meta(input, "x_tbl"), type = meta(input, "type")))
  expect_true(is_x_tbl_meta(meta(input, "x_tbl"), type = meta(input, "type")))
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
  expect_equal_pdqr_attrs(f_1, f_2, meta_not_check)
}

expect_equal_r_distr <- function(f_1, f_2, n_sample = 10000,
                                 mean_thres = 0.1, sd_thres = 0.05,
                                 meta_not_check = character(0)) {
  expect_close_r_f(f_1, f_2, n_sample, mean_thres, sd_thres)
  expect_equal_pdqr_attrs(f_1, f_2, meta_not_check)
}

expect_equal_pdqr_attrs <- function(f_1, f_2, meta_not_check = character(0)) {
  expect_equal(class(f_1), class(f_2))

  meta_names_1 <- setdiff(names(meta(f_1)), meta_not_check)
  meta_names_2 <- setdiff(names(meta(f_2)), meta_not_check)
  expect_equal(
    lapply(meta_names_1, meta, obj = f_1),
    lapply(meta_names_2, meta, obj = f_2)
  )
}

expect_x_tbl_imputation <- function(f) {
  # Type "fin"
  n_fin <- nrow(x_fin_x_tbl)

    # Reordering rows
  bad_fin_input_1 <- x_fin_x_tbl[n_fin:1, ]
  output_1 <- f(bad_fin_input_1, "fin")
  expect_equal(meta(output_1, "x_tbl"), x_fin_x_tbl)

    # Reordering columns
  bad_fin_input_2 <- x_fin_x_tbl[, c("cumprob", "x", "prob")]
  output_2 <- f(bad_fin_input_2, "fin")
  expect_equal(meta(output_2, "x_tbl"), x_fin_x_tbl)

    # Normalising "prob" column
  bad_fin_input_3 <- x_fin_x_tbl
  bad_fin_input_3[["prob"]] <- bad_fin_input_3[["prob"]] * 10
  output_3 <- f(bad_fin_input_3, "fin")
  expect_equal(meta(output_3, "x_tbl"), x_fin_x_tbl)

    # Recomputing "cumprob" column
  bad_fin_input_4 <- x_fin_x_tbl
  bad_fin_input_4[["cumprob"]] <- bad_fin_input_4[["cumprob"]] * 10
  output_4 <- f(bad_fin_input_4, "fin")
  expect_equal(meta(output_4, "x_tbl"), x_fin_x_tbl)

  # Type "infin"
  n_infin <- nrow(x_infin_x_tbl)

    # Reordering rows
  bad_infin_input_1 <- x_infin_x_tbl[n_infin:1, ]
  output_1 <- new_d(bad_infin_input_1, "infin")
  expect_equal(meta(output_1, "x_tbl"), x_infin_x_tbl)

    # Reordering columns
  bad_infin_input_2 <- x_infin_x_tbl[, c("y", "x")]
  output_2 <- f(bad_infin_input_2, "infin")
  expect_equal(meta(output_2, "x_tbl"), x_infin_x_tbl)

    # Normalizing
  bad_infin_input_3 <- x_infin_x_tbl
  bad_infin_input_3[["y"]] <- bad_infin_input_3[["y"]] * 10
  output_3 <- new_d(bad_infin_input_3, "infin")
  expect_equal(meta(output_3, "x_tbl"), x_infin_x_tbl)

    # Recomputing "cumprob" column
  bad_infin_input_4 <- x_infin_x_tbl
  bad_infin_input_4[["cumprob"]] <- bad_infin_input_4[["cumprob"]] * 10
  output_4 <- f(bad_infin_input_4, "infin")
  expect_equal(meta(output_4, "x_tbl"), x_infin_x_tbl)
}

expect_pdqr_print <- function(f, fin_name, infin_name = fin_name) {
  supp_regex <- "Support: \\[[-0-9\\.]+, [-0-9\\.]+\\]"

  f_fin <- f(x_fin, type = "fin")
  n_fin <- length(unique(x_fin))
  expect_output(
    print(f_fin),
    regex_scatter(fin_name, "finite number", supp_regex, n_fin, "element")
  )

  f_infin <- f(x_infin, type = "infin")
  expect_output(
    print(f_infin),
    regex_scatter(infin_name, "infinite number", supp_regex)
  )
}

regex_scatter <- function(...) {
  paste0(c(...), collapse = ".*")
}
