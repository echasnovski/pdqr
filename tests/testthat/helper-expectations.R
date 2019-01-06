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
  # Type "raw"
  n_raw <- nrow(x_raw_x_tbl)

    # Reordering rows
  bad_raw_input_1 <- x_raw_x_tbl[n_raw:1, ]
  output_1 <- f(bad_raw_input_1, "raw")
  expect_equal(meta(output_1, "x_tbl"), x_raw_x_tbl)

    # Reordering columns
  bad_raw_input_2 <- x_raw_x_tbl[, c("cumprob", "x", "prob")]
  output_2 <- f(bad_raw_input_2, "raw")
  expect_equal(meta(output_2, "x_tbl"), x_raw_x_tbl)

    # Normalising "prob" column
  bad_raw_input_3 <- x_raw_x_tbl
  bad_raw_input_3[["prob"]] <- bad_raw_input_3[["prob"]] * 10
  output_3 <- f(bad_raw_input_3, "raw")
  expect_equal(meta(output_3, "x_tbl"), x_raw_x_tbl)

    # Recomputing "cumprob" column
  bad_raw_input_4 <- x_raw_x_tbl
  bad_raw_input_4[["cumprob"]] <- bad_raw_input_4[["cumprob"]] * 10
  output_4 <- f(bad_raw_input_4, "raw")
  expect_equal(meta(output_4, "x_tbl"), x_raw_x_tbl)

  # Type "smooth"
  n_smooth <- nrow(x_smooth_x_tbl)

    # Reordering rows
  bad_smooth_input_1 <- x_smooth_x_tbl[n_smooth:1, ]
  output_1 <- new_d(bad_smooth_input_1, "smooth")
  expect_equal(meta(output_1, "x_tbl"), x_smooth_x_tbl)

    # Reordering columns
  bad_smooth_input_2 <- x_smooth_x_tbl[, c("y", "x")]
  output_2 <- f(bad_smooth_input_2, "smooth")
  expect_equal(meta(output_2, "x_tbl"), x_smooth_x_tbl)

    # Normalizing
  bad_smooth_input_3 <- x_smooth_x_tbl
  bad_smooth_input_3[["y"]] <- bad_smooth_input_3[["y"]] * 10
  output_3 <- new_d(bad_smooth_input_3, "smooth")
  expect_equal(meta(output_3, "x_tbl"), x_smooth_x_tbl)

    # Recomputing "cumprob" column
  bad_smooth_input_4 <- x_smooth_x_tbl
  bad_smooth_input_4[["cumprob"]] <- bad_smooth_input_4[["cumprob"]] * 10
  output_4 <- f(bad_smooth_input_4, "smooth")
  expect_equal(meta(output_4, "x_tbl"), x_smooth_x_tbl)
}

expect_pdqr_print <- function(f, f_name) {
  supp_regex <- "Support: \\[[-0-9\\.]+, [-0-9\\.]+\\]"

  f_raw <- f(x_raw, type = "raw")
  expect_output(
    print(f_raw),
    regex_scatter(f_name, "raw type", supp_regex)
  )

  f_smooth <- f(x_smooth, type = "smooth")
  expect_output(
    print(f_smooth),
    regex_scatter(f_name, "smooth type", supp_regex)
  )
}

regex_scatter <- function(...) {
  paste0(c(...), collapse = ".*")
}
