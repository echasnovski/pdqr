context("test-summ_classmetric")


# Input data --------------------------------------------------------------
f_dis <- new_d(seq(1.5, 4.5, by = 1), "discrete")
g_dis <- new_d(seq(1.2, 2.1, by = 0.1), "discrete")
f_con <- new_d(data.frame(x = c(0.5, 2.5), y = c(1, 1) / 2), "continuous")
g_con <- new_d(data.frame(x = c(0, 1), y = c(1, 1)), "continuous")

dis_threshold <- c(1, 1.5, 1.95, 3, 5)
mixed_threshold <- c(0, 1, 1.75, 3, 5)
con_threshold <- c(-1, 0.25, 0.75, 1.5, 3)

# These values are also tests for classification rule: "negative" if "<="
# threshold, "positive" otherwise.
dis_classmetric_df <- data.frame(
  threshold = dis_threshold,

  # styler: off
  TPR = c(1, 0.6,  0.2,  0,   0),
  TNR = c(0, 0.25, 0.25, 0.5, 1),
  FPR = c(1, 0.75, 0.75, 0.5, 0),
  FNR = c(0, 0.4,  0.8,  1,   1),

  PPV = c(1/2, 0.6/1.35,  0.2/0.95,  0/0.5,   0/0),
  NPV = c(0/0, 0.25/0.65, 0.25/1.05, 0.5/1.5, 1/2),
  FDR = c(1/2, 0.75/1.35, 0.75/0.95, 0.5/0.5, 0/0),
  FOR = c(0/0, 0.4/0.65,  0.8/1.05,  1/1.5,   1/2),

  `LR+` = c(1/1, 0.6/0.75, 0.2/0.75, 0/0.5, 0/0),
  `LR-` = c(0/0, 0.4/0.25, 0.8/0.25, 1/0.5, 1/1),

  Acc = c(1+0, 0.6+0.25, 0.2+0.25, 0+0.5, 0+1) / 2,
  ER  = c(1+0, 0.75+0.4, 0.75+0.8, 0.5+1, 0+1) / 2,

  GM = sqrt(c(1*0, 0.6*0.25, 0.2*0.25, 0*0.5, 0*1)),
  F1 = c(2/3, 1.2/2.35, 0.4/1.95, 0/1.5, 0/1),
  OP = c(0.5-1/1, 0.425-0.35/0.85, 0.225-0.05/0.45, 0.25-0.5/0.5, 0.5-1/1),
  MCC = c(NaN, -0.160128153805087, -0.550688791753935, -0.577350269189626, NaN),
  YI = c(1+0, 0.6+0.25, 0.2+0.25, 0+0.5, 0+1) - 1,
  MK = c(NaN, 0.6/1.35+0.25/0.65, 0.2/0.95+0.25/1.05, 0+0.5/1.5, NaN) - 1,
  Jaccard = c(1/2, 0.6/1.75, 0.2/1.75, 0/1.5, 0/1),
  DOR = c(NaN, (0.6/0.75)/(0.4/0.25), (0.2/0.75)/(0.8/0.25), 0, NaN),
  # styler: on

  check.names = FALSE
)

mixed_classmetric_df <- data.frame(
  threshold = mixed_threshold,

  # styler: off
  TPR = c(1, 0.75, 0.375, 0,   0),
  TNR = c(0, 0,    0.25,  0.5, 1),
  FPR = c(1, 1,    0.75,  0.5, 0),
  FNR = c(0, 0.25, 0.625, 1,   1),

  PPV = c(1/2, 0.75/1.75, 0.375/1.125, 0/0.5,   0/0),
  NPV = c(0/0, 0/0.25,    0.25/0.875,  0.5/1.5, 1/2),
  FDR = c(1/2, 1/1.75,    0.75/1.125,  0.5/0.5, 0/0),
  FOR = c(0/0, 0.25/0.25, 0.625/0.875, 1/1.5,   1/2),

  `LR+` = c(1/1, 0.75/1, 0.375/0.75, 0/0.5, 0/0),
  `LR-` = c(0/0, 0.25/0, 0.625/0.25, 1/0.5, 1/1),

  Acc = c(1+0, 0.75+0, 0.375+0.25, 0+0.5, 0+1) / 2,
  ER  = c(1+0, 1+0.25, 0.75+0.625, 0.5+1, 0+1) / 2,

  GM = sqrt(c(1*0, 0.75*0, 0.375*0.25, 0*0.5, 0*1)),
  F1 = c(2/3, 1.5/2.75, 0.75/2.125, 0/1.5, 0/1),
  OP = c(0.5-1/1, 0.375-0.75/0.75, 0.3125 - 0.125/0.625, 0.25-0.5/0.5, 0.5-1/1),
  MCC = c(NaN, -0.377964473009227, -0.377964473009227, -0.577350269189626, NaN),
  YI = c(1+0, 0.75+0, 0.375+0.25, 0+0.5, 0+1) - 1,
  MK = c(NaN, 0.75/1.75+0, 0.375/1.125+0.25/0.875, 0+0.5/1.5, NaN) - 1,
  Jaccard = c(1/2, 0.75/2, 0.375/1.75, 0/1.5, 0/1),
  DOR = c(NaN, 0.75/Inf, (0.375/0.75)/(0.625/0.25), 0/(1/0.5), NaN),
  # styler: on

  check.names = FALSE
)

con_classmetric_df <- data.frame(
  threshold = con_threshold,

  # styler: off
  TPR = c(1, 0.75, 0.25,  0,   0),
  TNR = c(0, 0,    0.125, 0.5, 1),
  FPR = c(1, 1,    0.875, 0.5, 0),
  FNR = c(0, 0.25, 0.75,  1,   1),

  PPV = c(1/2, 0.75/1.75, 0.25/1.125,  0/0.5,   0/0),
  NPV = c(0/0, 0/0.25,    0.125/0.875, 0.5/1.5, 1/2),
  FDR = c(1/2, 1/1.75,    0.875/1.125, 0.5/0.5, 0/0),
  FOR = c(0/0, 0.25/0.25, 0.75/0.875,  1/1.5,   1/2),

  `LR+` = c(1/1, 0.75/1, 0.25/0.875, 0/0.5, 0/0),
  `LR-` = c(0/0, 0.25/0, 0.75/0.125, 1/0.5, 1/1),

  Acc = c(1+0, 0.75+0, 0.25+0.125, 0+0.5, 0+1) / 2,
  ER  = c(1+0, 1+0.25, 0.875+0.75, 0.5+1, 0+1) / 2,

  GM = sqrt(c(1*0, 0.75*0, 0.25*0.125, 0*0.5, 0*1)),
  F1 = c(2/3, 1.5/2.75, 0.5/2.125, 0/1.5, 0/1),
  OP = c(0.5-1/1, 0.375-0.75/0.75, 0.1875-0.125/0.375, 0.25-0.5/0.5, 0.5-1/1),
  MCC = c(NaN, -0.377964473009227, -0.629940788348712, -0.577350269189626, NaN),
  YI = c(1+0, 0.75+0, 0.25+0.125, 0+0.5, 0+1) - 1,
  MK = c(NaN, 0.75/1.75+0, 0.25/1.125+0.125/0.875, 0+0.5/1.5, NaN) - 1,
  Jaccard = c(1/2, 0.75/2, 0.25/1.875, 0/1.5, 0/1),
  DOR = c(NaN, 0.75/Inf, (0.25/0.875)/(0.75/0.125), 0/(1/0.5), NaN),
  # styler: on

  check.names = FALSE
)


# Custom expectations -----------------------------------------------------
expect_classmetric <- function(f, g, ref_df) {
  metric_names <- setdiff(colnames(ref_df), "threshold")
  for (meth in metric_names) {
    expect_equal(
      summ_classmetric(f, g, ref_df[["threshold"]], method = meth),
      ref_df[[meth]]
    )
  }
}


# summ_classmetric --------------------------------------------------------
test_that("summ_classmetric works", {
  # Skip check on "noLD" platform due to complexity of accuracy checking
  # Don't use `skip_if()` because otherwise CRAN doesn't accept submission
  if (is_noLD()) {
    expect_true(TRUE)
    return()
  }

  expect_classmetric(f_dis, g_dis, dis_classmetric_df)
  expect_classmetric(f_dis, f_con, mixed_classmetric_df)
  expect_classmetric(f_con, g_con, con_classmetric_df)
})

test_that("summ_classmetric respects method aliases", {
  # `meth` is a character vector with names representing aliases, different from
  # "main" method name. Values represent "main"method names.
  meth <- classmetric_aliases[names(classmetric_aliases) != classmetric_aliases]

  for (i in seq_along(meth)) {
    expect_equal(
      summ_classmetric(f_dis, g_dis, dis_threshold, names(meth)[i]),
      summ_classmetric(f_dis, g_dis, dis_threshold, meth[i])
    )
  }
})

test_that("summ_classmetric works with different pdqr classes", {
  expect_equal(
    summ_classmetric(d_dis, d_con, 2), summ_classmetric(p_dis, q_con, 2)
  )
})

test_that("summ_classmetric validates input", {
  expect_error(summ_classmetric("a", d_dis, 1), "`f`.*not pdqr-function")
  expect_error(summ_classmetric(d_dis, "a", 1), "`g`.*not pdqr-function")
  expect_error(
    summ_classmetric(d_dis, d_dis),
    "`threshold`.*missing.*classification threshold"
  )
  expect_error(summ_classmetric(d_dis, d_dis, "a"), "`threshold`.*numeric")
  expect_error(
    summ_classmetric(d_dis, d_dis, 1, method = 1), "`method`.*string"
  )
  expect_error(
    summ_classmetric(d_dis, d_dis, 1, method = "a"), "`method`.*one of"
  )
})


# summ_classmetric_df -----------------------------------------------------
test_that("summ_classmetric_df works", {
  # Skip check on "noLD" platform due to complexity of accuracy checking
  # Don't use `skip_if()` because otherwise CRAN doesn't accept submission
  if (is_noLD()) {
    expect_true(TRUE)
    return()
  }

  method_vec <- unique(classmetric_aliases)

  expect_equal(
    summ_classmetric_df(f_dis, g_dis, dis_threshold, method = method_vec),
    dis_classmetric_df
  )
  expect_equal(
    summ_classmetric_df(f_dis, f_con, mixed_threshold, method = method_vec),
    mixed_classmetric_df
  )
  expect_equal(
    summ_classmetric_df(f_con, g_con, con_threshold, method = method_vec),
    con_classmetric_df
  )
})

test_that("summ_classmetric_df respects method aliases", {
  # `meth` is a character vector with names representing aliases, different from
  # "main" method name. Values represent "main"method names.
  meth <- classmetric_aliases[names(classmetric_aliases) != classmetric_aliases]

  total_df <- summ_classmetric_df(
    f_dis, g_dis, mixed_threshold, method = names(classmetric_aliases)
  )
  alias_df <- total_df[, names(meth)]
  ref_df <- total_df[, meth]
  ## Change names to use `expect_equal`
  names(ref_df) <- names(alias_df)

  expect_equal(alias_df, ref_df)
})

test_that("summ_classmetric_df works with different pdqr classes", {
  expect_equal(
    summ_classmetric_df(d_dis, d_con, 2),
    summ_classmetric_df(p_dis, q_con, 2)
  )
})


test_that("summ_classmetric_df validates input", {
  expect_error(summ_classmetric_df("a", d_dis, 1), "`f`.*not pdqr-function")
  expect_error(summ_classmetric_df(d_dis, "a", 1), "`g`.*not pdqr-function")
  expect_error(
    summ_classmetric_df(d_dis, d_dis),
    "`threshold`.*missing.*classification threshold"
  )
  expect_error(summ_classmetric_df(d_dis, d_dis, "a"), "`threshold`.*numeric")
  expect_error(
    summ_classmetric_df(d_dis, d_dis, 1, method = 1), "`method`.*character"
  )
  expect_error(
    summ_classmetric_df(d_dis, d_dis, 1, method = "a"), "`method`.*values"
  )
})


# classmetric -------------------------------------------------------------
# Tested in `summ_classmetric()` and `summ_classmetric_df()`


# classmetric_op ----------------------------------------------------------
# Tested in `summ_classmetric()` and `summ_classmetric_df()`


# classmetric_mcc ---------------------------------------------------------
# Tested in `summ_classmetric()` and `summ_classmetric_df()`
