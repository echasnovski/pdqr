Math.pdqr <- function(x, ...) {
  assert_pdqr_fun(x)

  switch(
    .Generic,
    abs = math_abs(x),
    sign = math_sign(x),
    math_pdqr_impl(.Generic, x, ...)
  )
}

math_pdqr_impl <- function(gen, f, ...) {
  n_sample <- getOption("pdqr.group_gen.n_sample")
  args_new <- getOption("pdqr.group_gen.args_new")

  gen_fun <- function(y) {
    get(gen)(y, ...)
  }

  form_trans(
    list(f), gen_fun, method = "random",
    n_sample = n_sample, args_new = args_new
  )
}

Ops.pdqr <- function(e1, e2) {
  if (missing(e2)) {
    assert_pdqr_fun(e1)

    switch(
      .Generic,
      `+` = e1,
      `-` = reflect_pdqr_around_zero(e1),
      `!` = negate_pdqr(e1)
    )
  } else {
    if (is_ops_linear(.Generic, e1, e2)) {
      ops_linear(.Generic, e1, e2)
    } else if (.Generic %in% c(">=", ">", "<=", "<", "==", "!=")) {
      ops_compare(.Generic, e1, e2)
    } else if (.Generic %in% c("&", "|")) {
      ops_logic(.Generic, e1, e2)
    } else {
      assert_pdqr_fun(e1)
      assert_pdqr_fun(e2)

      n_sample <- getOption("pdqr.group_gen.n_sample")
      args_new <- getOption("pdqr.group_gen.args_new")

      form_trans(
        list(e1, e2), get(.Generic), method = "random",
        n_sample = n_sample, args_new = args_new
      )
    }
  }
}

Summary.pdqr <- function(..., na.rm = FALSE) {
  if (.Generic == "range") {
    stop_collapse(
      "`range()` can't be applied to ", '"pdqr" functions as it returns two ',
      "numbers. Use `min()` and `max()`."
    )
  }

  gen_fun <- function(...) {
    g <- get(.Generic)
    f <- function(...) {g(..., na.rm = na.rm)}

    # `Map()` is needed to "vectorize" `Summary` generics
    unlist(Map(f, ...))
  }

  n_sample <- getOption("pdqr.group_gen.n_sample")
  args_new <- getOption("pdqr.group_gen.args_new")

  form_trans(
    list(...), gen_fun, method = "random",
    n_sample = n_sample, args_new = args_new
  )
}

math_abs <- function(f) {
  if (meta_type(f) == "fin") {
    x_tbl <- meta_x_tbl(f)
    x_tbl[["x"]] <- abs(x_tbl[["x"]])

    new_pdqr_by_ref(f)(x_tbl, "fin")
  } else {
    f_mix <- form_mix(list(f, -f))

    form_resupport(f_mix, c(0, NA), method = "trim")
  }
}

math_sign <- function(f) {
  d_f <- as_d(f)

  x_tbl <- data.frame(
    x    = c(          -1,             0,            1),
    prob = c((d_f < 0)(1), (d_f == 0)(1), (d_f > 0)(1))
  )

  new_pdqr_by_ref(f)(x_tbl, "fin")
}

reflect_pdqr_around_zero <- function(f) {
  refl_x_tbl <- reflect_x_tbl(meta_x_tbl(f), 0)

  new_pdqr_by_ref(f)(refl_x_tbl, meta_type(f))
}

negate_pdqr <- function(f) {
  # Probability of type "infin" pdqr-function being exactly 0 is equal to zero
  prob_zero <- if (meta_type(f) == "fin") {as_d(f)(0)} else {0}

  new_pdqr_by_ref(f)(
    data.frame(x = 0:1, prob = c(1-prob_zero, prob_zero)), "fin"
  )
}

is_ops_linear <- function(gen, e1, e2) {
  e1_is_number <- is_single_number(e1)
  e2_is_number <- is_single_number(e2)

  ((gen %in% c("+", "-", "*")) && (e1_is_number || e2_is_number)) ||
    ((gen == "/") && (e2_is_number))
}

ops_linear <- function(gen, e1, e2) {
  # It is assumed that this function is run only if `is_ops_linear` gave `TRUE`
  e1_is_number <- is_single_number(e1)

  # `e1` and `e2` should be exactly one single number and one pdqr-function
  if (e1_is_number) {
    assert_pdqr_fun(e2)

    ops_meta <- list(e1_num = e1, e2_num = meta_support(e2), pdqr = e2)
  } else {
    assert_pdqr_fun(e1)

    ops_meta <- list(e1_num = meta_support(e1), e2_num = e2, pdqr = e1)
  }

  if ((gen == "-") && e1_is_number) {
    # This relies on custom implementations of `-` which takes one argument and
    # `+` which takes two arguments and goes into the next clause
    (-e2) + e1
  } else {
    # Output is done by transforming support linearly based on the input
    # operation `gen` and number
    res_supp <- get(gen)(ops_meta[["e1_num"]], ops_meta[["e2_num"]])

    form_resupport(ops_meta[["pdqr"]], res_supp, method = "linear")
  }
}

ops_compare <- function(gen, e1, e2) {
  input <- ensure_pdqr_functions(e1, e2)
  e1 <- input[[1]]
  e2 <- input[[2]]

  switch(
    gen,
    `>=` = form_geq(e1, e2),
    `>`  = form_greater(e1, e2),
    `<=` = form_leq(e1, e2),
    `<`  = form_less(e1, e2),
    `==` = form_equal(e1, e2),
    `!=` = form_not_equal(e1, e2)
  )
}

ops_logic <- function(gen, e1, e2) {
  input <- ensure_pdqr_functions(e1, e2)
  e1 <- input[[1]]
  e2 <- input[[2]]

  d_zero <- new_d(0, "fin")

  zero_prob_1 <- prob_equal(e1, d_zero)
  zero_prob_2 <- prob_equal(e2, d_zero)

  out_class <- get_pdqr_class(e1)

  switch(
    gen,
    `&` = boolean_pdqr((1 - zero_prob_1) * (1 - zero_prob_2), out_class),
    `|` = boolean_pdqr(1 - zero_prob_1*zero_prob_2, out_class)
  )
}

ensure_pdqr_functions <- function(e1, e2) {
  if (is_single_number(e1)) {
    assert_pdqr_fun(e2)

    e1 <- new_pdqr_by_ref(e2)(e1, "fin")
  } else if (is_single_number(e2)) {
    assert_pdqr_fun(e1)

    e2 <- new_pdqr_by_ref(e1)(e2, "fin")
  } else {
    if (!is_pdqr_fun(e1)) {
      stop_collapse("`e1` should be pdqr-function or single number.")
    }
    if (!is_pdqr_fun(e2)) {
      stop_collapse("`e2` should be pdqr-function or single number.")
    }
  }

  list(e1, e2)
}
