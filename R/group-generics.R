# Methods for group generics ----------------------------------------------
#' @export
Math.pdqr <- function(x, ...) {
  assert_pdqr_fun(x)

  switch(
    .Generic,
    abs = math_abs(x),
    sign = math_sign(x),
    math_pdqr_impl(.Generic, x, ...)
  )
}

#' @export
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
      e_list <- ensure_pdqr_functions(e1, e2)

      n_sample <- getOption("pdqr.group_gen.n_sample")
      args_new <- getOption("pdqr.group_gen.args_new")

      res <- form_trans(
        e_list, get(.Generic), method = "random",
        n_sample = n_sample, args_new = args_new
      )

      # Ensure that `res` doesn't have values outside of reasonable support
      repair_group_gen_support(
        res, .Generic, supp_list = lapply(e_list, meta_support)
      )
    }
  }
}

#' @export
Summary.pdqr <- function(..., na.rm = FALSE) {
  if (.Generic == "range") {
    stop_collapse(
      "`range()` can't be applied to ", '"pdqr" functions as it returns two ',
      "numbers. Use `min()` and `max()`."
    )
  }

  dots <- list(...)

  gen_fun <- function(...) {
    g <- get(.Generic)
    f <- function(...) {g(..., na.rm = na.rm)}

    # `Map()` is needed to "vectorize" `Summary` generics
    unlist(Map(f, ...))
  }

  n_sample <- getOption("pdqr.group_gen.n_sample")
  args_new <- getOption("pdqr.group_gen.args_new")

  res <- form_trans(
    dots, gen_fun, method = "random",
    n_sample = n_sample, args_new = args_new
  )

  # Ensure that `res` doesn't have values outside of reasonable support
  repair_group_gen_support(
    res, .Generic, supp_list = lapply(dots, meta_support)
  )
}


# Helper implementation functions -----------------------------------------
math_pdqr_impl <- function(gen, f, ...) {
  n_sample <- getOption("pdqr.group_gen.n_sample")
  args_new <- getOption("pdqr.group_gen.args_new")

  gen_fun <- function(y) {
    get(gen)(y, ...)
  }

  res <- form_trans(
    list(f), gen_fun, method = "random",
    n_sample = n_sample, args_new = args_new
  )

  # Ensure that `res` doesn't have values outside of reasonable support
  repair_group_gen_support(res, gen, supp_list = list(meta_support(f)))
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


# Functions for repairing support -----------------------------------------
repair_group_gen_support <- function(f, gen, supp_list) {
  repair_supp_method <- getOption("pdqr.group_gen.repair_supp_method")
  if (is.null(repair_supp_method)) {
    return(f)
  }

  new_f_supp <- switch(
    gen,
    sqrt     = sqrt(pmax(0, supp_list[[1]])),
    floor    = floor(supp_list[[1]]),
    ceiling  = ceiling(supp_list[[1]]),
    trunc    = trunc(supp_list[[1]]),
    round    = round(supp_list[[1]]),
    signif   = signif(supp_list[[1]]),
    exp      = exp(supp_list[[1]]),
    log      = inf_to_na(log(supp_list[[1]])),
    expm1    = expm1(supp_list[[1]]),
    log1p    = inf_to_na(log1p(supp_list[[1]])),
    cos      = repair_supp_periodic(
      op = `cos`, supp = supp_list[[1]], ref_points = c(1, 2)*pi, period = 2*pi
    ),
    sin      = repair_supp_periodic(
      op = `sin`, supp = supp_list[[1]], ref_points = c(-1, 1)*0.5*pi,
      period = 2*pi
    ),
    tan      = repair_supp_periodic(
      op = `tan`, supp = supp_list[[1]],
      # Inside nudges are needed to explicitly state side of special points
      ref_points = c(-1, 1)*0.5*pi + 1e-15*c(1, -1),
      period = pi
    ),
    cospi    = repair_supp_periodic(
      op = `cospi`, supp = supp_list[[1]], ref_points = c(1, 2), period = 2
    ),
    sinpi    = repair_supp_periodic(
      op = `sinpi`, supp = supp_list[[1]], ref_points = c(-1, 1)*0.5, period = 2
    ),
    tanpi    = repair_supp_periodic(
      op = `tanpi`, supp = supp_list[[1]],
      # Inside nudges are needed to explicitly state side of special points
      ref_points = c(-1, 1)*0.5 + 1e-15*c(1, -1), period = 1
    ),
    acos     = repair_supp_monotone(
      op = `acos`, supp = supp_list[[1]], input_bounds = c(-1, 1),
      increasing_op = FALSE
    ),
    asin     = repair_supp_monotone(
      op = `asin`, supp = supp_list[[1]], input_bounds = c(-1, 1)
    ),
    atan     = atan(supp_list[[1]]),
    cosh     = repair_supp_cosh(supp_list[[1]]),
    sinh     = sinh(supp_list[[1]]),
    tanh     = tanh(supp_list[[1]]),
    acosh    = repair_supp_monotone(
      op = `acosh`, supp = supp_list[[1]], input_bounds = c(1, Inf)
    ),
    asinh    = asinh(supp_list[[1]]),
    atanh    = repair_supp_monotone(
      op = `atanh`, supp = supp_list[[1]], input_bounds = c(-1, 1)
    ),
    lgamma   = simulate_repair_supp(`lgamma`, supp_list),
    gamma    = simulate_repair_supp(`gamma`, supp_list),
    digamma  = simulate_repair_supp(`digamma`, supp_list),
    trigamma = simulate_repair_supp(`trigamma`, supp_list),
    `+`      = supp_list[[1]] + supp_list[[2]],
    `-`      = repair_supp_subtraction(supp_list[[1]], supp_list[[2]]),
    `*`      = repair_supp_multiplication(supp_list[[1]], supp_list[[2]]),
    `/`      = repair_supp_division(supp_list[[1]], supp_list[[2]]),
    `^`      = simulate_repair_supp(`^`, supp_list),
    `%%`     = simulate_repair_supp(`%%`, supp_list),
    `%/%`    = floor(repair_supp_division(supp_list[[1]], supp_list[[2]])),
    sum      = repair_supp_sum(supp_list),
    prod     = repair_supp_prod(supp_list),
    min      = repair_supp_min(supp_list),
    max      = repair_supp_max(supp_list),
    meta_support(f)
  )

  form_resupport(f, support = new_f_supp, method = repair_supp_method)
}

repair_supp_periodic <- function(op, supp, ref_points, period) {
  op_supp <- op(supp)
  ref_supp <- range(op(ref_points))

  if (is_periodically_inside(ref_points[1], supp, period)) {
    supp_left <- ref_supp[1]
  } else {
    supp_left <- min(op_supp)
  }

  if (is_periodically_inside(ref_points[2], supp, period)) {
    supp_right <- ref_supp[2]
  } else {
    supp_right <- max(op_supp)
  }

  inf_to_na(c(supp_left, supp_right))
}

is_periodically_inside <- function(x, interval, period) {
  k_left <- ceiling((interval[1] - x) / period)
  k_right <- floor((interval[2] - x) / period)

  k_left <= k_right
}

repair_supp_monotone <- function(op, supp, input_bounds = c(-Inf, Inf),
                                 increasing_op = TRUE) {
  supp <- c(max(supp[1], input_bounds[1]), min(supp[2], input_bounds[2]))
  if (!increasing_op) {
    supp <- supp[2:1]
  }

  inf_to_na(op(supp))
}

repair_supp_cosh <- function(supp) {
  if ((supp[1] < 0) && (0 < supp[2])) {
    c(1, max(cosh(supp)))
  } else {
    cosh(supp)
  }
}

repair_supp_subtraction <- function(e1_supp, e2_supp) {
  c(e1_supp[1] - e2_supp[2], e1_supp[2] - e2_supp[1])
}

repair_supp_multiplication <- function(e1_supp, e2_supp) {
  # Here `na.rm = TRUE` is needed to avoid `NaN`s in case `0*Inf` when called
  # inside `repair_supp_division()`
  range(e1_supp[1]*e2_supp, e1_supp[2]*e2_supp, na.rm = TRUE)
}

repair_supp_division <- function(e1_supp, e2_supp) {
  inf_to_na(repair_supp_multiplication(e1_supp, repair_supp_inverse(e2_supp)))
}

repair_supp_inverse <- function(supp) {
  if ((supp[1] > 0) || (supp[2] < 0)) {
    1 / supp[2:1]
  } else if (supp[1] == 0) {
    # In this case `supp[2]` can't be 0
    c(1 / supp[2], Inf)
  } else if (supp[2] == 0) {
    # In this case `supp[1]` can't be 0
    c(-Inf, 1 / supp[1])
  } else {
    # Case when 0 is strictly inside support
    c(-Inf, Inf)
  }
}

repair_supp_sum <- function(supp_list) {
  supp_left <- sum(vapply(supp_list, `[`, FUN.VALUE = numeric(1), i = 1))
  supp_right <- sum(vapply(supp_list, `[`, FUN.VALUE = numeric(1), i = 2))

  c(supp_left, supp_right)
}

repair_supp_prod <- function(supp_list) {
  if (length(supp_list) == 1) {
    supp_list[[1]]
  } else {
    Reduce(repair_supp_multiplication, supp_list[-1], init = supp_list[[1]])
  }
}

repair_supp_min <- function(supp_list) {
  supp_left <- min(vapply(supp_list, `[`, FUN.VALUE = numeric(1), i = 1))
  supp_right <- min(vapply(supp_list, `[`, FUN.VALUE = numeric(1), i = 2))

  c(supp_left, supp_right)
}

repair_supp_max <- function(supp_list) {
  supp_left <- max(vapply(supp_list, `[`, FUN.VALUE = numeric(1), i = 1))
  supp_right <- max(vapply(supp_list, `[`, FUN.VALUE = numeric(1), i = 2))

  c(supp_left, supp_right)
}

simulate_repair_supp <- function(op, supp_list, n = 1e4) {
  smpl_list <- lapply(supp_list, function(supp) {
    stats::runif(n, min = supp[1], max = supp[2])
  })

  smpl <- do.call(op, smpl_list)

  # Replace `Inf` and `-Inf` with `NA` to be appropriate for `form_resupport()`
  inf_to_na(range(smpl, na.rm = TRUE))
}
