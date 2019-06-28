# Documentation -----------------------------------------------------------
#' Pdqr methods for S3 group generic functions
#'
#' There are custom methods implemented for three out of four [S3 group generic
#' functions][base::groupGeneric]: `Math`, `Ops`, `Summary`. **Note** that many
#' of them have random nature with an idea of generating samples from input
#' pdqr-functions, performing certain operation on them (results in one
#' generated sample from desired random variable), and creating new
#' pdqr-function with appropriate [new_*()][new-pdqr] function. This is done
#' with [form_trans()], so all rules for determining [class][meta_class()] and
#' [type][meta_type()] of output is taken from it.
#'
#' @param x,e1,e2 Objects.
#' @param ... Further arguments passed to methods.
#' @param na.rm Logical: should missing values be removed?
#'
#' @details Customization of method behavior may be done using mechanism of
#' [options()][base::options()]. These are the possible options:
#' - **`pdqr.group_gen.args_new`**. This will be used as `args_new` argument for
#' `form_trans()` in methods with random nature. Default is `list()`.
#' - **`pdqr.group_gen.n_sample`**. This will be used as `n_sample` argument for
#' `form_trans()` in methods with random nature. Default is 10000.
#' - **`pdqr.group_gen.repair_supp_method`**. All methods that have random
#' nature take care of output support by trying to "repair" it, because default
#' use of `new_*()` functions returns a slightly bigger support than range of
#' input sample (see Examples). Repairing is done with [form_resupport()] where
#' target support is computed separately and `method` argument is controlled by
#' this option (preferred ones are `"reflect"`, default, and `"trim"`). In most
#' cases output support is computed directly based on special features of
#' generic function. But for some difficult cases, like `gamma()`, `digamma()`,
#' `lgamma()`, `psigamma()`, `^`, and `%%` it is a result of simulation (i.e.
#' slightly random, which slightly increases random nature of those methods).
#'
#' @section Math:
#'
#' This family of S3 generics represents mathematical functions. Most of the
#' methods have **random nature**, except `abs()` and `sign()` which are
#' computed directly. Output of `sign()` has "discrete" type with 3 "x" values:
#' -1, 0, 1.
#'
#' **Note** that `cumsum()`, `cumprod()`, `cummmax()`, and `cummin()` functions
#' don't make much sense in these implementations: their outputs represent
#' random variable, sample of which is computed by applying `cum*()` function to
#' a sample, generated from input pdqr-function.
#'
#' @section Ops:
#'
#' This family of S3 generics represents common operators. For all functions
#' (except `&` and `|`) input can be a pdqr-function or single number.
#'
#' A list of methods with **non-random nature**:
#' - `!`, `+`, `-` in case of single input, i.e. `!f` or `-f`.
#' - Functions representing linear transformation, i.e. adding, subtracting,
#' multiplying, and dividing by a single number. For example, all `f + 1`,
#' `2 - f` (which is actually `(-f) + 2`), `3*f` and `f/2` are linear
#' transformations, but `1 / f`, `f + g` are not.
#' - Functions for comparing: `==`, `!=`, `<`, `<=`, `>=`, `>`. Their output is
#' **boolean pdqr-function**: "discrete" type function with elements being
#' exactly 0 and 1. Probability of 0 represents probability of operator output
#' being false, and 1 - being true. Probability of being true is computed
#' directly as **limit of empirical estimation from simulations** (as size of
#' samples grows to infinity). In other words, output is an exact number which
#' might be approximated by simulating two big samples of same size from input
#' `e1` and `e2` (one of which can be a single number), and estimating
#' probability as share of those pairs from samples for which comparison is
#' true. **Note** that if at least one input has "continuous" type, then:
#'     - `==` will always have probability 0 of being true because probability
#'     of generating a certain exact one or two numbers from continuous random
#'     variable is zero.
#'     - `!=` will always have probability 1 of being true for the same reason
#'     as above.
#'     - Pairs `>=` and `>`, `<=` and `<` will return the same input because
#'     probability of being equal is always zero.
#' - Logical functions `&` and `|`. Their input can be only pdqr-functions
#' (because single number input doesn't make much sense). They are most useful
#' for applying to boolean pdqr-functions (see description of functions for
#' comparing), and warning is thrown in case any input is not a boolean
#' pdqr-function. `&`'s probability of being true is a product of those
#' probabilities from input `e1` and `e2`. `|`'s probability of being false is a
#' product of those probabilities from input `e1` and `e2`. **Note** that
#' probability of being false is a probability of being equal to 0; of being
#' true - complementary to that.
#'
#' All other methods are **random**. For example, `f + f`, `f^g` are random.
#'
#' @section Summary:
#'
#' Methods for `all()` and `any()` have **non-random nature**. Their input can
#' be only pdqr-functions, and if any of them is not boolean, a warning is
#' thrown (because otherwise output doesn't make much sense). They return a
#' boolean pdqr-function with the following probability of being true:
#' - In `all()` - probability of *all* input function being true, i.e. product
#' of probabilities of being true (implemented as complementary to probability
#' of being equal to 0).
#' - In `any()` - probability of *any* input function being true, i.e.
#' complementary probability to product of all functions being false
#' (implemented as probability of being equal to 0).
#'
#' Methods for `sum()`, `prod()`, `min()`, `max()` have **random nature**. They
#' are implemented to use vectorized version of certain generic, because
#' transformation function for `form_trans()` should be vectorized: for input
#' samples which all have size n it should also return sample of size n (where
#' each element is a transformation output for corresponding elements from input
#' samples). This way `min(f, g)` can be read as "random variable
#' representing minimum of `f` and `g`", etc.
#'
#' **Notes**:
#' - `range()` function doesn't make sense here because it returns 2 numbers per
#' input and therefore can't be made vectorized. Error is thrown if it is
#' applied to pdqr-function.
#' - Although all `sum()`, `prod()`, `min()`, `max()` accept pdqr-functions or
#' single numbers, using numbers and "continuous" functions simultaneously is
#' not a great idea. This is because output will be automatically smoothed (as
#' `form_trans()` will use some `new_*()` function) which will give a misleading
#' picture. For a more realistic output:
#'     - Instead of `min(f, num)` use
#'     `form_resupport(f, c(num, NA), method = "winsor")` (see
#'     [form_resupport()]).
#'     - Instead of `max(f, num)` use
#'     `form_resupport(f, c(NA, num), method = "winsor")`.
#'     - Instead of `sum(f, num)` use `f + num`.
#'     - Instead of `prod(f, num)` use `f * num`.
#'
#' @return All methods return pdqr-function which represents the result of
#'   applying certain function to random variable(s) described with input
#'   pdqr-function(s). **Note** that independence of input random variables is
#'   assumed, i.e. `f + f` is not the same as `2*f` (see Examples).
#'
#' @seealso [summ_prob_true()] and [summ_prob_false()] for extracting
#'   probability from boolean pdqr-functions.
#'
#' @family pdqr methods for generic functions
#'
#' @examples
#' d_norm <- as_d(dnorm)
#' d_unif <- as_d(dunif)
#' d_dis <- new_d(data.frame(x = 1:4, prob = 1:4 / 10), "discrete")
#'
#' set.seed(101)
#'
#' # Math
#' plot(d_norm, main = "Math methods")
#'   # `abs()` and `sign()` are not random
#' lines(abs(d_norm), col = "red")
#'   # All others are random
#' lines(cos(d_norm), col = "green")
#' lines(cos(d_norm), col = "blue")
#'
#'   # Although here distribution shouldn't change, it changes slightly due to
#'   # random implementation
#' meta_x_tbl(d_dis)
#' meta_x_tbl(floor(d_dis))
#'
#' # Ops
#'   # Single input, linear transformations, and logical are not random
#' d_dis > 1
#' !(d_dis > 1)
#' d_norm >= (2*d_norm+1)
#'   # All others are random
#' plot(d_norm + d_norm)
#'   # This is an exact reference curve
#' lines(as_d(dnorm, sd = sqrt(2)), col = "red")
#'
#' plot(d_dis + d_norm)
#'
#' plot(d_unif^d_unif)
#'
#' # Summary
#'   # `all()` and `any()` are non-random
#' all(d_dis > 1, d_dis > 1)
#'   # Others are random
#' plot(max(d_norm, d_norm, d_norm))
#'
#' plot(d_norm + d_norm + d_norm)
#' lines(sum(d_norm, d_norm, d_norm), col = "red")
#'
#'   # Using single numbers is allowed, but gives misleading output in case of
#'   # "continuous" functions. Use other functions instead (see documentation).
#' plot(min(d_unif, 0.5))
#' lines(form_resupport(d_unif, c(NA, 0.5), method = "winsor"), col = "blue")
#'
#' # Use `options()` to control methods
#' plot(d_unif + d_unif)
#' op <- options(
#'   pdqr.group_gen.n_sample = 100,
#'   pdqr.group_gen.args_new = list(adjust = 0.5)
#' )
#' lines(d_unif + d_unif, col = "red")
#'   # `f + f` is different from `2*f` due to independency assumption. Also the
#'   # latter implemented non-randomly.
#' lines(2 * d_unif, col = "blue")
#'
#' # Methods for generics attempt to repair support, so they are more reasonable
#' # to use than direct use of `form_trans()`
#' d_unif + d_unif
#' form_trans(list(d_unif, d_unif), `+`)
#'
#' @name methods-group-generic
NULL


# Methods for group generics ----------------------------------------------
#' @rdname methods-group-generic
#' @export
Math.pdqr <- function(x, ...) {
  assert_gen_single_input(gen = .Generic, x)

  switch(
    .Generic,
    abs = math_abs(x),
    sign = math_sign(x),
    math_pdqr_impl(.Generic, x, ...)
  )
}

#' @rdname methods-group-generic
#' @export
Ops.pdqr <- function(e1, e2) {
  if (missing(e2)) {
    assert_gen_single_input(gen = .Generic, e1)

    switch(
      .Generic,
      `+` = e1,
      `-` = reflect_pdqr_around_zero(e1),
      `!` = negate_pdqr(e1)
    )
  } else {
    if (is_ops_linear(.Generic, e1, e2)) {
      return(ops_linear(.Generic, e1, e2))
    } else if (.Generic %in% c("&", "|")) {
      return(ops_logic(.Generic, e1, e2))
    }

    e_list <- ensure_pdqr_functions(gen = .Generic, e1, e2)
    e1 <- e_list[[1]]
    e2 <- e_list[[2]]

    if (.Generic %in% c(">=", ">", "<=", "<", "==", "!=")) {
      ops_compare(.Generic, e1, e2)
    } else {
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

#' @rdname methods-group-generic
#' @export
Summary.pdqr <- function(..., na.rm = FALSE) {
  if (.Generic == "range") {
    stop_collapse(
      "`range()` can't be applied to ", '"pdqr" functions as it returns two ',
      "numbers. Use `min()` and `max()`."
    )
  }

  if (.Generic %in% c("all", "any")) {
    return(summary_allany(gen = .Generic, ...))
  }

  dots <- ensure_pdqr_functions(gen = .Generic, ...)

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
  if (meta_type(f) == "discrete") {
    x_tbl <- meta_x_tbl(f)
    x_tbl[["x"]] <- abs(x_tbl[["x"]])

    new_pdqr_by_ref(f)(x_tbl, "discrete")
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

  new_pdqr_by_ref(f)(x_tbl, "discrete")
}

reflect_pdqr_around_zero <- function(f) {
  refl_x_tbl <- reflect_x_tbl(meta_x_tbl(f), 0)

  new_pdqr_by_ref(f)(refl_x_tbl, meta_type(f))
}

negate_pdqr <- function(f) {
  # Probability of type "continuous" pdqr-function being exactly 0 is equal to
  # zero
  prob_zero <- if (meta_type(f) == "discrete") {as_d(f)(0)} else {0}

  new_pdqr_by_ref(f)(
    data.frame(x = 0:1, prob = c(1-prob_zero, prob_zero)), "discrete"
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
    if (!is_pdqr_fun(e2)) {
      stop_collapse("Second argument of `", gen, "` should be pdqr-function.")
    }

    ops_meta <- list(e1_num = e1, e2_num = meta_support(e2), pdqr = e2)
  } else {
    if (!is_pdqr_fun(e1)) {
      stop_collapse("First argument of `", gen, "` should be pdqr-function.")
    }

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

ops_logic <- function(gen, e1, e2) {
  if (!is_pdqr_fun(e1) || !is_pdqr_fun(e2)) {
    stop_collapse("Both of `", gen, "` input should be pdqr-functions.")
  }
  if (!is_boolean_pdqr_fun(e1) || !is_boolean_pdqr_fun(e2)) {
    warning_boolean_pdqr_fun(f_name = paste0("One of `", gen, "` input"))
  }

  d_zero <- new_d(0, "discrete")

  prob_false_1 <- prob_equal(e1, d_zero)
  prob_false_2 <- prob_equal(e2, d_zero)

  out_class <- meta_class(e1)

  switch(
    gen,
    `&` = boolean_pdqr((1-prob_false_1)*(1-prob_false_2), out_class),
    `|` = boolean_pdqr(1 - prob_false_1*prob_false_2, out_class)
  )
}

ops_compare <- function(gen, e1, e2) {
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

summary_allany <- function(gen, ...) {
  dots <- list(...)
  all_pdqr <- all(vapply(dots, is_pdqr_fun, logical(1)))
  if (!all_pdqr) {
    stop_collapse("All input to `", gen, "()` should be pdqr-functions.")
  }
  any_non_boolean <- any(!vapply(dots, is_boolean_pdqr_fun, logical(1)))
  if (any_non_boolean) {
    warning_boolean_pdqr_fun(f_name = paste0("Some input to `", gen, "()`"))
  }

  d_zero <- new_d(0, "discrete")

  prob_false <- vapply(dots, function(f) {prob_equal(f, d_zero)}, numeric(1))

  out_class <- meta_class(dots[[1]])

  switch(
    gen,
    all = boolean_pdqr(prod(1 - prob_false), out_class),
    any = boolean_pdqr(1 - prod(prob_false), out_class)
  )
}

ensure_pdqr_functions <- function(gen, ...) {
  dots <- list(...)
  gen_name <- enbacktick(gen)

  is_pdqr <- vapply(dots, is_pdqr_fun, logical(1))
  is_number <- vapply(dots, is_single_number, logical(1))

  if (!all(is_pdqr | is_number)) {
    stop_collapse(
      "All inputs to ", gen_name, " should be pdqr-function or single number."
    )
  }

  f_class <- compute_f_list_meta(dots)[["class"]]
  res <- vector(mode = "list", length = length(dots))
  res[is_pdqr] <- dots[is_pdqr]
  res[is_number] <- lapply(
    dots[is_number], new_pdqr_by_class(f_class), type = "discrete"
  )

  res
}

assert_gen_single_input <- function(gen, input) {
  if (!is_pdqr_fun(input)) {
    stop_collapse("Input to `", gen, "` should be pdqr-function.")
  }

  TRUE
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
