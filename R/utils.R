# Notifications -----------------------------------------------------------
stop_glue <- function(..., .sep = "", .envir = parent.frame(),
                      call. = FALSE, .domain = NULL) {
  stop(
    glue_null(..., .sep = .sep, .envir = .envir),
    call. = call., domain = .domain
  )
}

warning_glue <- function(..., .sep = "", .envir = parent.frame(),
                         call. = FALSE, .domain = NULL) {
  warning(
    glue_null(..., .sep = .sep, .envir = .envir),
    call. = call., domain = .domain
  )
}

message_glue <- function(..., .sep = "", .envir = parent.frame(),
                         .domain = NULL, .appendLF = TRUE) {
  message(
    glue_null(..., .sep = .sep, .envir = .envir),
    domain = .domain, appendLF = .appendLF
  )
}

glue_null <- function(..., .sep = "", .envir = parent.frame()) {
  glue::glue(
    ..., .sep = .sep, .envir = .envir, .transformer = null_transformer
  )
}

# This allows to print 'NULL' in `glue()` for code which evaluates in `NULL`
null_transformer <- function(text, envir) {
  out <- eval(parse(text = text, keep.source = FALSE), envir)
  if (is.null(out)) {
    return("NULL")
  }

  out
}


# Construct discrete distribution table -----------------------------------
compute_distr_tbl <- function(x) {
  x_tbl <- table(round(x, digits = 8))
  x_prob <- as.numeric(x_tbl) / length(x)

  data.frame(x = as.numeric(names(x_tbl)), prob = x_prob)
}


# Extended density --------------------------------------------------------
# Ensures continuous linear interpolation by adding first and last points
# with value 0.
density_ext <- function(x, ...) {
  dens <- stats::density(x, ...)

  x_dens <- dens[["x"]]
  n <- length(x_dens)
  max_offset <- 10^(-4)
  if (n == 1) {
    offset <- rep(max_offset, 2)
  } else {
    x_width_first <- diff(utils::head(x_dens, 2))
    x_width_last <- diff(utils::tail(x_dens, 2))
    offset <- pmin(max_offset, c(x_width_first, x_width_last))
  }

  new_x <- c(x_dens[1] - offset[1], x_dens, x_dens[n] + offset[2])
  new_y <- c(0, dens[["y"]], 0)

  new_integral <- trapez_integral(new_x, new_y)

  list(x = new_x, y = new_y / new_integral)
}

trapez_integral <- function(x, y) {
  # `x` is assumed to be sorted increasingly (as after the `density()` call)
  sum(diff(x) * (utils::head(y, -1) + utils::tail(y, -1))) / 2
}
