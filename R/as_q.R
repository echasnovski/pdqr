as_q <- function(f, ...) {
  assert_pdqr_fun(f)

  new_q(x = meta(f, "x_tbl"), type = meta(f, "type"))
}
