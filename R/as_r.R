as_r <- function(f, ...) {
  assert_pdqr_fun(f)

  new_r(x = meta(f, "x_tbl"), type = meta(f, "type"))
}
