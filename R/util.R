#' @noRd
.equals.double <- function(val, cmp, delta) val  <= cmp + delta  &  val >=  cmp - delta

#' @noRd
.in <- function(val, lower, upper) val >= lower & val <= upper
