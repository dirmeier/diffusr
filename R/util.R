#' @noRd
.equals.double <- function(val, cmp, delta) val  + delta <= cmp  |  val  - delta >= cmp

#' @noRd
.in <- function(val, lower, upper) val >= lower & val <= upper
