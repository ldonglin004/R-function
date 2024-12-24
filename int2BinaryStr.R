int2BinaryStr <- function(x) {
  if (x < -2147483648 || x > 2147483647 || !is.integer(x)) {
    return(NA)
  }
  bit <- paste0(rev(as.integer(intToBits(x)))[1:32], collapse = "")
  bit <- sprintf("%032s", bit)
  return(bit)
}
