my_pexp <- function(x, rate, lower.tail, log.p) {
  if (lower.tail == TRUE && log.p == FALSE) {
    return(-expm1(-rate * x))
  } 
  if (lower.tail == TRUE && log.p == TRUE) {
    if (abs(x) < 0.1) {
      return(log(-expm1(-rate * x)))
    } else {
      return(log1p(-exp(-rate * x)))
    }
  }
  if (lower.tail == FALSE && log.p == TRUE) {
    return(-rate * x)
  }
  if (lower.tail == FALSE && log.p == FALSE) {
    return(exp(-rate * x))
  }
}

print(my_pexp(1e-20, 1, TRUE, TRUE))