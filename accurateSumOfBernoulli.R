accurateSumOfBernoulli <- function(p) {
  n <- length(p)
  if (n == 1) {
    return(c(1 - p, p))  
  }
  mid <- floor(n / 2)
  left_p <- p[1:mid]
  right_p <- p[(mid + 1):n]
  pmf_left <- accurateSumOfBernoulli(left_p)
  pmf_right <- accurateSumOfBernoulli(right_p)
  result_len <- length(pmf_left) + length(pmf_right) - 1
  result <- numeric(result_len)
  for (i in seq_along(pmf_left)) {
    result[i:(i + length(pmf_right) - 1)] <- result[i:(i + length(pmf_right) - 1)] + pmf_left[i] * pmf_right
  }
  
  return(result)
}
