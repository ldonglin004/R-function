streamedCov <- function(x, y) {
  stopifnot(length(x) == length(y))
  n = ax = ay = sxy = 0
  for (i in 1:length(x)) {
    xi = x[i]
    yi = y[i]
    n <- n + 1
    dx <- x[i] - ax
    dy <- y[i] - ay
    ax <- ax + dx / n
    ay <- ay + dy / n
    sxy <- sxy + (n-1) / n * dx * dy
  }
  return(sxy / (n-1))
}