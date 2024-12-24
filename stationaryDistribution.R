stationaryDistribution <- function(P) {
  n <- nrow(P)
  A <- P - diag(1, n)
  A <- rbind(A, rep(1, n))
  b <- c(rep(0, n), 1)
  Pi <- qr.solve(A, b)
  return(Pi)
}