euclideanDistanceThreshold <- function(X, Y, thres) {
  X_norm <- colSums(X^2)
  Y_norm <- colSums(Y^2)
  dist_squared <- outer(X_norm, Y_norm, "+") - 2 * t(X) %*% Y
  count <- sum(dist_squared <= thres^2)
  return(count)
}
