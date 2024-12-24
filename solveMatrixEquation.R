solveMatrixEquation <- function(A, B, C) {
  n <- ncol(A)
  Vec_C <- as(C, "sparseVector")
  D <- kronecker(Diagonal(n), A) + kronecker(t(B), Diagonal(n))
  u <- chol(crossprod(D))
  w <- forwardsolve(t(u),crossprod(D, Vec_C))
  Vec_x <- round(backsolve(u, w))
  x_matrix <- Matrix(Vec_x, n, n, sparse = TRUE)
  x <- as(x_matrix, "TsparseMatrix")
  return(x)
}