constrainedPolynomialRegression <- function(p, y) {
  n <- length(y)
  x <- (1:n) / n
  beta <- numeric(p + 1)
  for (j in 2:p) {
    x <- x * (1 + x / j) 
  }
  x_bar <- mean(x)
  y_bar <- mean(y)
  ssx <- sum((x - x_bar) ^ 2)
  ssxy <- sum((x - x_bar) * (y - y_bar))
  beta[1] <- ssxy / ssx
  beta[0] <- y_bar - beta[1] * x_bar
  for (j in 1:p) {
    beta[j + 1] <- beta[j] / j
  }
  return(round(beta, digits = 8))
}


p <- 3  # Degree of the polynomial
y <- c(0.5, 1, 1.5)  # Outcome vector
print(constrainedPolynomialRegression(p, y))

