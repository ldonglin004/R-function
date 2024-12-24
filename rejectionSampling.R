rejectionSampling <- function (n, a, b) {
  
  target_density <- function(x, a, b) {
    exp(-x^2) * x^(a - 1) * (1 - x)^(b - 1)
  }

  ratio <- function(x) {
    target_density(x, a, b) / dbeta(x, a, b)
  }
  M <- optimize(ratio, interval = c(0, 1), maximum = TRUE)$objective
  
  attempted <- 0
  accepted <- 0
  values <- numeric(n)
  
  
  while (accepted < n) {
    batch_size <- ceiling((n - accepted) * 1.1)
    x_candidates <- rbeta(batch_size, a, b)
    accept_probs <- target_density(x_candidates, a, b) / (M * dbeta(x_candidates, a, b))
    accepted_indices <- which(runif(batch_size) < accept_probs)
    n_accepted <- length(accepted_indices)
    
    if (accepted + n_accepted > n) {
      n_accepted <- n - accepted 
    }
    
    values[(accepted + 1):(accepted + n_accepted)] <- x_candidates[accepted_indices[1:n_accepted]]
    accepted <- accepted + n_accepted
    attempted <- attempted + batch_size
  }
  
  return(list(attempted = attempted, accepted = n, values = values))
}


# samp <- rejectionSampling(5000000,18.1,21.0)
# samp$accepted # number of accepted samples
# 
# 
# print(quantile(samp$values ,probs=c(0.01, 0.25, 0.5, 0.75, 0.99),na.rm=TRUE),
#         digits=1)
