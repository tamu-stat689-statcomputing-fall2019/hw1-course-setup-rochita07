# Generate n-dimensional response Y that follows linear regression model Y = Xbeta + epsilon, where epsilon is normal zero with variance sigma^2 independent across samples. Seed should be set at the beginning of the function
# X - design matrix
# beta - given parameter vector
# sigma - standard deviation of the noise
# seed  - starting seed value
generateY <- function(X, beta, sigma, seed = 5832652){
  #[ToDo] Set seed and generate Y following linear model
  epsilon = rnorm(n, mean = 0, sd = sigma)  # random vector of dimension n where each random variable is normal zero with variance sigma^2
  Y = X %*% beta + epsilon
  # Return Y
  return(Y)
}



# Calculate beta_LS - least-squares solution, do not use lm function
# X - design matrix
# Y -response
calculateBeta <- function(X, Y){
  # Calculate beta_LS
  beta_LS = solve(crossprod(X, X)) %*% crossprod(X, Y)
  # Return beta
  return(beta_LS)
}

# Calculate MSE
calculateMSE <- function(beta, beta_LS){
  
  # Return MSE - error ||beta - beta_LS||_2^2
  MSE = crossprod(beta - beta_LS, beta - beta_LS )/ length(beta)
  return(MSE)
}


