# VECTOR MEMORY EXHAUSTED ERROR from diff_matrix!!!
################################################################################
#' Estimate Ripley's K function with distance matrix
#'
#' @param X vector with data of one kind sorted in ascending order
#' @param T vector with diameters to search over in ascending order
#'
#' @returns vector of estimated K value for each t in T
################################################################################
k1d_univ2 <- function(X, T){
  N <- length(X)
  lambda_hat <- N / (X[N] - X[1] + 1)
  
  # Initialize objects
  down_sum <- matrix(0, nrow = length(T), ncol = N)
  up_sum <- matrix(0, nrow = length(T), ncol = N)
  
  # Pre-calculate the differences for efficiency
  diff_matrix <- abs(outer(X, X, "-"))
  print(diff_matrix)
  for (t in 1:length(T)){
    # Calculate x in (a-t, a)
    down_sum[t, ] <- colSums(diff_matrix <= T[t])
    
    # Calculate x in (a, a+t)
    up_sum[t, ] <- colSums(diff_matrix < T[t], na.rm = TRUE)
  }
  
  # Calculate observed sum
  obs_sum <- rowSums(down_sum + up_sum)
  
  # Calculate K_hat
  K_hat <- lambda_hat^-1 * obs_sum / N
  
  return(K_hat)
}

k1d_univ(demo, T = c(10))


