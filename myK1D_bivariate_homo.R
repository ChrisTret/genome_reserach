################################################################################
#' Estimate Ripley's K function with loops
#'
#' @param X vector with data of one kind sorted in ascending order
#' @param Y vector with data of one kind sorted in ascending order
#' @param T vector with diameters to search over in ascending order
#'
#' @returns vector of estimated K_XY value for each t in T
################################################################################
k1d_biv <- function(X, Y, T){
  Nx <- length(X)
  Ny <- length(Y)
  
  lambda_hat_x <- Nx / (X[Nx]-X[1]+1)
  print(paste("Lambda x:", lambda_hat_x))
  lambda_hat_y <- Ny / (Y[Ny]-Y[1]+1)
  print(paste("Lambda y:", lambda_hat_y))
  
  area <- max(X[Nx],Y[Ny]) - min(X[1],Y[1])
  print(paste("Area:", area))
  
  
  result <- numeric(length(T))
  obs_sum <- numeric(length(T))
  K_hat <- numeric(length(T))
  
  for (t in 1:length(T)){
    start <- 1
    for (a in 1:Nx){
      result <- check_range_biv(X,Y,a,start,T[t])
      obs_sum[t] <- obs_sum[t] + result[[1]]
      start <- result[[2]]
    }
  }
  
  K_hat <- obs_sum/(lambda_hat_y*lambda_hat_y*area)
  
  return(K_hat)
}

k1d_biv(Alu, L1, c(50,500,1000,2500,5000))
k1d_biv(L1, Alu, c(50,500,1000,2500,5000))

k1d_univ(Alu, c(50,500,1000,2500,5000))


################################################################################
#' Count number of observations within t of element a of X in Y on the left
#'
#' @param X vector of data in ascending order to check around a given element 
#' @param Y vector of values to check for within t below an element in X
#' @param a index of element in X to search around
#' @param j smallest index of Y in range of t from X[a] in previous iteration 
#' @param t length to search over
#'
#' @returns a list with two items
#'   \item {my_sum}{number of observations within t of element a in X on the left}
#'   \item {next_start}{index to start at for next call}
#' 
################################################################################
check_range_biv <- function(X,Y,a,j,t){
  next_start <- j
  my_sum <- 0
  while(j <= length(Y) && X[a] + t > Y[j]){
    if(X[a]-t < Y[j]){
      my_sum = my_sum + 1
    }
    else{
      next_start <- j 
      # ideally start next iteration at first success, 
      # but this is easier to implement
    }
    j = j + 1
  }
  return(list(my_sum = my_sum, next_start = next_start))
}


#### Algorithm Idea
#'
#' 1. Select x
#' 2. Calculate num y w/i t above x
#' 3. Calculate num y w/i t below x
#' 4. Move to next x
#' 5. Sum over all
#' 6. Divide by range and estimated densities 

