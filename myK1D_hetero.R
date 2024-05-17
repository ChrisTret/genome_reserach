################################################################################
#' Create regions for piecewise constant rate
#'
#' @param X vector with data of one kind sorted in ascending order
#' @param num_reg number of evenly spaced regions to create
#'
#' @returns list of num_reg vectors, one for each region 
################################################################################
make_regions_univ <- function(X,num_reg = 0){
  
  range <- X[length(X)]-X[1]+1
  partition <- numeric(num_reg + 1)
  partition <- round(c(0:num_reg)*range/num_reg)
  print(paste("Range:", range))
  print(partition)
  
  # Initialize an empty list to store vectors
  data_list <- vector("list", length = num_reg)
  
  # X <- X - X[1]
  
  # Iterate through each element in X and place it in the appropriate column
  for (element in X) {
    for (i in 1:(num_reg)) {
      if (element - X[1] > partition[1] && element - X[1] <= partition[i + 1]) {
        data_list[[i]] <- c(data_list[[i]], element)
        break
      }
    }
  }
  
  return(data_list)
}

test <- make_regions(Alu, 4)


################################################################################
#' Estimate Ripley's K function for piecewise constant
#' heterogeneous univariate poisson process
#'
#' @param X vector with data of one kind sorted in ascending order
#' @param T vector with diameters to search over in ascending order
#' @param num-reg number of regions to create
#'
#' @returns vector of estimated K value for each t in T for each region
################################################################################
k1d_univ_list <- function(X, T, num_reg = 0, print = FALSE) {
  
  X_list <- make_regions_univ(X, num_reg)
  
  result_list <- list()  # Initialize a list to store results for each vector in X_list
  
  for (X in X_list) {
    result <- k1d_univ(X, T)  # Call the original k1d_univ function for each vector X
    result_list <- c(result_list, list(result))  # Store the result in the result_list
  }
  
  return(result_list)  # Return the list of results for each vector in X_list
}

k1d_univ_list(Alu, 500, 4)



################################################################################
#' Create regions for piecewise constant rate
#'
#' @param X vector with data of one kind sorted in ascending order
#' @param Y vector with data of another kind sorted in ascending order
#' @param num_reg number of evenly spaced regions to create
#'
#' @returns list of 2 lists of num_reg vectors
################################################################################
make_regions_biv <- function(X, Y, num_reg = 0){
  
  range <- max(X[length(X)], Y[length(Y)])-min(X[1],Y[1])+1
  partition <- numeric(num_reg + 1)
  partition <- round(c(0:num_reg)*range/num_reg)
  print(paste("Range:", range))
  print(partition)
  
  # Initialize empty lists to store vectors
  data_list_X <- vector("list", length = num_reg)
  data_list_Y <- vector("list", length = num_reg)
  
  # Iterate through each element in X and place it in the appropriate column
  for (element in X) {
    for (i in 1:(num_reg)) {
      if (element - X[1] > partition[1] && element - X[1] <= partition[i + 1]) {
        data_list_X[[i]] <- c(data_list_X[[i]], element)
        break
      }
    }
  }
  
  # Iterate through each element in Y and place it in the appropriate column
  for (element in Y) {
    for (i in 1:(num_reg)) {
      if (element - Y[1] > partition[1] && element - Y[1] <= partition[i + 1]) {
        data_list_Y[[i]] <- c(data_list_Y[[i]], element)
        break
      }
    }
  }
  
  data_list <- list(data_list_X, data_list_Y)
  
  return(data_list)
}

test <- make_regions_biv(Alu, L1,4)



################################################################################
#' Estimate Ripley's K function for piecewise constant
#' heterogeneous bivariate poisson process
#'
#' @param X vector with data of one kind sorted in ascending order
#' @param Y vector with data of another kind sorted in ascending order
#' @param T vector with diameters to search over in ascending order
#' @param num-reg number of regions to create
#' 
#' @returns vector of estimated K value for each t in T for each region
################################################################################
k1d_biv_list <- function(X, Y, T, num_reg = 0) {
  
  biv_list <- make_regions_biv(X, Y, num_reg)
  
  X_list <- biv_list[[1]]
  Y_list <- biv_list[[2]]
  
  result_list <- list()  # Initialize a list to store results for each vector in X_list
  
  for (i in seq_along(X_list)) {
    result <- k1d_biv(X_list[[i]], Y_list[[i]], T)  # Call the original k1d_biv function for each pair of vectors
    result_list <- c(result_list, list(result))  # Store the result in the result_list
  }
  
  return(result_list)  # Return the list of results for each vector in X_list
}

k1d_biv_list(Alu, L1, c(500,600), 4)
