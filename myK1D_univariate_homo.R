### NO WEIGHTING FUNCTION CALCULATED

# Demo with length 10 vec, t=3
demo <- seq(from = 1, to = 100, by = 5)
demo <- as.matrix(c(10:1000))
k1d_univ(demo, T = c(10))

my_test <- as.matrix(c(1,6,12,15,17,24))
k1d_univ(my_test, T = c(3,4,10)) # hand-calculated 4/3, 8/3. 32/3


# Algorithm idea
# 1. select point a
# 2. calculate values in (a-t, a)
# 3. calculated values in (a, a+t)
# 4. sum values
# 5. repeat for next i

################################################################################
#' Estimate Ripley's K function with loops
#'
#' @param X vector with data of one kind sorted in ascending order
#' @param T vector with diameters to search over in ascending order
#'
#' @returns vector of estimated K value for each t in T
################################################################################
k1d_univ <- function(X, T){
  head(X)
  N <- length(X)
  X <- sort(X)
  
  print(paste("Obs X", N))
  print(paste("Distance", X[N]-X[1]+1))
  
  obs_sum <- 0
  lambda_hat <- N / (X[N]-X[1]+1) # number occurrences over length of interval
  print(paste("lambda:", lambda_hat))
  
  # Initialize objects
  down_sum <- integer(length(T))
  up_sum <- integer(length(T))
  obs_sum <- integer(length(T))
  K_hat <- numeric(length(T))
  
  # a: index of current focus point in X
  for(a in 1:N){
    for(t in 1:length(T)){
      # calculate x in (a-t, a)
      down_sum[t] <- check_interval_down(X,a,T[t])

      # calculate x in (a, a+t)
      up_sum[t] <- check_interval_up(X,a,T[t])

      # calculate num observed x in (a-t, a+t)
      obs_sum[t] <- obs_sum[t] + down_sum[t] + up_sum[t]
    }
  }

  K_hat <- obs_sum/(lambda_hat*N)

  return(K_hat)
}

k1d_univ(Alu, 1000)

k1d_univ(my_test, T = c(3,4,10)) # hand-calculated 4/3, 8/3. 32/3

k1d_univ(demo, 10)

################################################################################
#' Count number of observations within t of element a in X on the right
#'
#' @param X vector with data of one kind sorted in ascending order
#' @param a index of element in X to search around
#' @param t length to search over
#'
#' @returns number of observations within t of element a in X on the right
################################################################################
check_interval_up <- function(X,a,t){
  j = 1
  my_sum <- 0
  while(a + j <= length(X) && X[a+j] < X[a] + t){
    my_sum = my_sum + 1
    j = j + 1
  }
  return(my_sum)
}

################################################################################
#' Count number of observations within t of element a in X on the left
#'
#' @param X vector with data of one kind sorted in ascending order
#' @param a index of element in X to search around
#' @param t length to search over
#' 
#' @returns number of observations within t of element a in X on the left
################################################################################
check_interval_down <- function(X,a,t){
  j = 1
  my_sum <- 0
  while(a > j && X[a] - t < X[a-j]){
    my_sum = my_sum + 1
    j = j + 1
  }
  return(my_sum)
}


################################################################################
#' Compare speed of two Ripley's K algorithms
#'
#' @param X vector with data of one kind sorted in ascending order
#' @param T vector with diameters to search over in ascending order
#' @param function1 first function to compare
#' @param function2 second function to compare
#' @param repetitions numer of times to run each experiment (default 50)
#'
#' @returns vector of estimated K value for each t in T
################################################################################
speed_comparison <- function(X, t, function1, function2, repetitions = 50){
  
  #initialize time vectors
  total_time1 <- numeric(repetitions)
  total_time2 <- numeric(repetitions)
  
  #run experiment 1
  for (i in 1:repetitions) {
    start <-Sys.time()
    function1(X,t)
    end <- Sys.time()
    total_time1[i] <- end - start
  }
  
  #run experiment 2
  for (i in 1:repetitions) {
    start <-Sys.time()
    function2(X,t)
    end <- Sys.time()
    total_time2[i] <- end - start
  }
  
  # Create dataframe
  time1 <- data.frame(duration = total_time1)
  colnames(time1) <- "duration"
  time2 <- data.frame(duration = total_time2)
  colnames(time2) <- "duration"
  time1$test <- "1"
  time2$test <- "2"
  times <- rbind(time1,time2)
  
  # plot histograms
  hist <- ggplot(times, aes(x = duration, fill = test))+
    geom_histogram(position = "identity", alpha = 0.6) +
    labs(title = "Time Hisograms")
  print(hist)
  
  # run t test
  t.test(time1$duration,time2$duration)
}

speed_comparison(X = demo, t = 10, function1 = k1d_univ, function2 = k1d_univ2, 
                 repetitions = 50)


################################################################################
#' Calculate number of data points within t of edge
#'
#' @param X vector with data of one kind sorted in ascending order
#' @param t diameter to check for data within t of edge
#'
#' @returns number of data points within t of edge
################################################################################
danger_zone <- function(X,t){
  danger_count1 <- 1 # X[1] is by definition in the danger zone
  danger_count2 <- 1 # X[n] is by definition in the danger zone
  n <- length(X)
  
  while(X[danger_count1+1] - t < X[1]){
    danger_count1 <- danger_count1 + 1
  }
  while (X[n-danger_count2] + t > X[n] && n-danger_count2 > danger_count1+1){
    danger_count2 <- danger_count2 + 1
  }
  
  danger_count <- danger_count1 + danger_count2
  
  return(danger_count)
}
danger_zone(demo, 10)
