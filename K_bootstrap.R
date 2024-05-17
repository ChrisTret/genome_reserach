bootstrap_K <- function(X, B, T){
  n <- length(X)
  Tnboot <- numeric(B)
  for(b in 1:B){
    xboot <- sample(X, n, replace = T)
    Tnboot[b] <- k1d_univ(xboot, T)
  }
  Vboot <- (B-1)/B*var(Tnboot)
  return(list(Tnboot=Tnboot, Vboot = Vboot, seboot = sqrt(Vboot)))
}


bootstrap_K_biv <- function(X, Y, B, T){
  n <- length(X)
  m <- length(Y)
  Tnboot <- numeric(B)
  for(b in 1:B){
    xboot <- sample(X, n, replace = T)
    yboot <- sample(Y, m, replace = T)
    Tnboot[b] <- k1d_biv(xboot, yboot, T)
  }
  Vboot <- (B-1)/B*var(Tnboot)
  return(list(Tnboot=Tnboot, Vboot = Vboot, seboot = sqrt(Vboot)))
}

#test_boot <- bootstrap_K(Alu_split[[1]], 100, 3000)
