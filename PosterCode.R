# calculate univariate K values
t_vec <- seq(from = 50, to = 5000, length.out = 100)
K_Alu <- k1d_univ(Alu, T = t_vec)
K_L1 <- k1d_univ(L1, T = t_vec)

# univariate K for Alu by CSR plots
plot(c(0,5000), c(0,10000), type = "l", col = "blue", xlab = "t", 
  ylab = "K_hat", xlim = c(0,5000), ylim = c(0,20000),
  main = "Obserserved vs Theoretical K value for Alu under CSR")
points(t_vec, K_Alu, cex = 0.5, pch = 16)

# univariate K for L1 by CSR plots
plot(c(0,5000), c(0,10000), type = "l", col = "blue", xlab = "t", 
  ylab = "K_hat", xlim = c(0,5000), ylim = c(0,20000),
  main = "Obserserved vs Theoretical K value for L1 under CSR")
points(t_vec, K_L1, cex = 0.5, pch = 16)


# calculate bivariate K values
K_biv_A <- k1d_biv(Alu, L1, t_vec)
K_biv_L <- k1d_biv(L1, Alu, t_vec)

# bivariate K for Alu-L1 by CSR plots
plot(c(0,5000), c(0,10000), type = "l", col = "blue", xlab = "t", 
     ylab = "K_hat", xlim = c(0,5000), ylim = c(0,20000),
     main = "Obserserved vs Theoretical K value for Alu-L1 under CSR")
points(t_vec, K_biv_A, cex = 0.5, pch = 16)

# bivariate K for L1-Alu by CSR plots
plot(c(0,5000), c(0,10000), type = "l", col = "blue", xlab = "t", 
     ylab = "K_hat", xlim = c(0,5000), ylim = c(0,20000),
     main = "Obserserved vs Theoretical K value for L1-Alu under CSR")
points(t_vec, K_biv_L, cex = 0.5, pch = 16)



# Z stats
split <- 26
Alu_split <- split(Alu, rep(c(1:split), each = length(Alu)/split))
K <- numeric(split)
Z <- numeric(split)
t_star <- 3000

# bootstrap the variance
seBoot <- numeric(split)
for (i in c(1:splits)) {
  seBoot[i] <- bootstrap_K(Alu_split[[i]], 100, t_star)$seboot
}

# calculate K stats
for (i in c(1:splits)) {
  danger_count <-   danger_zone(Alu_split[[i]], t_star)
  print(paste(i, danger_count))
  K[i] <- k1d_univ(Alu_split[[i]], t_star)
  Z[i] <- (K[i]-2*t_star)/seBoot[i]
}
Z_stat_Alu <- sum(Z)/sqrt(split)

################################################################################

split <- 22
L1_split <- split(L1, rep(c(1:split), each = length(L1)/split))
K <- numeric(split)
Z <- numeric(split)
t_star <- 3000

# bootstrap the variance
seBoot <- numeric(split)
for (i in c(1:split)) {
  seBoot[i] <- bootstrap_K(L1_split[[i]], 100, t_star)$seboot
}

# calculate K stats
for (i in c(1:split)) {
  danger_count <-   danger_zone(L1_split[[i]], t_star)
  print(paste(i, danger_count))
  K[i] <- k1d_univ(L1_split[[i]], t_star)
  Z[i] <- (K[i]-2*t_star)/seBoot[i]
}
Z_stat_L1 <- sum(Z)/sqrt(split)

################################################################################

split <- 26
Alu_split <- split(Alu, rep(c(1:split), each = length(Alu)/split))
K <- numeric(split)
Z <- numeric(split)
t_star <- 3000

# bootstrap the variance
seBoot <- numeric(split)
for (i in c(1:splits)) {
  seBoot[i] <- bootstrap_K(Alu_split[[i]], 100, t_star)$seboot
}

# calculate K stats
for (i in c(1:splits)) {
  danger_count <-   danger_zone(Alu_split[[i]], t_star)
  print(paste(i, danger_count))
  K[i] <- k1d_univ(Alu_split[[i]], t_star)
  Z[i] <- (K[i]-2*t_star)/seBoot[i]
}
Z_stat_Alu <- sum(Z)/sqrt(split)

################################################################################

