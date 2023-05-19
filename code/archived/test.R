rm(list = ls())
library(MASS)
library(tidyverse)
source("code/simTOcc_misspecified.R")
# generate covariance matrix for points in `x` using given kernel function
cov_matrix <- function(x, kernel_fn, ...) {
    outer(x, x, function(a, b) kernel_fn(a, b, ...))
}
# given x coordinates, take N draws from kernel function at those points
draw_samples <- function(x, N, seed = 1, kernel_fn, ...) {
    Y <- matrix(NA, nrow = length(x), ncol = N)
    # set.seed(seed)
    for (n in 1:N) {
        K <- cov_matrix(x, kernel_fn, ...)
        Y[, n] <- mvrnorm(1, mu = rep(0, times = length(x)), Sigma = K)
    }
    Y
}

x <- seq(0, 2, length.out = 201)  # x-coordinates
N <- 3  # no. of draws
col_list <- c("red", "blue", "black")  # for line colors

se_kernel <- function(x, y, sigma = 1, length = 1) {
    sigma^2 * exp(- (x - y)^2 / (2 * length^2))
}
K <- cov_matrix(x, se_kernel)
Y <- draw_samples(x, N, kernel_fn = se_kernel, length = 1)
plot(range(x), range(Y), xlab = "x", ylab = "y", type = "n",
     main = "SE kernel, length = 0.2")
for (n in 1:N) {
    lines(x, Y[, n], col = col_list[n], lwd = 1.5)
}

# Your stuff
rmvn <- function(n, mu=0, V = matrix(1)) {
  p <- length(mu)
  if(any(is.na(match(dim(V),p))))
    stop("Dimension problem!")
  D <- chol(V)
  t(matrix(rnorm(n*p), ncol=p)%*%D + rep(mu,rep(n,p)))
}
sim.exp <- function(n.time.max, rho, sigma.sq.t) {
  Sigma.eta <- matrix(NA, n.time.max, n.time.max)
  for (i in 1:n.time.max) {
    for (j in 1:n.time.max) {
      Sigma.eta[(i - 1) * n.time.max + j] <- sigma.sq.t * exp(-1.0 * (abs(i - j)^2 * rho^2))
      # Sigma.eta[(i - 1) * n.time.max + j] <- sigma.sq.t * exp(-rho * abs(i - j))
    } # j
  } # i
  # eta <- NA
  # eta <- rmvn(1, rep(0, n.time.max), Sigma.eta)
  eta <- mvrnorm(1, mu = rep(0, n.time.max), Sigma = Sigma.eta)
  return(list(eta = eta, Sigma = Sigma.eta))
}

test <- sim.exp(100, 3 / 100, 1)
# head(test$Sigma)
plot(test$eta, type = 'l')

# Generate AR1 ------------------------------------------------------------
ar1 <- function(n.time.max, rho, sigma.sq.t) {
  exponent <- abs(matrix(1:n.time.max - 1, nrow = n.time.max,
                         ncol = n.time.max, byrow = TRUE) - (1:n.time.max - 1))
  Sigma.eta <- sigma.sq.t * rho^exponent
  return(Sigma.eta)
}
n <- 50
out <- ar1(n, 0.9, 1)

plot.df <- data.frame(x = rep(1:n, each = n), 
		      y = rep(1:n, n),
		      val = c(out))
ggplot(data = plot.df, aes(x = x, y = y, fill = val)) +
  geom_raster() +
  theme_bw() +
  scale_fill_gradient2(midpoint = 0, low = '#B2182B', mid = 'white', high = '#2166AC',
  	               na.value = NA)

