rm(list=ls())
## Load Rstan library 
library(rstan)
## Simulated values for a linear regression with two predictors
n <- 1000
x1 <- rnorm(n,0,1)
x2 <- rnorm(n,5,10)
alpha <- 1.5
beta1 <- 2.500000
beta2 <- 3.000000
y <- alpha + (beta1 * x1)  + (beta2*x2) + rnorm(n)
X <- as.matrix(cbind(1,x1,x2))

## Set up data for Stan
N <- length(y) ## Number of rows
K <- ncol(X) ## Number of predictors
stan_data <- list(N = N, K = K, y = y, x = X) ## Data as a list

## Stan model
rs_code <- "
data {
  int<lower=0> N; // number of data items
  int<lower=0> K; // number of predictors
  matrix[N,K] x; // predictor matrix
  vector[N] y; // outcome vector
}
parameters {
  vector[K] beta; // coefficients for predictors
  real<lower=0> sigma; // error scale
}
model {
  y ~ normal(x * beta, sigma); // likelihood
}
"

stan_mod <- stan(model_code = rs_code, data = stan_data, chains = 4)
print(stan_mod)
output <- extract(stan_mod, permuted = TRUE)
names(output)
betas <- (output$beta)
means <- apply(betas, 2, mean)
ci <- apply(betas, 2, quantile, c(.025,.975))
colnames(t(rbind(means, ci)))
