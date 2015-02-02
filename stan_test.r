rm(list=ls())
## Load libraries
library(ggplot2)
library(rstan)
## Simulated values for a linear regression with two predictors
## Number of observations
n <- 1000
## Draw values from normal distribution
x1 <- rnorm(n,0,1)
x2 <- rnorm(n,5,10)
## Set parameter values
alpha <- 1.5
beta1 <- 2.500000
beta2 <- 3.000000
## Generate y
y <- alpha + (beta1 * x1)  + (beta2*x2) + rnorm(n)


## Set up data for Stan
N <- length(y) ## Number of rows
K <- ncol(X) ## Number of predictors
X <- as.matrix(cbind(1,x1,x2)) ## X matrix
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

## Estimate model
stan_mod <- stan(model_code = rs_code, data = stan_data, chains = 4)
## Extract model output
output <- extract(stan_mod, permuted = TRUE)
## Get beta means and credible intervals
betas <- (output$beta)
means <- apply(betas, 2, mean)
ci <- apply(betas, 2, quantile, c(.025,.975))

## Plot the output
plot.df <- as.data.frame(t(rbind(means, ci)))
colnames(plot.df) <- c('mean','lb','ub')
plot.df$var <- c('Intercept', 'beta 1', 'beta 2')

p <- ggplot(plot.df, aes(x = var, y = mean))
p <- p + geom_point()
p <- p + geom_errorbar(aes(y = mean, ymax = ub, ymin = lb,width =.1))
p <- p + labs(y="Coefficients with 95% credible intervals",x="Variable")
p <- p + theme_bw()
p <- p + coord_flip()
p
