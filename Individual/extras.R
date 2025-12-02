library(dplyr)
library(readr)
library(stats)
library(mgcv)
library(rjson)
library(tidyr)
library(purrr)
library(stringr)
library(jsonlite)
library(R.basic)

negBin_ll <- function(params, data) {
  r <- params[1]
  p <- params[2]
  
  # Constraints
  if (r <= 0 || p <= 0 || p >= 1) return(Inf)
  
  x <- data
  
  # log PMF using gamma
  ll <- lgamma(x + r) - lgamma(r) - lgamma(x + 1) +
    r * log(p) + x * log(1 - p)
  
  return(-sum(ll))  # return negative log-likelihood
}

negBin <- function(x, r, p)
{
  return(choose(x + r - 1, x)*(1-p)^x*p^r)
}

options(digits = 15)

exampleMatch <- read.csv("matches/all_matches.csv")

inningsTotals <- aggregate(extras~match_id+innings, exampleMatch, sum)
scores <- inningsTotals$extras
hist(scores, breaks=20, freq=FALSE)

initial_parameters <- c(10, 0.5)

result <- optim(
  par = initial_parameters,
  fn = negBin_ll,
  data = scores,
  method = "L-BFGS-B",
  lower = c(1e-6, 1e-6),
  upper = c(1e6, 0.999999),
  control = list(maxit = 1000, trace = 1)
)

ys2 <- seq(1:30)
ys2 <- negBin(ys2, as.numeric(result$par[1]), as.numeric(result$par[2]))
lines(ys2)
