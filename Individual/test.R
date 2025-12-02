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
class(exampleMatch)

# Source - https://stackoverflow.com/a
# Posted by akrun, modified by community. See post 'Timeline' for change history
# Retrieved 2025-11-24, License - CC BY-SA 4.0

inningsTotals <- aggregate(runs_off_bat+extras~match_id+innings, exampleMatch, sum)
scores <- inningsTotals$`runs_off_bat + extras`
hist(scores, breaks=20, freq=FALSE)

pMOM <- mean(scores)/var(scores)
rMOM <- (pMOM * mean(scores)) / (1 - pMOM)

ys <- seq(1:260)
ys <- negBin(ys, rMOM, pMOM)
lines(ys)

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

result
ys2 <- seq(1:260)
ys2 <- negBin(ys2, as.numeric(result$par[1]), as.numeric(result$par[2]))
lines(ys2)

rMLE <- result$par[1]
pMLE <- result$par[2]

innings1Totals <- aggregate(runs_off_bat+extras~match_id+innings, exampleMatch[exampleMatch$innings==1,], sum)
scores1 <- innings1Totals$`runs_off_bat + extras`
hist(scores1, breaks=20, freq=FALSE)
mean(scores1)

pMOM1 <- mean(scores1)/var(scores1)
rMOM1 <- (pMOM1 * mean(scores1)) / (1 - pMOM1)

ys1 <- seq(1:260)
ys1 <- negBin(ys1, rMOM1, pMOM1)
lines(ys)

initial_parameters <- c(10, 0.5)

result1 <- optim(
  par = initial_parameters,
  fn = negBin_ll,
  data = scores1,
  method = "L-BFGS-B",
  lower = c(1e-6, 1e-6),
  upper = c(1e6, 0.999999),
  control = list(maxit = 1000, trace = 1)
)

ys12 <- seq(1:260)
ys12 <- negBin(ys12, as.numeric(result1$par[1]), as.numeric(result1$par[2]))
lines(ys12)

rMLE1 <- result1$par[1]
pMLE1 <- result1$par[2]
