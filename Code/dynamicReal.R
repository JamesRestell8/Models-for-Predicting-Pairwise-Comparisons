library(dplyr)
library(readr)
library(stats)
library(mgcv)

options(digits = 15)

matches <- list.files(path="seasons/bettingSeasons/", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows

matches <- dplyr::select(matches, c("HomeTeam", "AwayTeam", "FTHG", "FTAG"))

phi <- function(t)
{
  return(exp(-0.0065 * t))
}

tauFunc <- function(lamd, mu, rho, x, y)
{
  if (x == 0 && y == 0)
  {
    return(1 - lamd * mu * rho)
  }
  if (x == 0 && y == 1)
  {
    return(1 + lamd * rho)
  }
  if (x == 1 && y == 0)
  {
    return(1 + mu * rho)
  }
  if (x == 1 && y == 1)
  {
    return(1 - rho)
  }
  return(1)
}

uniqueTeams <- unique(matches$HomeTeam)
teamsIndex <- setNames(seq_along(uniqueTeams), uniqueTeams)

logLikelihood <- function(parameters, data) {
  numTeams <- as.integer((length(parameters) - 2) / 2)
  
  attackStrengths <- parameters[1:numTeams]
  defenceStrengths <- parameters[(numTeams + 1):(numTeams * 2)]
  
  homeAdv <- parameters[length(parameters) - 1]
  rho <- parameters[length(parameters)]
  
  t <- nrow(data)
  ll <- 0
  
  for (tk in 1:nrow(data)) {
    home_team <- as.character(data$HomeTeam[tk])
    away_team <- as.character(data$AwayTeam[tk])
    x <- as.integer(data$FTHG[tk])
    y <- as.integer(data$FTAG[tk])
    
    i <- which(uniqueTeams == home_team)
    j <- which(uniqueTeams == away_team)
    
    lamd <- attackStrengths[i] * defenceStrengths[j] * homeAdv
    mu <- attackStrengths[j] * defenceStrengths[i]
    
    ll <- ll + (log(tauFunc(lamd, mu, rho, x, y) + 1e-7) - 
                  lamd + x * log(lamd) - 
                  mu + y * log(mu)) * phi(t - tk)
  }
  
  return(ll)
}

logLikelihood(rep(1, times=58), matches)

numTeams <- length(uniqueTeams)

# Initialize parameters (important: use reasonable starting values)
initial_attack <- rep(1, numTeams)      # Attack strengths = 1
initial_defence <- rep(1, numTeams)     # Defence strengths = 1
initial_homeAdv <- 1.3                  # Home advantage ~1.3
initial_rho <- 0                        # Correlation parameter

initial_parameters <- c(initial_attack, initial_defence, initial_homeAdv, initial_rho)

# Define bounds
lower_bounds <- c(
  rep(0.01, numTeams),      # Attack strengths > 0
  rep(0.01, numTeams),      # Defence strengths > 0
  0.01,                     # Home advantage > 0
  -0.99                     # Rho can be negative
)

upper_bounds <- c(
  rep(Inf, numTeams),       # Attack strengths
  rep(Inf, numTeams),       # Defence strengths
  Inf,                      # Home advantage
  0.99                      # Rho upper bound
)

result <- optim(
  par = initial_parameters,
  fn = function(params) -logLikelihood(params, matches),
  method = "L-BFGS-B",
  lower = lower_bounds,
  upper = upper_bounds,
  control = list(maxit = 1000, trace = 1),
)

# Extract MLE estimates
mle_parameters <- result$par

# Parse the parameters
numTeams <- length(uniqueTeams)
attackStrengths_mle <- mle_parameters[1:numTeams]
defenceStrengths_mle <- mle_parameters[(numTeams + 1):(numTeams * 2)]
homeAdv_mle <- mle_parameters[length(mle_parameters) - 1]
rho_mle <- mle_parameters[length(mle_parameters)]

# Name them for clarity
names(attackStrengths_mle) <- uniqueTeams
names(defenceStrengths_mle) <- uniqueTeams

# Display results
cat("\n=== MLE Results ===\n")
cat("Convergence:", result$convergence, "(0 = success)\n")
cat("Log-Likelihood:", -result$value, "\n\n")

cat("Home Advantage:", homeAdv_mle, "\n")
cat("Rho (correlation):", rho_mle, "\n\n")

cat("Attack Strengths:\n")
print(sort(attackStrengths_mle, decreasing = TRUE))

cat("\nDefence Strengths:\n")
print(sort(defenceStrengths_mle, decreasing = TRUE))

# Optional: Create a summary data frame
team_strengths <- data.frame(
  Team = uniqueTeams,
  Attack = attackStrengths_mle,
  Defence = defenceStrengths_mle
) %>% arrange(desc(Attack))

print(team_strengths)

library(ggplot2)
library(ggrepel)

ggplot(team_strengths, aes(x = Defence, y = Attack)) +
  geom_point(size = 2, color = "blue") +
  geom_text_repel(aes(label = Team), size = 3, max.overlaps = 20) +
  scale_x_reverse() +
  labs(title = "Team Attack vs Defence Strengths",
       x = "Defence Strength",
       y = "Attack Strength") +
  theme_minimal()

