profits <- read.csv("profitsBTTS.csv", header=FALSE, sep=",")

profits <- t(profits)
mean(profits)
var(profits)
hist(profits, breaks=20)
ts.plot(cumsum(profits))

t.test(profits, alternative="less")


kelly_stats <- function(profits, starting_bankroll, bets_per_year = NULL) {
  
  # Reconstruct bankroll before each bet
  bankroll_before <- starting_bankroll + c(0, cumsum(profits[-length(profits)]))
  
  # Check for ruin
  if (any(bankroll_before <= 0)) {
    stop("Bankroll hits zero or below — Kelly growth undefined (ruin).")
  }
  
  # Kelly log returns
  log_returns <- log(1 + profits / bankroll_before)
  
  # Core statistics
  g_per_bet <- mean(log_returns)
  g_per_bet_sd <- sd(log_returns)
  
  # Multiplicative growth per bet
  growth_per_bet <- exp(g_per_bet) - 1
  
  # Optional annualization
  if (!is.null(bets_per_year)) {
    g_annual <- g_per_bet * bets_per_year
    growth_annual <- exp(g_annual) - 1
  } else {
    g_annual <- NA
    growth_annual <- NA
  }
  
  # Drawdowns
  bankroll_after <- bankroll_before + profits
  peak <- cummax(bankroll_after)
  drawdowns <- (bankroll_after - peak) / peak
  max_drawdown <- min(drawdowns)
  
  # Calmar ratio (if annualized)
  calmar <- if (!is.null(bets_per_year)) {
    g_annual / abs(max_drawdown)
  } else {
    NA
  }
  
  return(list(
    g_per_bet = g_per_bet,
    growth_per_bet = growth_per_bet,
    g_annual = g_annual,
    growth_annual = growth_annual,
    log_return_sd = g_per_bet_sd,
    max_drawdown = max_drawdown,
    calmar_ratio = calmar
  ))
}
starting_bankroll <- 1000
results <- kelly_stats(
  profits = profits,
  starting_bankroll = starting_bankroll,
  bets_per_year = 500
)

print(results)
