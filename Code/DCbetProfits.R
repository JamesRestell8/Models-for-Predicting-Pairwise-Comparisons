profits <- read.csv("profitsMLE.csv", header=FALSE, sep=",")

profits <- t(profits)
mean(profits)
var(profits)
hist(profits)
ts.plot(cumsum(profits))

t.test(profits, alternative="less")
