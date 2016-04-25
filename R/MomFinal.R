library(Momentum)
library(ggplot2)

data("monthlyreturns")
data("portfolios")
data("wmlreturns")
data("FFfactors")


# Cumulative returns from Jan 1999-Dec 2007
ggplot() + geom_line(aes(WML$port.num, WML$cumreturn))
