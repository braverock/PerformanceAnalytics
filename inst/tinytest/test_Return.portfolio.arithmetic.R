
library(xts)

# Mock objects
data(edhec, package = "PerformanceAnalytics")
R_monthly <- edhec['199701/199704', 1:2]
equalweights_quarterly = xts(matrix(rep(0.5, 4), nrow=2), order.by=as.Date(c('1996-12-31', '1997-03-31')))
epsilon = 1e-10

# Test internal API: Does Return.portfolio.arithmetic rebalance via weights arg?
ret_ari <- PerformanceAnalytics:::Return.portfolio(R = R_monthly,
                                                   weights = equalweights_quarterly,
                                                   verbose = TRUE,
                                                   geometric=FALSE)

# Because weights is quarterly and R is monthly,
# we expect returned monthly BOP.Weights to float around
# based on returns in R and should observe max > min.
minwgt <- min(ret_ari$BOP.Weight)
maxwgt <- max(ret_ari$BOP.Weight)

expect_true(length(ret_ari$BOP.Weight) == 8)  # 4 Monthly weights for 2 assets.
expect_true((maxwgt - minwgt) > epsilon)  # Monthly BOP.Weights are floating.

