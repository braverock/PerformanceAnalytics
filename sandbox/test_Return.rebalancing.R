library(PerformanceAnalytics)
data(edhec)
R <- edhec["1997",1:5]
colnames(R) <- c("CA", "CTA", "Distr", "EM", "EMN")

# Note: I verified these results by semi-random spot checks with the 
# spreadsheet calculations. Will add more comprehensive tests as time permits.

# Case 1: User inputs returns only
# Equally weighted portfolio with no rebalancing
out1 <- Return.rebalancing3(R)


# Case 2: User input weights with no rebalancing
out2 <- Return.rebalancing3(R, weights=c(0, 0.2, 0.4, 0.1, 0.3),
                            verbose=TRUE)


# Case 3: User input weights and rebalancing frequency
out3 <- Return.rebalancing3(R, weights=c(0, 0.2, 0.4, 0.1, 0.3), 
                          rebalance_on="quarters")

# Case 4: User input xts object for weights 
rebal_dates <- c("1996-12-31", "1997-03-31", "1997-06-30", "1997-09-30")
weights <- xts(matrix(1/ncol(R), nrow=length(rebal_dates), ncol=ncol(R)), 
               as.Date(rebal_dates))
out4 <- Return.rebalancing3(R, weights, value=5, verbose=TRUE)
all.equal(rowSums(out4$contribution), as.numeric(out4$returns))

out4a <- Return.rebalancing3(R, rebalance_on="quarters", value=5, verbose=TRUE)
all.equal(out4, out4a)

# out4 and out4a should match Peter's spreadsheet exactly

# Weights that start after first observation in returns
rebal_dates <- c("1997-03-31", "1997-06-30", "1997-09-30")
weights <- xts(matrix(1/ncol(R), nrow=length(rebal_dates), ncol=ncol(R)), as.Date(rebal_dates))
out4b <- Return.rebalancing3(R, weights)

# Case 5: Equally weighted portfolio with monthly rebalancing
out5 <- Return.rebalancing3(R, rebalance_on="months", value=1)

