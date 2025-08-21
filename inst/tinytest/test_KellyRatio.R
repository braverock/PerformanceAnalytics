library(xts)
library(PerformanceAnalytics)

# Mock data
set.seed(123)
dates <- as.Date("2000-01-01") + 0:11
R1 <- xts(rnorm(12, 0.01, 0.02), dates)
R2 <- xts(rnorm(12, 0.005, 0.015), dates)
R_multi <- cbind(R1, R2)
colnames(R_multi) <- c("Asset1", "Asset2")

Rf_const <- 0.001   # constant risk-free rate per period
Rf_xts   <- xts(rep(0.001, 12), dates)  # time series risk-free
names(Rf_xts) <- "RiskFree"

## --- Tests ---

# 1. Default (half-Kelly) single column
res1 <- KellyRatio(R1, Rf = Rf_const)
expect_equal(nrow(res1), 1)
expect_equal(rownames(res1), "Kelly Ratio")

# 2. Full Kelly (method = "full")
res2 <- KellyRatio(R1, Rf = Rf_const, method = "full")
res_half <- KellyRatio(R1, Rf = Rf_const, method = "half")
expect_equal(res2, res_half * 2)

# 3. Multiple assets
res3 <- KellyRatio(R_multi, Rf = Rf_const)
expect_equal(ncol(res3), 2)
expect_equal(colnames(res3), c("Asset1", "Asset2"))

# 4. Time series Rf input
res4 <- KellyRatio(R_multi, Rf = Rf_xts)
expect_equal(dim(res4), c(1, 2))

# 5. NA handling
R1_withNA <- R1
R1_withNA[3] <- NA
res5 <- KellyRatio(R1_withNA, Rf = Rf_const)
expect_false(any(is.na(res5)))   # still computes because na.rm=TRUE

