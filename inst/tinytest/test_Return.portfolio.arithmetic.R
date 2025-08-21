library(xts)
library(PerformanceAnalytics)

data(edhec, package = "PerformanceAnalytics")
R_monthly <- edhec['199701/199704', 1:2]
epsilon <- 1e-10

# 1. NA handling -----------------------------------------------------------
R_na <- R_monthly
R_na[1,1] <- NA
expect_warning(out_na <- Return.portfolio(R_na, weights=c(0.5,0.5)))
expect_false(anyNA(out_na))

# 2. Frequency guardrails --------------------------------------------------
R_daily <- to.daily(R_monthly)
attr(R_daily, "frequency") <- NULL # fudge to mimic freq
expect_error(Return.portfolio(R_daily, weights=c(0.5,0.5), freq="seconds"),
             "daily frequency or higher")

# 3. weights = NULL (equal weights) ----------------------------------------
out_nullw <- Return.portfolio(R_monthly, weights=NULL, geometric=FALSE)
expect_inherits(out_nullw, "xts")

# 4a. weights as vector, no rebalance -------------------------------------
out_vec_noreb <- Return.portfolio(R_monthly, weights=c(0.5,0.5),
                                  rebalance_on=NA, geometric=FALSE)
expect_equal(ncol(out_vec_noreb), 1)

# 4b. weights as vector, monthly rebalance --------------------------------
out_vec_reb <- Return.portfolio(R_monthly, weights=c(0.5,0.5),
                                rebalance_on="months", geometric=FALSE)
expect_inherits(out_vec_reb, "xts")

# 5. weights as xts with mismatched cols triggers warning -----------------
w_bad <- xts(matrix(0.5, nrow=1, ncol=1), order.by=as.Date("1996-12-31"))
colnames(w_bad) <- "bogus"
expect_warning(out_bad <- Return.portfolio(R_monthly, weights=w_bad, geometric=FALSE))

# 6. Geometric branch ------------------------------------------------------
out_geo <- Return.portfolio(R_monthly, weights=c(0.5,0.5), geometric=TRUE)
expect_inherits(out_geo, "xts")

# 7. Residual handling in geometric ----------------------------------------
w_not1 <- xts(matrix(c(0.6,0.3), nrow=1), order.by=as.Date("1996-12-31"))
colnames(w_not1) <- colnames(R_monthly)
expect_warning(out_resid <- Return.portfolio(R_monthly, weights=w_not1, geometric=TRUE))
expect_true("Residual" %in% colnames(out_resid))

# 8. wealth.index = TRUE ---------------------------------------------------
out_wi <- Return.portfolio(R_monthly, weights=c(0.5,0.5), geometric=TRUE, wealth.index=TRUE)
expect_true(all(out_wi >= 0))

# 9. contribution = TRUE ---------------------------------------------------
out_contrib <- Return.portfolio(R_monthly, weights=c(0.5,0.5), contribution=TRUE, geometric=FALSE)
expect_true(ncol(out_contrib) > 1)

# 10. verbose = TRUE -------------------------------------------------------
out_verbose <- Return.portfolio(R_monthly, weights=c(0.5,0.5), verbose=TRUE, geometric=TRUE)
expect_true(is.list(out_verbose))
expect_true("returns" %in% names(out_verbose))

# 11. stop condition (last date before rebalance) --------------------------
R_short <- edhec['199701/199912', 1:2]
w_future <- xts(matrix(c(0.5,0.5), nrow=1), order.by=as.Date("2020-12-31"))
expect_error(Return.portfolio(R_short, weights=w_future), "last date")

