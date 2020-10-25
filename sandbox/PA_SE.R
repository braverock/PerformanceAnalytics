# =====================================
# Application Script -
# Standard Errors for Hedge Funds Data
# =====================================

# Loading data from PerformanceAnalytics
data(edhec)
# Changing the data colnames
names(edhec)  =  c("CA", "CTA", "DIS", "EM", "EMN",
                   "ED", "FIA", "GM", "LS", "MA",
                   "RV", "SS", "FOF")

# Mean 
out <- mean.arithmetic(edhec, SE = TRUE)
t(out)

# StdDev 
out <- StdDev(edhec, SE = TRUE)
t(out)
out <- StdDev(edhec, SE = TRUE, portfolio_method="single", clean="geltner")
t(out)

# SemiSD 
out <- DownsideDeviation(edhec, SE = TRUE)
t(out)
out <- SemiDeviation(edhec, SE = TRUE)
t(out)
out <- SemiSD(edhec, SE = TRUE)
t(out)

# LPM
out <- lpm(edhec, SE = TRUE, about_mean = FALSE)
t(out)

# ES
out <- ES(edhec, SE = TRUE, method="historical", invert=FALSE)
t(out)

# VaR
out <- VaR(edhec, SE = TRUE, method="historical", invert=FALSE)
t(out)

# SharpeRatio / ESratio / VaRratio / DownsideSharpeRatio
out <- SharpeRatio(edhec, SE = TRUE)
t(out)

# RachevRatio
out <- RachevRatio(edhec, SE = TRUE)
t(out)

# Omega
out <- Omega(edhec, SE = TRUE)
t(out)

# SortinoRatio
out <- SortinoRatio(edhec, SE = TRUE)
t(out)

# Downside Sharpe Ratio
out <- DownsideSharpeRatio(edhec, SE = TRUE)
t(out)


