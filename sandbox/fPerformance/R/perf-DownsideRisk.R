

################################################################################
# FUNCTION:                    DOWNSIDE RISK MEASURES:
#  downsideRisk                 Returns downside risk
#  downsideVariance             Returns downside variance
#  downsidePotential            Returns downside potential
#  upsideRisk                   Returns upside risk
#  upsideVariance               Returns upside variance
#  upsidePotential              Returns upside potential
#  downsideFrequency            Returns downside frequency
#  upsideFrequency              Returns upside frequency
#  omegaRatio                   Returns Omega ratio
#  bernardoLedoitRatio          Returns Bernardo-Ledoit ratio
#  dRatio                       Returns d ratio
#  omegaSharpeRatio             Returns Omega Sharpe ratio
#  sortinoRatio                 Returns Sortino ratio
#  kappaRatio                   Returns kappa ratio
#  upsidePotentialRatio         Returns upside potential ratio
#  volatilitySkewness           Returns volatility skewness
#  variabilitySkewness          Returns variability skewness
#  adjustedSharpeRatio          Returns adjusted Sharpe ratio
#  skewnessKurtosisRatio        Returns skewness kurtosis ratio
#  prospectRatio                Returns prospect ratio
################################################################################


downsideRisk <-
function(periodPercentReturns, targetReturn = 0)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns downside risk
    
    # Example:
    #   downsideRisk(R)
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns))
    
    # Data:
    Returns = periodPercentReturns - targetReturn
    n = nrow(Returns)
    Returns = Returns[Returns < 0, ]
    
    # Result:
    ans = sqrt(sum(Returns^2)/n)
    names(ans) = "% Downside Risk per Period"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


downsideVariance <-
function(periodPercentReturns, targetReturn = 0)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns downside variance
    
    # Example:
    #   downsideVariance
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns))
    
    # Data:
    Returns = periodPercentReturns - targetReturn
    n = nrow(Returns)
    Returns = Returns[Returns < 0, ]
    
    # Result:
    ans = sum(Returns^2)/n
    names(ans) = "Downside Variance per Period"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


downsidePotential <-
function(periodPercentReturns, targetReturn = 0)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns downside potential
    
    # Example:
    #   xxx
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns))
    
    # Data:
    Returns = periodPercentReturns - targetReturn
    n = nrow(Returns)
    Returns = Returns[Returns < 0, ]
    
    # Result:
    ans = sum(Returns)/n
    names(ans) = "% Downside Risk per Period"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


upsideRisk <-
function(periodPercentReturns, targetReturn = 0)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns upside risk
    
    # Example:
    #   upsideRisk(R, 0)
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns))
    
    # Data:
    Returns = periodPercentReturns - targetReturn
    n = nrow(Returns)
    Returns = Returns[Returns > 0, ]
    
    # Result:
    ans = sqrt(sum(Returns^2)/n)
    names(ans) = "% Upside Risk per Period"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


upsideVariance <-
function(periodPercentReturns, targetReturn = 0)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns upside variance
    
    # Example:
    #   upsideVariance(R)
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns))
    
    # Data:
    Returns = periodPercentReturns - targetReturn
    n = nrow(Returns)
    Returns = Returns[Returns > 0, ]
    
    # Result:
    ans = sum(Returns^2)/n
    names(ans) = "Upside Variance per Period"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


upsidePotential <-
function(periodPercentReturns, targetReturn = 0)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns upside potential
    
    # Example:
    #   upsidePotential(R)
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns))
    
    # Data:
    Returns = periodPercentReturns - targetReturn
    n = nrow(Returns)
    Returns = Returns[Returns > 0, ]
    
    # Result:
    ans = sum(Returns)/n
    names(ans) = "% Upside Potential per Period"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


downsideFrequency <-
function(periodPercentReturns, targetReturn = 0)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns downside frequency
    
    # Example:
    #   downsideFrequency(R)
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns))
    
    # Data:
    Returns = periodPercentReturns - targetReturn
    Returns = Returns[Returns < 0, ]
    nDownside = nrow(Returns)
    
    # Result:
    ans = nDownside / n
    names(ans) = "Downside Frequency"
    
    # Return Value:
    ans 
}


# ------------------------------------------------------------------------------


upsideFrequency <-
function(periodPercentReturns, targetReturn = 0)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns upside frequency
    
    # Example:
    #   upsideFrequency(R)
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns))
    
    # Data:
    Returns = periodPercentReturns - targetReturn
    n = nrow(Returns)
    Returns = Returns[Returns < 0, ]
    nUpside = nrow(Returns)
    
    # Result:
    ans = nDownside / n
    names(ans) = "Upside Frequency"
    
    # Return Value:
    ans 
}


# ------------------------------------------------------------------------------


omegaRatio <-
function(periodPercentReturns, targetReturn = 0)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns Omega ratio
    
    # Example:
    #   omegaRatio(R, 0)
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns))
    
    # Data:
    Returns = periodPercentReturns - targetReturn
    Return1 = sum(Returns[Returns > 0, ])
    Return2 = sum(Returns[-Returns > 0, ])
    
    # Result:
    ans = Return1 / Return2
    names(ans) = "Omega Ratio"
    
    # Return Value:
    ans 
}


# ------------------------------------------------------------------------------


bernardoLedoitRatio <-
function(periodPercentReturns)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns Bernardo-Ledoit ratio
    
    # Example:
    #   bernardoLedoitRatio(R)
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns))
    
    # Data:
    R = periodPercentReturns
    targetReturn = 0
    
    # Result:
    ans = omegaRatio(periodPercentReturns, targetReturn)
    names(ans) = "Bernardo Ledoit Ratio"
    
    # Return Value:
    ans 
}


# ------------------------------------------------------------------------------


dRatio <-
function(periodPercentReturns)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns d ratio
    
    # Example:
    #   dRatio(R)
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns))
    
    # Data:
    R = periodPercentReturns
    nd = nrow(R[R < 0, ])
    nu = nrow(R[R > 0, ])
    
    # Result:
    ans = (nd/nu) / bernardoLedoitRatio(R)
    names(ans) = "d Ratio"
    
    # Return Value:
    ans 
}


# ------------------------------------------------------------------------------


omegaSharpeRatio <-
function(periodPercentReturns, targetReturn = 0)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns Omega Sharpe ratio
    
    # Example:
    #   omegaSharpeRatio(R)
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns))
    
    # Data:
    Return = averageReturn(periodPercentReturns) - targetReturn
    Returns = targetReturn - periodPercentReturns
    Returns = Returns[Returns > 0]
    n = NROW(Returns)
    Risk = sum(Returns) / n
    
    # Result:
    ans = Return / Risk 
    names(ans) = "Omega Sharpe Ratio"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


sortinoRatio <-
function(periodPercentReturns, targetReturn = 0)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns Sortino ratio
    
    # Example:
    #   sortinoRatio(R)
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns))
    
    # Data:
    Return = averageReturn(periodPercentReturns) - targetReturn
    Risk = downsideRisk(periodPercentReturns, targetReturn)
    
    # Result:
    ans = Return / Risk
    names(ans) = "Sortino Ratio"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


kappaRatio <-
function(periodPercentReturns, targetReturn = 0, a = 1)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns kappa ratio
    
    # Example:
    #   kappaRatio(R)
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns))
    
    # Data:
    Return = averageReturn(periodPercentReturns) - targetReturn
    Returns = targetReturn - periodPercentReturns
    Returns = Returns[Returns > 0]^a
    n = NROW(Returns)
    Risk = sum(Returns) / n
    
    # Result:
    ans = Return / Risk
    names(ans) = paste("a =", kappa, "Kappa Ratio")
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


upsidePotentialRatio <-
function(periodPercentReturns, targetReturn = 0)
{   
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns upside potential ratio
    
    # Example:
    #   upsidePotentialRatio(R)
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns))
    
    # Data:
    R = periodPercentReturns
    Returns = targetReturn - R
    Returns = Returns[Returns > 0] 
    Returns = sum(Returns) / n
    Risk = downsideRisk(R, targetReturn)
    
    # Result:
    ans = Return / Risk 
    names(ans) = "Upside Potential Ratio"
   
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


volatilitySkewness <-
function(periodPercentReturns, targetReturn = 0)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns volatility skewness
    
    # Example:
    #   volatilitySkewness(R)
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns))
    
    # Data:
    
    # Result:
    ans = 
        upsideVariance(periodPercentReturns, targetReturn = 0) /
        downsideVariance(periodPercentReturns, targetReturn = 0)
    names(ans) = "Volatility Skewness"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


variabilitySkewness <-
function(periodPercentReturns, targetReturn = 0)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns variability skewness
    
    # Example:
    #   variabilitySkewness(R)
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns))
    
    # Data:
    R = periodPercentReturns
    
    # Result:
    ans = upsideRisk(R, targetReturn = 0) / downsideRisk(R, targetReturn = 0)
    names(ans) = "Variability Skewness"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


adjustedSharpeRatio <-
function(periodPercentReturns, riskFreeRate = 0, 
    method = c("geometric", "arithmetic"), 
    scale = c("quarterly", "monthly", "weekly", "daily"))
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns adjusted Sharpe ratio
    
    # Example:
    #   adjustedSharpeRatio(R)
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns))
    
    # Data:
    SR = sharpeRatio(periodPercentReturns, riskFreeRate, method, scale)
    S = nSkewness
    
    # Result:
    ans = SR * ( 1  + S*SR/6 - (K-3)*(SR^2)/24 )
    names(ans) = "Adjusted Sharpe Ratio"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


skewnessKurtosisRatio <-
function(periodPercentReturns)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns skewness kurtosis ratio
    
    # Example:
    #   skewnessKurtosisRatio(R)
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns))
    
    # Data:
    R = periodPercentReturns
    
    # Result:
    ans = NA
    names(ans) = "Skewness Kurtosis Ratio"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


prospectRatio <-
function(periodPercentReturns)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns prospect ratio
    
    # Example:
    #   prospectRatio(R)
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns))
    
    # Data:
    R = periodPercentReturns
    
    # Result:
    ans = NA
    names(ans) = "Prospect Ratio"
    
    # Return Value:
    ans
}


################################################################################

