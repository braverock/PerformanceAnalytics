
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General 
# Public License along with this library; if not, write to the 
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
# MA  02111-1307  USA


################################################################################
# FUNCTION:                    VALUE AT RISK MEASURES:
#  normalVaR                    Returns normal VaR
#  normalVaRRatio               Returns normal VaR ratio
#  normalRewardToVaR            Returns annualised normal reward to VaR ratio
#  conditionalVaR               Returns unnualised conditional VaR
#  conditionalSharpeRatio       Returns conditional VaR Sharpe ratio
#  modifiedVaR                  Returns annualised Cornish-Fishers modified VaR
#  modSharpeRatio               Returns annualised modified Sharpe ratio
################################################################################


normalVaR <- 
function(periodPercentReturns, probability = 0.95)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns normal VaR
    
    # Example:
    #   normalVaR(R)
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns))
    
    # Data:
    R = periodPercentReturns
    
    # Result:
    ans  = mean(R) - qnorm(probability) * nVariance(R)
    names(ans) = paste(probability, "normal VaR")

    # Return Value:
    ans
}



# ------------------------------------------------------------------------------


normalVaRRatio <- 
function(periodPercentReturns, probability = 0.95, nAssets = 1)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns normal VaR ratio
    
    # Example:
    #   normalVaRRatio(R)
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns))
    
    # Data:
    R = periodPercentReturns
    normalVaR = normalVaR(R, probability)
    
    # Result:
    ans  = normalVaR / nAssets
    names(ans) = paste(probability, "normal VaR Ratio")

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


normalRewardToVaR <-
function(periodPercentReturns, targetReturn = 0, probability = 0.95, 
    method = c("geometric", "arithmetic"), 
    scale = c("quarterly", "monthly", "weekly", "daily"), nAssets = 1)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns annualised normal reward to VaR ratio
    
    # Example:
    #   normalRewardToVaR(R, 0, 0.95, "g", "m")
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns))  
    method = match.arg(method)
    Scale = .scale(match.arg(scale))  
    
    # Data:
    R = periodPercentReturns 
    targetReturn = targetReturn * Scale
    Return = annualisedReturn(R, method, scale) - targetReturn
    Risk = normalVaRRatio(R, probability)
    
    # Result:
    ans = Return / Risk
    names(ans) = paste(probability, "Normal Reward to VaR")
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


conditionalVaRRatio <-
function(periodPercentReturns, targetReturn = 0, probability = 0.95,
    method = c("geometric", "arithmetic"), 
    scale = c("quarterly", "monthly", "weekly", "daily"))
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns unnualised conditional VaR
    
    # Example:
    #   conditionalVaRRatio(R, 0, "g", "m"); conditionalVaRRatio(R, 0, "a", "m")

    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns)) 
    method = match.arg(method)
    Scale = .scale(match.arg(scale))
    
    # Data:
    R = periodPercentReturns 
    Return = annualisedReturn(R, method, scale) - targetReturn * Scale
    Risk = 
    
    # Result:
    ans = Return / Risk
    names(ans) = "Conditional VaR Ratio"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


conditionalSharpeRatio <-
function(periodPercentReturns, targetReturn = 0, probability = 0.95,
    method = c("geometric", "arithmetic"), 
    scale = c("quarterly", "monthly", "weekly", "daily"))
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns conditional VaR Sharpe Ratio
    
    # Example:
    #   conditionalSharpeRatio(R, 0, "g", "m"); conditionalSharpeRatio(R, 0, "a", "m")

    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns))    
    method = match.arg(method)
    Scale = .scale(match.arg(scale))
    
    # Data:
    R = periodPercentReturns 
    targetReturn = targetReturn * Scale
    Return = annualisedReturn(R, method, scale) - targetReturn
    Risk = 
    
    # (rp-rf) / conditionalVaR
    
    # Result:
    ans = Return / Risk
    names(ans) = "Conditional Sharpe Ratio"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------

    
modifiedVaR <-
function(periodPercentReturns, targetReturn = 0, probability = 0.95,
    method = c("geometric", "arithmetic"), 
    scale = c("quarterly", "monthly", "weekly", "daily"))
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns annualised Cornish-Fishers modified VaR
    
    # Example:
    #  modifiedVaR(R, 0, 0.95, "g", "m"); modifiedVaR(R, 0, 0.95, "a", "m")

    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns))    
    method = match.arg(method)
    Scale = .scale(match.arg(scale))
    
    # Data:
    R = periodPercentReturns
    M = mean(R)
    S = nSkewness(R)
    KE = excessKurtosis(R)
    V = nVariance(R)
    zc = -qnorm(probability)
    Return = annualisedReturn(R, method, scale) - targetReturn
    Risk = M + V * 
        ( zc + ((zc^2-1)/6)*S + ((zc^3-3*zc)/24)*KE - ((2*zc^3-5*zc)/36) * S^2 )  

    # Result:
    ans = Return / Risk
    names(ans) = "Cornish Fischer VaR"
    
    # Return Value:
    ans
}
    

# ------------------------------------------------------------------------------


modSharpeRatio <-
function(periodPercentReturns, targetReturn = 0, probability = 0.95,
    method = c("geometric", "arithmetic"), 
    scale = c("quarterly", "monthly", "weekly", "daily"))
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns annualised modified Sharpe ratio
    
    # Example:
    #  modSharpeRatio(R, 0, 0.95, "g", "m"); modSharpeRatio(R, 0, 0.95, "a", "m")

    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns))   
    method = match.arg(method)
    Scale = .scale(match.arg(scale))
    
    # Data:
    R = periodPercentReturns 
    Return = annualisedReturn(R, method, scale) - targetReturn * Scale
    Risk = modifiedVaR(R, targetReturn, probability, method, scale)
    
    # Result:
    ans = Return / Risk
    names(ans) = "Modified Sharpe Ratio"
    
    # Return Value:
    ans
}


################################################################################

