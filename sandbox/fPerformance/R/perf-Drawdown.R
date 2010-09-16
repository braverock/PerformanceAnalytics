
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
# FUNCTION:                    DRAWDOWN RISK MEASURES:
#  averageDrawdown              Returns average drawdown
#  maximumDrawdown              Returns maximum drawdown
#  continuousDrawdown           Returns continuous drawdown
#  largestIndividualDrawdown    Returns largest individual drawdown
#  averageNLargestDrawdown      Returns average of N largest drawdowns
#  recoveryTime                 Returns recovery time
#  drawdownDeviation            Returns drawdown deviation
#  ulcerIndex                   Returns Ulcer index
#  painIndex                    Returns Pain index
#  calmarRatio                  Returns annualized Calmar Ratio
#  sterlingRatio                Returns annualised Sterling ratio
#  sterlingCalmarRatio          Returns annualised Sterling-Calmar ratio
#  burkeRatio                   Returns annualised Burke ratio
#  modBurkeRatio                Returns annualised modified Burke ratio
#  martinRatio                  Returns annualised Martin ratio
#  painRatio                    Returns annualised Pain ratio
#  lakeRatio                    Returns annualised Lake ratio
#  peakRatio                    Returns annualised peak ratio
################################################################################


averageDrawdown <- 
function(periodPercentReturns)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns average drawdown
    
    # Example:
    #   averageDrawdown(R)
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns))
    
    # Data:
    R = periodPercentReturns
    D = -100 * drawdowns(R/100)
    
    # Result:
    ans = mean(D[D > 0, ])
    names(ans) = "Average Drawdown"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


maximumDrawdown <-
function(periodPercentReturns)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns maximum drawdown
    
    # Example:
    #   maximumDrawdown(R)
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns))
    
    # Data:
    R = periodPercentReturns
    D = -100 * drawdowns(R/100)
    
    # Result:
    ans = max(D)
    names(ans) = "Maximum Drawdown"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


continuousDrawdowns <-
function(periodPercentReturns)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns maximum drawdown

    # Example:
    #   continuousDrawdowns(R)

    # FUNCTION:

    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns))

    # Data:
    R = periodPercentReturns
    last = rev(as.vector(R))[1]
    x = cumsum(R)
    n <- length(x)
    
    # Cpmpute pits and peaks:
    #     adapted from pastecs::turnpoints()
    diffs <- c(x[1] - 1, x[1:(n - 1)]) != x
    uniques <- x[diffs]
    n2 <- length(uniques)
    poss <- (1:n)[diffs]
    exaequos <- c(poss[2:n2], n + 1) - poss - 1
    if (n2 < 3) {
        warning("Less than 3 unique values, no calculation!")
        nturns <- NA
        firstispeak <- FALSE
        peaks <- rep(FALSE, n2)
        pits <- rep(FALSE, n2)
    } else {
        m <- n2 - 2
        ex <- matrix(uniques[1:m + rep(3:1, rep(m, 3)) - 1], m)
        peaks <- c(FALSE, apply(ex, 1, max, na.rm = TRUE) == ex[, 2], FALSE)
        pits <- c(FALSE, apply(ex, 1, min, na.rm = TRUE) == ex[, 2], FALSE)
        tpts <- peaks | pits
        if (sum(tpts) == 0) {
            nturns <- 0
            firstispeak <- FALSE
            peaks <- rep(FALSE, n2)
            pits <- rep(FALSE, n2)
        }
    }

    # Result:
    if (last <= 0) {
        ans = x[peaks] - c(x[pits], rev(x)[1])
    } else if (last > 0){
        ans = x[peaks] - x[pits]
    }
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


largestIndividualDrawdown =
function(periodPercentReturns)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns largest individual drawdown

    # Example:
    #   largestIndividualDrawdown(R)

    # FUNCTION:

    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns))

    # Data:
    R = periodPercentReturns

    # Result:
    ans = max(continuousDrawdowns(R))
    names(ans) = "largest individual drawdown"
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


averageNLargestDrawdown <-
function(periodPercentReturns, n = 1)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns average of N largest drawdowns

    # Example:
    #   averageNLargestDrawdown(R, 3)

    # FUNCTION:

    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns))

    # Data:
    R = periodPercentReturns

    # Result:
    ans = mean(rev(sort(continuousDrawdowns(R)))[1:n])
    names(ans) = paste("Average of N =", n, "largest Drawdowns")
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


recoveryTime <-
function(periodPercentReturns)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns recovery time
    
    # Example:
    #   recoveryTime(R)
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns))
    
    # Data:
    R = periodPercentReturns
    D = -100 * drawdowns(R/100)
    
    # Result:
    ans = NA
    names(ans) = "Recovery Time"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


drawdownDeviation <-
function(periodPercentReturns)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns drawdown deviation
    
    # Example:
    #   drawdownDeviation(R)
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns))
    
    # Data:
    R = periodPercentReturns
    D = -100 * drawdowns(R/100)
    
    # Result:
    ans =  NA
    names(ans) = "Drawdown Deviation"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


ulcerIndex <-
function(periodPercentReturns)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns Ulcer index
    
    # Example:
    #   ulcerIndex(R)
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns))
    
    # Data:
    R = periodPercentReturns
    D = -100 * drawdowns(R/100)
    
    # Result:
    ans = sqrt(mean(D^2))
    names(ans) = "Ulcer Index"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


painIndex <-
function(periodPercentReturns)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns Pain index
    
    # Example:
    #   painIndex(R)
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns))
    
    # Data:
    R = periodPercentReturns
    D = -100 * drawdowns(R/100)
    
    # Result:
    ans = mean(abs(D))
    names(ans) = "Pain Index"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------
  
    
calmarRatio <-
function(periodPercentReturns, targetReturn = 0,
    method = c("geometric", "arithmetic"), 
    scale = c("quarterly", "monthly", "weekly", "daily"))
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns annualized Calmar Ratio
    
    # Example:
    #   calmarRatio(R, 0, "g", "m"); calmarRatio(R, 0, "a", "m")

    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns))
    method = match.arg(method)
    Scale = .scale(match.arg(scale))
    
    # Data:
    R = periodPercentReturns 
    targetReturn = targetReturn * Scale
    Return = annualisedReturn(R, method, scale) - targetReturn
    Risk = maximumDrawdown(R)
    
    # Result:
    ans = Return / Risk
    names(ans) = "Calmar Ratio"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


sterlingRatio <-
function(periodPercentReturns, riskFreeRate = 0,
    method = c("geometric", "arithmetic"), 
    scale = c("quarterly", "monthly", "weekly", "daily"))
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns annualised Sterling ratio
    
    # Example:
    #  sterlingRatio(R, 0, "g", "m"); sterlingRatio(R, 0, "a", "m")

    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns))    
    method = match.arg(method)
    Scale = .scale(match.arg(scale))
    
    # Data:
    R = periodPercentReturns 
    rP = annualisedReturn(R, method, scale)
    rF = riskFreeRate * Scale
    D = 100 * drawdowns(R/100)
    Return = rP -rF
    Risk =  abs(mean(D))
    
    # Result:
    ans = Return / Risk
    names(ans) = "Sterling Ratio"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


sterlingCalmarRatio <-
function(periodPercentReturns, riskFreeRate = 0,
    method = c("geometric", "arithmetic"), 
    scale = c("quarterly", "monthly", "weekly", "daily"))
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns annualised Sterling-Calmar ratio
    
    # Example:
    #   sterlingCalmarRatio(R, 0, "g", "m"); sterlingCalmarRatio(R, 0, "a", "m")

    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns))   
    method = match.arg(method)
    Scale = .scale(match.arg(scale))
    
    # Data:
    R = periodPercentReturns
    rP = annualisedReturn(R, method, scale)
    rF = riskFreeRate * Scale
    Return = rP -rF
    Risk = maximumDrawdown(R)
    
    # Result:
    ans = Return / Risk
    names(ans) = "Sterling-Calmar Ratio"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


burkeRatio <-
function(periodPercentReturns, targetReturn = 0,
    method = c("geometric", "arithmetic"), 
    scale = c("quarterly", "monthly", "weekly", "daily"))
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns annualised Burke ratio
    
    # Example:
    #  burkeRatio(R, 0, "g", "m"); burkeRatio(R, 0, "a", "m")

    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns))   
    method = match.arg(method)
    Scale = .scale(match.arg(scale))
    
    # Data:
    R = periodPercentReturns 
    rP = annualisedReturn(R, method, scale) - targetReturn
    rF = riskFreeRate * Scale
    D = 100 * drawdowns(R)
    Return = rP - rF
    Risk = sqrt(sum(D^2))
    
    # Result:
    ans = Return / Risk
    names(ans) = "Burke Ratio"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


modBurkeRatio <-
function(periodPercentReturns, targetReturn = 0,
    method = c("geometric", "arithmetic"), 
    scale = c("quarterly", "monthly", "weekly", "daily"))
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns annualised modified Burke ratio
    
    # Example:
    #  modBurkeRatio(R, 0, "g", "m"); modBurkeRatio(R, 0, "a", "m")

    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns))
    method = match.arg(method)
    Scale = .scale(match.arg(scale))
    
    # Data:
    R = periodPercentReturns
    rP = annualisedReturn(R, method, scale) - targetReturn
    rF = riskFreeRate * Scale
    n = nrow(R)
    D = 100 * drawdowns(R)
    Return = rP - rF
    Risk = sqrt(sum(D^2/n))
    
    # Result:
    ans = Return / Risk
    names(ans) = "Modified Burke Ratio"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


martinRatio <-
function(periodPercentReturns, targetReturn = 0,
    method = c("geometric", "arithmetic"), 
    scale = c("quarterly", "monthly", "weekly", "daily"))
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns annualised Martin ratio
    
    # Example:
    #  martinRatio(R, 0, "g", "m"); martinRatio(R, 0, "a", "m")

    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns))   
    method = match.arg(method)
    Scale = .scale(match.arg(scale))
    
    # Data:
    R = periodPercentReturns
    rP = annualisedReturn(R, method, scale) - targetReturn
    rF = riskFreeRate * Scale
    n = nrow(R)
    Dprime = 100 * drawdowns(R)
    Return = rP - rF
    Risk = sqrt(sum(Dprime^2/n))
    
    # Result:
    ans = Return / Risk
    names(ans) = "Martin Ratio"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


painRatio <-
function(periodPercentReturns, targetReturn = 0,
    method = c("geometric", "arithmetic"), 
    scale = c("quarterly", "monthly", "weekly", "daily"))
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns annualised Pain ratio
    
    # Example:
    #  xxxRatio(R, 0, "g", "m"); xxxRatio(R, 0, "a", "m")

    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns))  
    method = match.arg(method)
    Scale = .scale(match.arg(scale))
    
    # Data:
    R = periodPercentReturns
    rP = annualisedReturn(R, method, scale) - targetReturn
    rF = riskFreeRate * Scale
    n = nrow(R)
    Dprime = 100 * drawdowns(R)
    Return = rP - rF
    Risk = sum(Dprime^2) / n
    
    # Result:
    ans = Return / Risk
    names(ans) = "Pain Ratio"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


lakeRatio <-
function(periodPercentReturns, targetReturn = 0,
    method = c("geometric", "arithmetic"), 
    scale = c("quarterly", "monthly", "weekly", "daily"))
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns annualised Lake ratio
    
    # Example:
    #  lakeRatio(R, 0, "g", "m"); lakeRatio(R, 0, "a", "m")

    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns))
    method = match.arg(method)
    Scale = .scale(match.arg(scale))
    
    # Data:
    R = periodPercentReturns
    rP = annualisedReturn(R, method, scale) - targetReturn
    rF = riskFreeRate * Scale
    Return = rP - rF
    Risk = NA
    
    # Result:
    ans = Return / Risk
    names(ans) = "Lake Ratio"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


peakRatio <-
function(periodPercentReturns, targetReturn = 0,
    method = c("geometric", "arithmetic"), 
    scale = c("quarterly", "monthly", "weekly", "daily"))
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns annualised peak ratio
    
    # Example:
    #  peakRatio(R, 0, "g", "m"); peakRatio(R, 0, "a", "m")

    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns))
    method = match.arg(method)
    Scale = .scale(match.arg(scale))
    
    # Data:
    R = periodPercentReturns
    rP = annualisedReturn(R, method, scale) - targetReturn
    rF = riskFreeRate * Scale
    Return = rP - rF
    Risk = NA
    
    # Result:
    ans = Return / Risk
    names(ans) = "Peak Ratio"
    
    # Return Value:
    ans
}


################################################################################

