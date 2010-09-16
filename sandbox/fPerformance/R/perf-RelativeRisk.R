
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
# FUNCTION:                    RELATIVE RISK MEASURES:
#  trackingError                Returns tracking error
#  annualisedTrackingError      Returns annualised tracking error
#  informationRatio             Returns annualised information ratio
################################################################################


trackingError <-
function(periodPercentReturns, benchmarkReturns,
    method = c("geometric", "arithmetic"))
{   
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns tracking error
    
    # Example:
    #   trackingError(R, B, "g"); trackingError(R, B, "a")
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns)) 
    stopifnot(isUnivariate(benchmarkReturns)) 
    method = match.arg(method)

    # Data:
    R = periodPercentReturns
    B = benchmarkReturns
    RB = cbind(R, B)
    colnames(RB) = c("R", "B")
    R = RB[, 1]
    B = RB[, 2] 
    a = R - B
    g = 100 * ((1 + R/100) / (1 + B/100) - 1)
    
    # Result:
    if (method == "geometric") {
        ans = nStandardDeviation(g)
    } else if (method == "arithmetic") {
        ans = nStandardDeviation(a)
    }
    names(ans) = "Tracking Error"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


annualisedTrackingError <-
function(periodPercentReturns, benchmarkReturns,
    method = c("geometric", "arithmetic"),
    scale = c("quarterly", "monthly", "weekly", "daily"))
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns annualised tracking error

    # Example:
    #   annualisedTrackingError(R, B, "g", "m")
    #   annualisedTrackingError(R, B, "a", "m")

    # FUNCTION:

    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns))
    stopifnot(isUnivariate(benchmarkReturns))
    method = match.arg(method)
    Scale = .scale(match.arg(scale))

    # Data:
    R = periodPercentReturns
    B = benchmarkReturns
    RB = cbind(R, B)
    colnames(RB) = c("R", "B")
    R = RB[, 1]
    B = RB[, 2]
    a = R - B
    g = 100 * ((1 + R/100) / (1 + B/100) - 1)

    # Data:
    tError = trackingError(periodPercentReturns, benchmarkReturns, method)
    
    # Result:
    ans = tError * sqrt(Scale)
    scale
    names(ans) = "Annualized Tracking Error"

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


informationRatio <-
function(periodPercentReturns, benchmarkReturns,
    method = c("geometric", "arithmetic"), 
    scale = c("quarterly", "monthly", "weekly", "daily"))
{   
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns annualised information ratio
    
    # Example:
    #   informationRatio(R, B, "g", "m")
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns)) 
    stopifnot(isUnivariate(benchmarkReturns)) 
    method = match.arg(method)
    Scale = .scale(match.arg(scale))
    
    # Data:
    R = periodPercentReturns
    B = benchmarkReturns
    RB = cbind(R, B)
    colnames(RB) = c("R", "B")
    R = RB[, 1]
    B = RB[, 2]
    annR = annualisedReturn(R, method = "geometric", scale)
    annB = annualisedReturn(B, method = "geometric", scale)
    tError = annualisedTrackingError(R, B, method, scale)
    
    # Result:
    if (method == "geometric") {
      ans =  100*((1+annR/100)/(1+annB/100)-1) / tError
    } else if (method == "arithmetic") {
      ans = (annR-annB) / tError
    }
    names(ans) = "Information Ratio"
    
    # Return Value:
    ans
}


################################################################################

