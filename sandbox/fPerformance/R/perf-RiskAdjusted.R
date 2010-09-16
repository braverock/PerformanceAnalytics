
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
# FUNCTION:                RETURN ADJUSTED DOWNSIDE RISK MEASURES:
#  MS2Sortino               Returns the M2 Sortino ratio
#  omegaExcessReturn        Returns the annualised M2 Omega excess return
#  hurstIndex               Returns the Hurst Index
################################################################################


MS2Sortino <-
function(periodPercentReturns, targetReturn = 0,
    method = c("geometric", "arithmetic"), 
    scale = c("quarterly", "monthly", "weekly", "daily"))
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns the M2 Sortino ratio
    
    # Example:
    #  MS2Sortino(R, 0, "g", "m"); MS2Sortino(R, 0, "a", "m")

    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns))
    method = match.arg(method)
    Scale = .scale(match.arg(scale))
    
    # Data:
    R = periodPercentReturns 
    targetReturn = targetReturn * Scale
    Return = annualisedReturn(R, method, scale) - targetReturn
    Risk = NA
    
    # M2 for Sortino  
    # MS2 = rp + SortinoRatio * (SigmaDM - SigmaD)

    # Result:
    ans = Return / Risk
    names(ans) = "M2 Sortino Ratio"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


omegaExcessReturn <-
function(periodPercentReturns, targetReturn = 0,
    method = c("geometric", "arithmetic"), 
    scale = c("quarterly", "monthly", "weekly", "daily"))
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns the annualised M2 Omega excess return
    
    # Example:
    #   omegaExcessReturn(R, 0, "g", "m")
    #   omegaExcessReturn(R, 0, "a", "m")

    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns))    
    method = match.arg(method)
    Scale = .scale(match.arg(scale))
    
    # Data:
    R = periodPercentReturns 
    targetReturn = targetReturn * Scale
    Return = annualisedReturn(R, method, scale) - targetReturn
    Risk = NA
    
    # StyleBeta = SigmaD / SigmaMD
    # DownsideRiskAdjustedStyleBemchmark = 3 * StyleBeta * sigmaMD^2
    # OmegaExcessReturn = rp - DownsideRiskAdjustedStyleBemchmark

    # Result:
    ans = Return / Risk
    names(ans) = "Omega Excess Return"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.hurstIndex <-
function(periodPercentReturns, targetReturn = 0,
    method = c("geometric", "arithmetic"), 
    scale = c("quarterly", "monthly", "weekly", "daily"))
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns the Hurst Index
    
    # Example:
    #   .hurstIndex(R, 0, "g", "m")
    #   .hurstIndex(R, 0, "a", "m")

    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns))    
    method = match.arg(method)
    Scale = .scale(match.arg(scale))
    
    # Data:
    R = periodPercentReturns 
    targetReturn = targetReturn * Scale
    Return = annualisedReturn(R, method, scale) - targetReturn
    Risk = NA
    
    # Result:
    ans = Return / Risk
    names(ans) = "Hurst Index"
    
    # Return Value:
    ans
}


################################################################################

