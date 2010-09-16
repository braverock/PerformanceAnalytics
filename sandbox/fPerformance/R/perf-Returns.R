
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
# FUNCTION:                    RETURNS:
#  annualisedReturn             Returns annualised return
#  periodicReturn               Returns period return
#  averageReturn                A synonyme for the function periodReturn
# INTERNAL FUNCTION:           DESCRIPTION:
#  .scale                       Returns scale as integer number
################################################################################


annualisedReturn <- 
function(periodPercentReturns, 
    method = c("geometric", "arithmetic"), 
    scale = c("quarterly", "monthly", "weekly", "daily"))
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns annualised return
    
    # Example:
    #   annualisedReturn(R, "g", "m"); annualisedReturn(R, "a", "m")

    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns)) 
    method = match.arg(method)
    scale = .scale(match.arg(scale))
    
    # Data:
    R = periodPercentReturns
    f = scale / nrow(R) 
    
    # Result: 
    if(method == "geometric") {
        ans = 100 * ( prod(1 + R/100)^f - 1 )
    } else if (method == "arithmetic") {
        ans = f * sum(periodPercentReturns)  
    }
    names(ans) = "% annualised Return"
    
    # Return Value:
    ans
}
 

# ------------------------------------------------------------------------------


periodReturn <-
function(periodPercentReturns)  
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns period return
    
    # Example:
    #   periodReturn(R)
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns))
    
    # Data:
    R = periodPercentReturns
    
    # Result:
    ans = mean(R)
    names(ans) = "% mean Return per Period"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


averageReturn <- 
function(periodPercentReturns)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   A synonyme for the function periodReturn
    
    # Example:
    #   periodReturn(R)
    
    # FUNCTION:
    
    ans = periodReturn((periodPercentReturns))
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.scale <-
function(scale = c("quarterly", "monthly", "weekly", "daily"))
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns scale as integer number
    
    # FUNCTION:
    
    # Scale:
    scale = match.arg(scale)   
    Scale = c(4, 12, 52, 252)
    names(Scale) = c("quarterly", "monthly", "weekly", "daily")
    ans = Scale[scale]
    
    # Return Value:
    ans
}


################################################################################

