
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
# FUNCTION:                RETURN DISTRIBUTION MEASURES:
#  nSkewness                Returns moment skewness
#  sampleSkewness           Returns sample skewness
#  nKurtosis                Returns moment kurtosis
#  excessKurtosis           Returns excess kurtosis
#  sampleKurtosis           Returns sample kurtosis
#  sampleExcessKurtosis     Returns sample excess kurtosis
#  jarqueBeraStatistic      Returns Jarque Bera statistics
################################################################################


nSkewness <-
function(periodPercentReturns)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns moment skewness
    
    # Example:
    #   nSkewness(R)
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns)) 
    
    # Data:
    R = periodPercentReturns
    n = NROW(R)
    sigmaP = nStandardDeviation(R)
    
    # Result:
    ans = sum( ((R-mean(R))/sigmaP)^3 ) / n    
    names(ans) = "Moment Skewness"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


sampleSkewness <-
function(periodPercentReturns)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns sample skewness
    
    # Example:
    #   sampleSkewness(R)
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns))
    
    # Data:
    R = periodPercentReturns
    n = NROW(R)
    sigmaP = nStandardDeviation(R)
    N = n / ((n-1)*(n-2))
    
    # Result:
    ans = sum( ((R-mean(R))/sigmaP)^3 ) * N
    names(ans) = "Sample Skewness"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


nKurtosis <-
function(periodPercentReturns)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns moment kurtosis
    
    # Example:
    #   nKurtosis(R)
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns))
    
    # Data:
    R = periodPercentReturns
    n = NROW(R)
    sigmaP = nStandardDeviation(R)
    
    # Result:
    ans = sum( ((R-mean(R))/sigmaP)^4 ) / n
    names(ans) = "Moment Kurtosis"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


excessKurtosis <-
function(periodPercentReturns)
{  
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns excess kurtosis
    
    # Example:
    #   excessKurtosis(R)
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns))
    
    # Data:
    R = periodPercentReturns
    
    # Result:
    ans = nKurtosis(R) - 3
    names(ans) = "Excess Kurtosis" 
    
    # Return Value: 
    ans  
}


# ------------------------------------------------------------------------------


sampleKurtosis <-
function(periodPercentReturns)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns sample kurtosis
    
    # Example:
    #   sampleKurtosis(R)
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns))
    
    # Data:
    R = periodPercentReturns
    n = NROW(R)
    sigmaP = nStandardDeviation(R)
    N = n*(n+1) / ((n-1)*(n-2)*(n-2))
    
    # Result:
    ans = sum( ((R-mean(R))/sigmaP)^4 ) * N
    names(ans) = "Sample Kurtosis"  
    
    # Return Value:
    ans  
}


# ------------------------------------------------------------------------------


sampleExcessKurtosis <-
function(periodPercentReturns)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns sample excess kurtosis
    
    # Example:
    #   sampleExcessKurtosis(R)
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns))
    
    # Data:
    R = periodPercentReturns
    n = NROW(R)
    sigmaP = sampleStandardDeviation(R)
    N1 = n*(n+1) / ((n-1)*(n-2)*(n-3))
    N2 = (n-1)^2 / ((n-2)*(n-3))
    
    # Result:
    ans = sum( ((R-mean(R))/sigmaP)^4 ) * N1 - 3 * N2
    names(ans) = "Sample Excess Kurtosis"  
    
    # Return Value:
    ans  
}


# ------------------------------------------------------------------------------


jarqueBeraStatistic <-
function(periodPercentReturns)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns Jarque Bera statistics
    
    # Example:
    #   jarqueBeraStatistic(R)

    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns))
    
    # Data:
    R = periodPercentReturns
    n = NROW(R)
    S = nSkewness(R)
    KE = excessKurtosis(R)
    
    # Result:
    ans = n * ( S^2 + KE^2/4) / 6
    names(ans) = "Jarque Bera Statistic"  
    
    # Return Value:
    ans  
}


################################################################################

