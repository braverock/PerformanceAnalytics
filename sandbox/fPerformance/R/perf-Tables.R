
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
# FUNCTION:
#                         RISK MEASURES:
#  variabilityRiskTable    TABLE 4.1 Portfolio Variability
#                          TABLE 4.2 Benchmark Variability
#                          TABLE 4.3 Sharpe Ratio
#                          TABLE 4.4 M^2
#                          TABLE 4.5 Differential Return
#                         REGRESSION ANALYSIS:
#  covarianceRiskTable     TABLE 4.6 Covariance, correlation and regression beta
#  specificRiskTable       TABLE 4.7 Specific Risk
#                         RELATIVE RISK:
#  informationRiskTable    TABLE 4.8 Information ratio and excess return
#                          TABLE 4.9 Information ratio geometric excess return
#                         RETURN DISTRIBUTIONS:

#                         RISK ADJUSTED PERFORMANCE MEASURES:
#                          TABLE 4.10 Skewness and kurtosis
#  infoTable
#  distStatisticsTable
#                         DRAWDOWN:
#  drawdownRiskTable       TABLE 4.11 Drawdown statistics
#                         DOWNSIDE RISK:
#  downsideRiskTable       TABLE 4.12 Portfolio downside risk
#                          TABLE 4.13 Benchmark downside risk
#                         VALUE AT RISK:
#                         RETURN ADJUSTED FOR DOWNSIDE RISK:
################################################################################


variabilityRiskTable <- 
function(periodPercentReturns, 
    scale = c("quarterly", "monthly", "weekly", "daily"))
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   TABLE 4.1/2 - Portfolio/Benchmark Variability Table 
    
    # Example:
    #   variabilityRiskTable(R, "m")
    #   variabilityRiskTable(B, "m")  
    
    # FUNCTION:
    
    R = periodPercentReturns
    
    Table = rbind(
        "average Return" = averageReturn(R)[[1]],
        "mean Absolute Deviation" = meanAbsoluteDeviation(R)[[1]],
        "standard Deviation" = nStandardDeviation(R)[[1]],
        "annualised Standard Deviation" = 
            annualisedStandardDeviation(R, scale)[[1]])
    colnames(Table) = ""
    ans = round(Table, 2)
    
    # Return Value:
    ans
}
 
    
#  -----------------------------------------------------------------------------


sharpeRatioTable <- 
function(annualizedReturn, annualizedRisk, riskFreeRate = 0)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   TABLE 4.3 - Sharpe Ratio Table 
    
    # FUNCTION:
    
    sharpeRatio = (annualizedReturn - riskFreeRate) / annualizedRisk
    
    # Return Value:
    sharpeRatio
}


#  -----------------------------------------------------------------------------


mSquaredTable <- 
function(annualizedReturn, annualizedRisk, riskFreeRate = 0)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   TABLE 4.4 - M Squared Table 
    
    # FUNCTION:
    
    # Return Value:
    NA
}


#  -----------------------------------------------------------------------------


differentialReturnTable <- 
function(annualizedReturn, annualizedRisk, riskFreeRate = 0)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   TABLE 4.5 - Differential Return 
    
    # FUNCTION:
    
    # Return Value:
    NA
}


#  -----------------------------------------------------------------------------


covarianceRiskTable <- 
function(periodPercentReturns, benchmarkReturns, riskFreeRate = 0)
{    
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   TABLE 4.6 - Portfolio Covariance Table
    
    # Example:
    #   covarianceRiskTable(R, B, 0)  
    
    # FUNCTION:
    
    # Settings:
    R = periodPercentReturns
    B = benchmarkReturns
    rF = riskFreeRate
    
    # Table:
    Table = rbind(
        "Covariance" = nCovariance(R, B)[[1]],
        "Correlation" = nCorrelation(R, B)[[1]],
        "Regression Beta" = regressionCAPM(R, B, rF)[[2]],
        "Regression Alpha" = regressionCAPM(R, B, rF)[[1]])
    colnames(Table) = ""
    ans = round(Table, 2)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


specificRiskTable <- 
function(periodPercentReturns, benchmarkReturns, riskFreeRate = 0)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   TABLE 4.7 - Portfolio Specific Risk Table
    
    # Example:
    #   specificRiskTable(R, B, 0)  
    
    # FUNCTION:
    
    # Settings:
    R = periodPercentReturns
    B = benchmarkReturns
    rF = riskFreeRate
    
    # Table:
    Table = rbind(
        "Specific Risk" = specificRisk(R, B, rF, "m")[[1]],
        "Systematic Risk" = systematicRisk(R, B, "m")[[1]],
        "Total Risk" = totalRisk(R, B, rF, "m")[[1]])
    colnames(Table) = ""
    ans = round(Table, 2)
    
    # Return Value:
    ans
}

 
# ------------------------------------------------------------------------------


informationRiskTable <- 
function(periodPercentReturns, benchmarkReturns, riskFreeRate = 0)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   TABLE 4.8 - Portfolio Information Arithmetic Excess Return
    
    # Example:
    #   informationRiskTable(R, B, 0)  
    
    # FUNCTION:
    
    R = periodPercentReturns
    B = benchmarkReturns
    rF = riskFreeRate
    
    Table = rbind(
        "Annualised Portfolio" = annualisedReturn(R, "g", "m")[[1]],
        "Annualised Benchmark" = annualisedReturn(B, "g", "m")[[1]],
        "Tracking Error" = trackingError(R, B, "a")[[1]],
        "Annualised Tracking Error" = 
            annualisedTrackingError(R, B, "a", "m")[[1]],
        "Information Ratio" = informationRatio(R, B, "a", "m")[[1]])
    colnames(Table) = ""
    ans = round(Table, 2)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


infoTable <- 
function()
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   TABLE 4.8 - Information Ratio 
    
    # Example:
    #   specificRiskTable(R, B, 0)  
    
    # FUNCTION:
    
    R = periodPercentReturns
    B = benchmarkReturns
    rF = riskFreeRate
    
    Table = rbind(
        "Annualised Portfolio" = annualisedReturn(R, "g", "m")[[1]],
        "Annualised Benchmark" = annualisedReturn(B, "g", "m")[[1]],
        "Tracking Error" = trackingError(R, B, "g")[[1]],
        "Annualised Tracking Error" = 
            annualisedTrackingError(R, B, "g", "m")[[1]],
        "Information Error" = informationRatio(R, B, "g", "m")[[1]])
    colnames(Table) = ""
    ans = round(Table, 2)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


distStatisticsTable <- 
function()
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   TABLE 4.10 - Portfolio Distributional Statistics Table
    
    # Example:
    #   specificRiskTable(R, B, 0)  
    
    # FUNCTION:
    
    R = periodPercentReturns
    B = benchmarkReturns
    rF = riskFreeRate
    
    Table = rbind(
        "Monthly StDev" = nStandardDeviation(R)[[1]],
        "Moment Skewness" = nSkewness(R)[[1]],
        "Sample StDev" = sampleStandardDeviation(R)[[1]],
        "Moment Kurtosis" = nKurtosis(R)[[1]],
        "Excess Kurtosis" = excessKurtosis(R)[[1]],
        "Jarque Bera Stats" = jarqueBeraStatistic(R)[[1]],
        "Sample Skewness" = sampleSkewness(R)[[1]],
        "SampleExcessKurtosis" = sampleExcessKurtosis(R)[[1]])
    colnames(Table) = ""
    ans = round(Table, 2)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


drawdownRiskTable <- 
function(periodPercentReturns, riskFreeRate, n = 1,
    method = c("geometric", "arithmetic"), 
    scale = c("quarterly", "monthly", "weekly", "daily"))
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   TABLE 4.11 - Portfolio Drawdown Risk Table  
    
    # Example:
    #   drawdownRiskTable(R, 0, n = 3, "g", "m")  
    
    # FUNCTION:
    
    R = periodPercentReturns
    B = benchmarkReturns
    rF = riskFreeRate
    
    Table = rbind(
        "Maximum Drawdown" = maximumDrawdown(R)[[1]],
        "Largest Drawdown" = largestIndividualDrawdown(R)[[1]],
        "Average of 3 Largest Drawdowns" = averageNLargestDrawdown(R, n)[[1]],
        "Sterling Ratio" = sterlingRatio(R, rF, method, scale)[[1]],
        "Calmar Ratio" = calmarRatio(R, rF, method, scale)[[1]],
        "Burke Ratio" = burkeRatio(R, 0, method, scale)[[1]],
        "Pain Index" = painIndex(R)[[1]],
        "Ulcer Index" =ulcerIndex(R)[[1]],
        "Pain Ratio" = painRatio(R, rF, method, scale)[[1]],
        "Martin Ratio" = martinRatio(rF, 0, method, scale)[[1]])
    colnames(Table) = ""
    ans = round(Table, 2)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


downsideRiskTable <- 
function()
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   TABLE 4.12 - Portfolio Downside Risk Table
    
    # Example:
    #   downsideRiskTable(R, B, 0)  
    
    # FUNCTION:
    
    # Data:
    R = periodPercentReturns
    B = benchmarkReturns
    RB = cbind(R, B)
    colnames(RB) = c("R", "B")
    R = RB[, 1]
    B = RB[, 2]
    rF = riskFreeRate
    
    Table = rbind(
        "Annualized Portfolio" = maximumDrawdown[[1]],
        "Monthly Downside Risk" = largestIndividualDrawdown(R)[[1]],
        "Annualized Downside Risk" = averageNLargestDrawdown(R, 3)[[1]],
        "Downside Potential" = sterlingRatio(R, 0, "g", "m")[[1]],
        "Omega" = calmarRatio(R, 0, "g", "m")[[1]],
        "Sortino Ratio" = burkeRatio(R, 0, "g", "m")[[1]],
        "Upside Potential" = painIndex(R)[[1]],
        "Upside Potential Ratio" =ulcerIndex(R)[[1]],
        "Omega-Sharpe Ratio" = painRatio(R, 0, "g", "m")[[1]])
    colnames(Table) = ""
    ans = round(Table, 2)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


downside2Table <- 
function()
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   TABLE 4.12 - Benchmark Downside Risk Statistics  
    
    # Example:
    #   specificRiskTable(R, B, 0)  
    
    # FUNCTION:
    
    R = periodPercentReturns
    B = benchmarkReturns
    rF = riskFreeRate
    
    Table = rbind(
        "Annualized Portfolio" = maximumDrawdown[[1]],
        "Monthly Downside Risk" = largestIndividualDrawdown(R)[[1]],
        "Annualized Downside Risk" = averageNLargestDrawdown(R, 3)[[1]],
        "Downside Potential" = sterlingRatio(R, 0, "g", "m")[[1]],
        "Omega" = calmarRatio(R, 0, "g", "m")[[1]],
        "Sortino Ratio" = burkeRatio(R, 0, "g", "m")[[1]],
        "Upside Potential" = painIndex(R)[[1]],
        "Upside Potential Ratio" =ulcerIndex(R)[[1]],
        "Omega-Sharpe Ratio" = painRatio(R, 0, "g", "m")[[1]])
    colnames(Table) = ""
    ans = round(Table, 2)
    
    # Return Value:
    ans
}

################################################################################

