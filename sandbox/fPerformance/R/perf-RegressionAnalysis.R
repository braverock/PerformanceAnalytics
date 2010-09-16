
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
# FUNCTION:                    REGRESSION ANALYSIS MEASURES:
#  regressionAlpha              Returns regression alpha
#  regressionBeta               Returns regression beta
#  regressionEpsilon            Returns regression residuals
#  regressionCAPM               Returns CAPM alpha and beta
#  jensenAlpha                  Returns Jensen's alpha
#  bullBeta                     Returns Bull Beta
#  bearBeta                     Returns Bear Beta
#  betaTimingRatio              Returns beta timing ratio
#  nCovariance                  Returns momentum covariance
#  nCorrelation                 Returns momentum ccorrelation
#  RSquared                     Returns R squared
#  systematicRisk               Returns annualised systematic risk
#  specificRisk                 Returns annualised specific risk
#  totalRisk                    Returns annualised total risk
#  treynorRatio                 Returns Treynor ratio
#  modTreynorRatio              Returns modified Treynor risk
#  appraisalRatio               Returns appraisal ratio
#  modJensenAlpha               Returns annualised modified Jensen alpha
#  altModJensenAlpha            Returns alt annualised modified Jensen alpha
#  selectivity                  Returns unnualised selectivity
#  betaF                        Returns betaF diversification
#  selectivityNet               Returns annualised selectivity net
################################################################################


regressionAlpha <-
function(periodPercentageReturn, benchmarkReturn)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns regression alpha
    
    # Example:
    #   regressionAlpha(R, B)
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns)) 
    stopifnot(isUnivariate(benchmarkReturns)) 
    
    # Data:
    R = periodPercentReturns
    B = benchmarkReturns
    RB = cbind(R, B)
    colnames(RB) = c("R", "B")
    R = RB[, 1]
    B = RB[, 2]
    
    # Regression:
    fit = lm(R ~ B, data = RB)
    
    # Result:
    ans = fit$coefficients[[1]]
    names(ans) = "Regression Alpha"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


regressionBeta <-
function(periodPercentReturns, benchmarkReturns)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns regression beta
    
    # Example:
    #   regressionBeta(R, B)
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns)) 
    stopifnot(isUnivariate(benchmarkReturns)) 
    
    # Data:
    R = periodPercentReturns
    B = benchmarkReturns
    RB = cbind(R, B)
    colnames(RB) = c("R", "B")
    R = RB[, 1]
    B = RB[, 2]
    
    # Regression:
    fit = lm(R ~ B, data = RB)
    
    # Result:
    ans = fit$coefficients[[2]]
    names(ans) = "Regression Beta"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


regressionEpsilon <-
function(periodPercentageReturns, benchmarkReturns)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns regression residuals
    
    # Example:
    #   regressionEpsilon(R, B)
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns)) 
    stopifnot(isUnivariate(benchmarkReturns)) 
    
    # Data:
    R = periodPercentReturns
    B = benchmarkReturns
    RB = cbind(R, B)
    colnames(RB) = c("R", "B")
    R = RB[, 1]
    B = RB[, 2]
    
    # Regression:
    fit = lm(R ~ B, data = RB)
    
    # Result:
    ans = residuals(fit)
    names(ans) = NULL
    
    # Return Value:
    ans
}



# ------------------------------------------------------------------------------


regressionCAPM <-
function(periodPercentReturns, benchmarkReturns, riskFreeRate = 0)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns CAPM alpha and beta
    
    # Example:
    #   regressionCAPM(R, B)
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns)) 
    stopifnot(isUnivariate(benchmarkReturns)) 
    
    # Data:
    R = periodPercentReturns
    B = benchmarkReturns
    RB = cbind(R, B)
    colnames(RB) = c("R", "B")
    R = RB[, 1]
    B = RB[, 2]
    
    # Regression:
    fit = lm(R ~ B, data = RB)
    alpha = fit$coefficients[[1]]
    beta = fit$coefficients[[2]]
    
    # Result:
    ans = c(alphaCAPM = alpha, betaCAPM = beta)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------

   
jensenAlpha <-
function(periodPercentReturns, benchmarkReturns, riskFreeRate = 0,
    method = c("geometric", "arithmetic"), 
    scale = c("quarterly", "monthly", "weekly", "daily"))
{   
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns Jensen's alpha
    
    # Example:
    #   jensenAlpha(R, B, 0, "g", "m")
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns)) 
    method = match.arg(method) 
    Scale = .scale(match.arg(scale)) 
    
    # Data:
    R = periodPercentReturns
    B = benchmarkReturns
    RB = cbind(R, B)
    colnames(RB) = c("R", "B")
    R = RB[, 1]
    B = RB[, 2]
    rP = annualisedReturn(R, method, scale)
    rF = riskFreeRate * Scale
    betaP = regressionBeta(R, B)
    b = annualisedReturn(B, method, scale)
    
    # Result:
    ans = rP - rF - betaP * (b - rF)
    names(ans) = "Jensens Alpha"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


bullBeta =
function(periodPercentReturns, benchmarkReturns, riskFreeRate = 0)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns Bull Beta
    
    # Example:
    #   bullBeta(R, B, 0)
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns)) 
    stopifnot(isUnivariate(benchmarkReturns)) 
    
    # Data:
    R = periodPercentReturns
    B = benchmarkReturns
    RB = cbind(R, B)
    RB = RB[R > 0, ]
    colnames(RB) = c("R", "B")
    R = RB[, 1]
    B = RB[, 2]
    
    # Regression:
    fit = lm(R ~ B, data = RB)
    
    # Result:
    ans = fit$coefficients[[2]]
    names(ans) = "Bull Beta +"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


bearBeta =
function(periodPercentReturns, benchmarkReturns, riskFreeRate = 0)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns Bear Beta
    
    # Example:
    #   bearBeta(R, B, 0)
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns)) 
    stopifnot(isUnivariate(benchmarkReturns)) 
    
    # Data:
    R = periodPercentReturns
    B = benchmarkReturns
    RB = cbind(R, B)
    RB = RB[R < 0, ]
    colnames(RB) = c("R", "B")
    R = RB[, 1]
    B = RB[, 2]
    
    # Regression:
    fit = lm(R ~ B, data = RB)
    
    # Result:
    ans = fit$coefficients[[2]]
    names(ans) = "Bear Beta -"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


betaTimingRatio <-  
function(periodPercentReturns, benchmarkReturns, riskFreeRate = 0)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns beta timing ratio
    
    # Example:
    #   betaTimingRatio(R, B, 0)
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns)) 
    stopifnot(isUnivariate(benchmarkReturns)) 
    
    # Data:
    R = periodPercentReturns
    B = benchmarkReturns
    RB = cbind(R, B)
    colnames(RB) = c("R", "B")
    R = RB[, 1]
    B = RB[, 2]
    
    # Result:
    ans = bullBeta(R, B, riskFreeRate) / bearBeta(R, B, riskFreeRate)
    names(ans) = "betaTimingRatio"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


nCovariance <-
function(periodPercentReturns, benchmarkReturns) 
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns momentum covariance
    
    # Example:
    #   nCovariance(R, B)
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns)) 
    stopifnot(isUnivariate(benchmarkReturns)) 
    
    # Data:
    R = periodPercentReturns
    B = benchmarkReturns
    RB = cbind(R, B)
    colnames(RB) = c("R", "B")
    R = RB[, 1]
    B = RB[, 2]
    n = nrow(R)
    Covariance = sum((R - mean(R)) * (B - mean(B)))
    
    # Result:
    ans = Covariance / n
    names(ans) = "Moment Covariance"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


nCorrelation <-
function(periodPercentReturns, benchmarkReturns) 
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns momentum corelation
    
    # Example:
    #   nCorrelation(R, B)
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns)) 
    stopifnot(isUnivariate(benchmarkReturns)) 
    
    # Data:
    R = periodPercentReturns
    B = benchmarkReturns
    RB = cbind(R, B)
    colnames(RB) = c("R", "B")
    R = RB[, 1]
    B = RB[, 2]
    Covariance = nCovariance(R, B)[[1]]
    sigmaP = nStandardDeviation(R)
    sigmaM = nStandardDeviation(B)
    
    # Result:
    ans = Covariance / ( sigmaP * sigmaM )
    names(ans) = "Moment Correlation"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


RSquared <- 
function(periodPercentReturns, benchmarkReturns) 
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns R squared
    
    # Example:
    #   RSquared(R, B)
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns)) 
    stopifnot(isUnivariate(benchmarkReturns)) 
    
    # Data:
    R = periodPercentReturns
    B = benchmarkReturns
    RB = cbind(R, B)
    colnames(RB) = c("R", "B")
    R = RB[, 1]
    B = RB[, 2]
    
    # Result:
    ans = nCorrelation(R, B)[[1]]^2
    names(ans) = "R Squared"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


systematicRisk <-
function(periodPercentReturns, benchmarkReturns,
    scale = c("quarterly", "monthly", "weekly", "daily"))
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns annualised systematic risk
    
    # Example:
    #   systematicRisk(R, B, "m")
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns)) 
    stopifnot(isUnivariate(benchmarkReturns)) 

    # Data:
    R = periodPercentReturns
    B = benchmarkReturns
    RB = cbind(R, B)
    colnames(RB) = c("R", "B")
    R = RB[, 1]
    B = RB[, 2]
    beta = regressionBeta(R, B)
    sigmaM = annualisedStandardDeviation(B, scale)
      
    # Result:
    ans = beta * sigmaM
    names(ans) = "Systematic Risk per Period"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


specificRisk <-
function(periodPercentReturns, benchmarkReturns, riskFreeRate = 0,
    scale = c("quarterly", "monthly", "weekly", "daily"))
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns annualised specific risk
    
    # Example:
    #   specificRisk(R, B, "m")
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns)) 
    stopifnot(isUnivariate(benchmarkReturns)) 
    
    # Data:
    R = periodPercentReturns
    B = benchmarkReturns
    RB = cbind(R, B) - riskFreeRate
    colnames(RB) = c("R", "B")
    R = RB[, 1]
    B = RB[, 2]
    fit = lm(R ~ B, data = RB)
    resid = timeSeries(as.vector(resid(fit)))
    
    # Result:
    ans = annualisedStandardDeviation(resid, scale)
    names(ans) = "Specific Risk per Period"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


totalRisk <-
function(periodPercentReturns, benchmarkReturns, riskFreeRate = 0,
    scale = c("quarterly", "monthly", "weekly", "daily"))
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns annualised total risk
    
    # Example:
    #   totalRisk(R, B, 0, "m")
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns)) 
    stopifnot(isUnivariate(benchmarkReturns)) 
    
    # Data:
    R = periodPercentReturns
    B = benchmarkReturns
    RB = cbind(R, B) - riskFreeRate
    colnames(RB) = c("R", "B")
    R = RB[, 1]
    B = RB[, 2]
    systRisk = systematicRisk(R, B, scale)
    specRisk = specificRisk(R, B, rF, scale)
    
    # Result:
    ans = sqrt( systRisk^2 + specRisk^2)
    names(ans) = "Total Risk per Period"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


treynorRatio <-
function(periodPercentReturns, benchmarkReturns, riskFreeRate = 0)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns Treynor ratio
    
    # Example:
    #   treynorRatio(R, B, 0)
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns)) 
    stopifnot(isUnivariate(benchmarkReturns)) 
    
    # Data:
    R = periodPercentReturns
    B = benchmarkReturns
    RB = cbind(R, B) - riskFreeRate
    colnames(RB) = c("R", "B")
    R = RB[, 1]
    B = RB[, 2]
    rP = mean(R)
    rF = mean(B)
    beta = regressionCAPM(R, B, riskFreeRate = 0)[[2]]
    
    # Result:
    ans = (rP - rF) / beta
    names(ans) = "Treynor Ratio"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


modTreynorRatio <-
function(periodPercentReturns, benchmarkReturns, riskFreeRate = 0,
    method = c("geometric", "arithmetic"),
    scale = c("quarterly", "monthly", "weekly", "daily"))
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns modified Treynor risk
    
    # Example:
    #   modTreynorRatio(R, B, 0)  
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns)) 
    stopifnot(isUnivariate(benchmarkReturns)) 
    
    # Data:
    R = periodPercentReturns
    B = benchmarkReturns
    RB = cbind(R, B) - riskFreeRate
    colnames(RB) = c("R", "B")
    R = RB[, 1]
    B = RB[, 2]
    annualisedReturn = annualisedReturn(R, method, scale) 
    riskFreeRate = riskFreeRate
    sigmaS = regressionCAPM(R, B, riskFreeRate = 0)[1]
    
    # Result:
    ans = (annualisedReturn - riskFreeRate) / sigmaS
    names(ans) = "Modified Treynor Ratio"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


appraisalRatio <-
function(periodPercentReturns,  
    method = c("geometric", "arithmetic"), 
    scale = c("quarterly", "monthly", "weekly", "daily"))
{   
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns appraisal ratio
    
    # Example:
    #   appraisalRatio(R, )   
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns)) 
    
    # Data:
    R = periodPercentReturns
    jensenAlpha = NA
    sigmaEps = NA
    
    # Result:
    ans = jensenAlpha / sigmaEps
    names(ans) = "Appraisal Ratio"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


modJensenAlpha <-
function(periodPercentReturns,  
    method = c("geometric", "arithmetic"), 
    scale = c("quarterly", "monthly", "weekly", "daily"))
{   
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns annualised modified Jensen alpha
    
    # Example:
    #   modJensenAlpha(R, )   
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns)) 
    
    # Data:
    R = periodPercentReturns
    jensenAlpha = NA
    beta = NA
    
    # Result:
    ans = jensenAlpha / beta
    names(ans) = "Modified Jensen Alpha"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


altModJensenAlpha <-
function(periodPercentReturns,  
    method = c("geometric", "arithmetic"), 
    scale = c("quarterly", "monthly", "weekly", "daily"))
{   
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns alternative annualised modified Jensen alpha
    
    # Example:
    #   altModJensenAlpha(R, )      
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns)) 
    stopifnot(isUnivariate(benchmarkReturns)) 
    
    # Data:
    R = NA
    jensenAlpha = NA
    sigmaS = NA
    
    # Result:
    ans = jensenAlpha / sigmaS
    names(ans) = "Alternative Modified Jensen Alpha"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


selectivity <-
function(periodPercentReturns,  
    method = c("geometric", "arithmetic"), 
    scale = c("quarterly", "monthly", "weekly", "daily"))
{   
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns unnualised selectivity
    
    # Example:
    #   selectivity(R)   
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns)) 
    stopifnot(isUnivariate(benchmarkReturns)) 
    
    # Data:
    R = periodPercentReturns
    rP = rF = betaP = b = NA
    
    # Result:
    ans = rP -rF - betaP * (b - rF)
    names(ans) = NA
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


betaF <-
function(periodPercentReturns,  
    method = c("geometric", "arithmetic"), 
    scale = c("quarterly", "monthly", "weekly", "daily"))
{   
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns betaF diversification
    
    # Example:
    #   betaF(R, )   
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns)) 
    stopifnot(isUnivariate(benchmarkReturns)) 
    
    # Data:
    R = periodPercentReturns
    sigmaP = NA
    sigmaM = NA
    
    # Result:
    ans = sigmaP / sigmaM
    names(ans) = "betaF diversification"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


selectivityNet <-
function(periodPercentReturns,  
    method = c("geometric", "arithmetic"), 
    scale = c("quarterly", "monthly", "weekly", "daily"))
{   
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns annualised selectivity net
    
    # Example:
    #   selectivityNet(R, )   
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(isUnivariate(periodPercentReturns)) 
    stopifnot(isUnivariate(benchmarkReturns)) 
    
    # Data:
    R = periodPercentReturns
    alpha = NA
    d = NA
    
    # Result:
    ans = alpha - d
    names(ans) = "Selectivity Net"
    
    # Return Value:
    ans
}


################################################################################

