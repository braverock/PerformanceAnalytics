

################################################################################
# FUNCTION:
#  meanAbsoluteDeviation
#  nVariance
#  sampleVariance 
#  nStandardDeviation
#  sampleStandardDeviation
#  annualisedStandardDeviation
#  frequency
#  numberOfData
#  sharpeRatio
#  mSquared
#  mSquaredExcess
#  diffReturn
#  GrahamHarvey1
#  GrahamHarvey2
################################################################################


meanAbsoluteDeviation <- 
mad

    # A function implemented by Diethelm Wuertz
    

# ------------------------------------------------------------------------------


sampleVariance <- 
var

    # A function implemented by Diethelm Wuertz
    
    
# ------------------------------------------------------------------------------


sampleVar <-
function(R) 
{
    # A function implemented by Diethelm Wuertz
    
    n = length(R)
    n * var(R) / (n-1)
}


# ------------------------------------------------------------------------------


nStandardDeviation <- 
sd

    # A function implemented by Diethelm Wuertz
    

# ------------------------------------------------------------------------------


sampleStandardDeviation <- 
function(R) 
{
    # A function implemented by Diethelm Wuertz
    
    sqrt(sampleVariance(R))
}


# ------------------------------------------------------------------------------


annualisedStandardDeviation <-
function(R, scale = c("quarterly", "monthly", "weekly", "daily"))
{
    # A function implemented by Diethelm Wuertz
    
    # Example:
    #   annualisedStandardDeviation(R, "m")

    Scale = .scale(scale)
    ans = nStandardDeviation(R) * sqrt(Scale)
    names(ans) <- "annualisedStandardDeviation"
    ans
    
}


# ------------------------------------------------------------------------------


#  frequency

    # A function implemented by Diethelm Wuertz
    
    

# ------------------------------------------------------------------------------


#  numberOfData

    # A function implemented by Diethelm Wuertz
    

# ------------------------------------------------------------------------------


#  sharpeRatio
    
    # A function implemented by Diethelm Wuertz
        

# ------------------------------------------------------------------------------


#  mSquared

    # A function implemented by Diethelm Wuertz
    

# ------------------------------------------------------------------------------


#  mSquaredExcess

    # A function implemented by Diethelm Wuertz
    
    
# ------------------------------------------------------------------------------


#  diffReturn

    # A function implemented by Diethelm Wuertz
    
    
# ------------------------------------------------------------------------------


#  GrahamHarvey1

    # A function implemented by Diethelm Wuertz
    

# ------------------------------------------------------------------------------


#  GrahamHarvey2


    # A function implemented by Diethelm Wuertz
    
    
# ##############################################################################


