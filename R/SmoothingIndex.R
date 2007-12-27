`SmoothingIndex` <-
function (ra, ...)
{ # @author Peter Carl

    # Description:
    # SmoothingIndex

    # ra    log return vector

    # Function:

    ra = checkData(ra, method="vector", na.rm=TRUE)

    MA2 = NULL
    thetas = 0
    smoothing.index = 0

    # First, create a a maximum likelihood estimation fit for an MA process.

    # include.mean: Getmansky, et al. JFE 2004 p 555 "By applying the above procedure 
    # to observed de-meaned returns...", so set parameter to FALSE
    # transform.pars: ibid, "we impose the additional restriction that the estimated MA(k)
    # process be invertible." so set the parameter to TRUE
    MA2 = arima(ra, order=c(0,0,2), method="ML", transform.pars=TRUE, include.mean=FALSE)

    # Page 555:
    # 
    # "Because of the scaling property Eq. (52) of the MA(k) likelihood function, a 
    # simple procedure for obtaining estimates of our smoothing model with the 
    # normalization Eq. (49) is to transform estimates (θ; sigma) from standard 
    # MA(k) estimation packages such as SAS or RATS by dividing each θi by 1 + θ1 + 
    # θ2 ... θk and multiplying sigma by the same factor. The likelihood function 
    # remains unchanged but the transformed smoothing coefficients will now satisfy 
    # Eq. (49)."

    # From the arima function above, MA2$coef contains two coefficients, and no intercept value.
    # The calculation below adjusts for that.

    # Dr. Stefan Albrecht, CFA points out, "I assume that you have to take:"
thetas = c(ma0 = 1, coef(MA2)) / (1 + sum(coef(MA2)))
# 
# 
#     thetas = as.numeric((MA2$coef)/sum(MA2$coef))

    # This measure is well known in the industrial organization literature as the Herfindahl 
    # index, a measure of the concentration of firms in a given industry where yj represents the 
    # market share of firm j: Because theta_j A1⁄20; 1; x is also confined to the unit interval, 
    # and is minimized when all the theta_j ’s are identical, which implies a value of 1=ðk þ 1Þ 
    # for x; and is maximized when one coefficient is 1 and the rest are 0, in which case x 1⁄4 1: 
    # In the context of smoothed returns, a lower value of x implies more smoothing, and the upper bound
    # of 1 implies no smoothing, hence we shall refer to x as a ‘‘smoothing index’’.

    smoothing.index = sum(thetas^2) # Calc'd as Herfindahl index would be, referred to as ξ, below

    # The interpretation of this is tricky:

    # "Because θj ∈ [0, 1], ξ is also confined to the unit interval, and is minimized when all 
    # the θj ’s are identical, which implies a value of 1/(k + 1) for ξ, and is maximized when 
    # one coefficient is 1 and the rest are 0, in which case ξ = 1. In the context of smoothed 
    # returns, a lower value of ξ implies more smoothing, and the upper bound of 1 implies no 
    # smoothing, hence we shall refer to ξ as a “smoothing index”."

    # That's fine, except that this method (as described in the paper), does not enforce
    # θj ∈ [0, 1], so ξ is not limited to that range either.  All we can say is that lower values
    # are "less liquid" and higher values are "more liquid" or mis-specified.

    return(smoothing.index)

}