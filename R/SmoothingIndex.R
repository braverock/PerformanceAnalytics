`SmoothingIndex` <-
function (Ra, ...)
{ # @author Peter Carl

    # Description:
    # SmoothingIndex

    # Ra    return vector

    # Function:

    Ra = checkData(Ra, method="vector")

    # First, create a a maximum liklihood estimation fit for an MA process.

    # include.mean: Getmansky, et al. 2004 p 555 "By applying the above procedure 
    # to observed de-meaned returns...", so set parameter to FALSE
    # transform.pars: ibid, "we impose the additional restriction that the estimated MA(k)
    # process be invertible." so set the parameter to TRUE
    MA2 = arima(Ra, order=c(0,0,2), method="ML", transform.pars=TRUE, 
include.mean=FALSE)

    # Page 555 of Getmansky et al. JFE (2004):
    # 
    # "Because of the scaling property Eq. (52) of the MA(k) likelihood function, a 
    # simple procedure for obtaining estimates of our smoothing model with the 
    # normalization Eq. (49) is to transform estimates (θ; sigma) from standard 
    # MA(k) estimation packages such as SAS or RATS by dividing each θi by 1 + θ1 + 
    # θ2 ... θk and multiplying sigma by the same factor. The likelihood function 
    # remains unchanged but the transformed smoothing coefficients will now satisfy 
    # Eq. (49)."

    # From the arima function above, MA2$coef contains two coefficients, and no intercept value.
    # The text above indicates that theta_0 would be equal to 1 and returned as a coefficient.
    # The calculation below adjusts for that.  as.numeric strips off the labels

    thetas = as.numeric((MA2$coef)/sum(MA2$coef))

    # This measure is well known in the industrial organization literature as the Herfindahl 
    # index, a measure of the concentration of firms in a given industry where yj represents the 
    # market share of firm j: Because theta_j A1⁄20; 1; x is also confined to the unit interval, 
    # and is minimized when all the theta_j ’s are identical, which implies a value of 1=ðk þ 1Þ 
    # for x; and is maximized when one coefficient is 1 and the rest are 0, in which case x 1⁄4 1: 
    # In the context of smoothed returns, a lower value of x implies more smoothing, and the upper bound
    # of 1 implies no smoothing, hence we shall refer to x as a ‘‘smoothing index’’.

    SmoothingIndex = sum(thetas^2) # Calc'd as Herfindahl index would be

    return(SmoothingIndex)

}