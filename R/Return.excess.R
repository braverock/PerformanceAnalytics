`Return.excess` <-
function (R, rf)
{ # @author Peter Carl

    # DESCRIPTION:
    # Calculates the returns of an asset in excess of the given 
    # "risk free rate" for the period.

    # Inputs:
    # R: a matrix, data frame, or timeSeries of returns
    # rf: a measure of the risk free rate, whether a period average
    #     (a single number) or a timeseries vector

    # Outputs:
    # A timeseries of the calculated series

    # FUNCTION:

    # Transform input data to a timeseries (zoo) object
    R = checkData(R, method="zoo")

    # if the risk free rate is delivered as a timeseries, we'll check it
    # and convert it to a zoo object.
    if(!is.null(dim(rf)))
        rf = checkData(rf, method = "zoo")

    ## arithmetic on zoo objects intersects them first
    R.excess = R - rf

    # RESULTS:
    return(R.excess)
}