`download.RiskFree` <-
function(start = "1998-01-01", end = NULL)
{ # @ author Peter Carl

    #  Download returns of the risk free asset

    # Required inputs are just start and stop dates.
    # Outputs a table of estimates of ***Monthly*** risk-free returns

    # FUNCTION:

    # Requires the tseries library
    require(tseries)

    # Download the risk free data, in this case, the 13-WEEK TREASURY BILL (^IRX)
    # from finance.yahoo.com

    # @todo: Compression attribute can be made configurable for different granularity
    x = get.hist.quote("^irx", start = start, end = end, quote = "Close", compression = "m")

    # Yahoo returns yield data as an annualized percentage; 
    rf = as.data.frame(x/100/12)
    colnames(rf) = "13-week US Treasury Bill (^IRX)"
    rf
}