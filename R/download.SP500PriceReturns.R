`download.SP500PriceReturns` <-
function(start = "1998-01-01", end = NULL, compression = "m")
{ # @ author Peter Carl

    #  Download returns of the "market" asset.

    # Required inputs are just start and stop dates.
    # Outputs a table of log returns

    # FUNCTION:

    # Requires the tseries library
    require(tseries)

    # Download the "market" returns, in this case, the S&P 500 INDEX (^GSPC)
    # from finance.yahoo.com

    # @todo: Offer a menu of "market" assets
    # @todo: Compression attribute can be made configurable for different granularity
    x = get.hist.quote("^gspc", start = start, end = end, quote = "Close", compression = compression)
    # @todo: make the data type returned selectable (e.g., df, ts, zoo, matrix)
    # @todo: offer simple or log returns as method

    # Yahoo returns yield data as price series, so we need to calculate returns 
    market.returns = CalculateReturns(x, method="simple")

    colnames(market.returns)="SP500"
    market.returns
}