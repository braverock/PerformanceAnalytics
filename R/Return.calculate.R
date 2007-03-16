`CalculateReturns` <-
function(Prices, method = "compound")
{ # @ author Peter Carl

    #  Calculate returns from a price stream

    # Required inputs

    # Prices:
    # method: "simple", "compound"

    # FUNCTION:

    prices = checkData(Prices, method = "zoo")

    if(method=="simple")
        Returns = prices/lag(prices,-1) - 1

    if(method=="compound") {
        Returns = diff(log(prices))
    }

    Returns

}