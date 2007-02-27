`CalculateReturns` <-
function(Prices, method="simple")
{ # @ author Peter Carl

    #  Calculate returns from a price stream

    # Required inputs

    # Prices: 
    # method: "simple", "compound"

    # FUNCTION:

    prices = checkDataMatrix(Prices)
    prices.colnames = colnames(Prices)

    prices.nrows = dim(prices)[1]
    prices.ncols = dim(prices)[2]

    Returns = matrix(data = NA, nrow = (prices.nrows-1), ncol = prices.ncols)

    if(method=="simple") {
        for(j in 1:prices.ncols) {
            for(i in 2:prices.nrows) {
                Returns[i-1,j]=(prices[i,j]/prices[i-1,j])-1
            }
        }
    }
    if(method=="compound") {
        Returns = diff(log(prices))
    }
    prices.rownames=rownames(prices)

    Returns = as.data.frame(Returns, row.names=prices.rownames[2:N])
    colnames(Returns) = colnames(Prices)

    Returns

}