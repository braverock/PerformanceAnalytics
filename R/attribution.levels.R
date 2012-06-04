# 5-steps attribution (3-levels)
attribution.levels <-
function(Rp, Rb, wp, wb, h, ...)
{ # @author Andrii Babii

    Rb = checkData(Rb)
    Rp = checkData(Rp)

    levels <- unlist(list(...))
    if (!is.null(levels)) stopifnot(is.character(levels))
    
    # Get lists with returns and weights at all levels for the portfolio and the benchmark
    returns.p = list()
    weights.p = list()
    for(i in 1:length(levels)){
        returns.p[[i]] = Return.level(Rp, wp, h, level = levels[i])
        weights.p[[i]] = Weight.level(wp, h, level = levels[i])
    } 
    names(returns.p) = levels
    names(weights.p) = levels

    returns.b = list()
    weights.b = list()
    for(i in 1:length(levels)){
        returns.b[[i]] = Return.level(Rb, wb, h, level = levels[i])
        weights.b[[i]] = Weight.level(wb, h, level = levels[i])
    } 
    names(returns.b) = levels
    names(weights.b) = levels

    # Get lists with semi-notional funds returns 
    # (computed using portfolio weights and benchmark returns)
    bs = list()
    for(i in 1:length(levels)){
        bs[[i]] = Return.rebalancing(weights.p[[i]], returns.b[[i]])
    }
    
    # Get portfolio and benchmark returns
    r = Return.rebalancing(Rp, wp)
    b = Return.rebalancing(Rb, wb)

    allocation.1 = (1 + bs[[1]]) / (1 + b) - 1
    allocation.2 = (1 + bs[[2]]) / (1 + bs[[1]]) - 1
    allocation.3 = (1 + bs[[3]]) / (1 + bs[[2]]) - 1
    selection = (1 + r) / (1 + bs[[3]]) - 1
    total = (1 + r) / (1 + b) - 1 #Total excess return
    # Level 1 attribution
    l1 = (weights.p[[1]] - weights.b[[1]]) * ((1 + returns.b[[1]]) / (1 + b) - 1)
    # Level 2 attribution
    l2 = (weights.p[[2]] - weights.b[[2]]) * ((1 + returns.b[[2]]) / (1 + returns.b[[1]]) - 1) * ((1 + returns.b[[1]]) / (1 + bs[[1]]))
    # Level 3 attribution
    w = (weights.p[[3]] - weights.b[[3]])
    a1 = 1 + returns.b[[2]]
    b1 = ((1 + returns.b[[3]]) / (cbind(a1, a1, a1)) - 1)
    b2 = ((1 + returns.b[[2]]) / (1 + bs[[2]]))
    b2 = cbind(b2, b2, b2)
    l3 = w * b1 * b2
    # Security/Asset selection
    w = weights.p[[3]]
    a1 = cbind((1 + r), (1 + r), (1 + r)) 
    b1 = a1 / (1 + returns.b[[3]]) - 1
    a2 = cbind((1 + bs[[3]]), (1 + bs[[3]]), (1 + bs[[3]]))
    b2 = (1 + returns.b[[3]]) / a2
    select = w * b1 * b2

    result = list()
    general = cbind(allocation.1, allocation.2, allocation.3, selection, total)
    colnames(general) = c("L1 allocation", "L2 allocation", "L3 allocation", 
    "Selection", "Total")
    result[[1]] = general
    result[[2]] = l1
    result[[3]] = l2
    result[[4]] = l3
    result[[5]] = select
    names(result) = c("Multi-level attribution", "Level 1 attribution", "Level 2 attribution", "Level 3 attribution", "Security selection")
    return(result)

}

# Example:
require(FinancialInstrument)
require(PerformanceAnalytics)
list <- c("XOM", "IBM", "CVX", "WMT", "GE")
update_instruments.TTR(list, exchange="NYSE")
h <- buildHierarchy(ls_stocks(), c("type", "currency", "Sector"))
getSymbols(list)
for (i in list){
    r <- Return.calculate(to.yearly(get(i)))[2:6, 4]
    colnames(r) <- i
    if(i == "XOM"){
        Rp <- r
    } else{
        Rp <- cbind(Rp, r)
    }
}
Rb <- Rp
wp <- c(0.3, 0.2, 0.2, 0.1, 0.2)
wb <- c(0.1, 0.3, 0.2, 0.2, 0.2)
wp = Weight.transform(Rp, wp) # transform weights to the xts object 
wb = Weight.transform(Rb, wb) # of the same size as returns using a function from Return.level
    
attribution.levels(Rp, wp, Rb, wb, h, c("type", "currency", "Sector"))


