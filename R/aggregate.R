#' aggregates portfolio up
#' 
#' @aliases aggregate
#' 
#' Aggregates the portfoio up to the chosen level using returns, weights and
#' portfolio hierarchy (from the buildHierarchy function)
#'
#' @aliases aggregate
#' @param Rp portfolio returns
#' @param wp portfolio weights
#' @param h  portfolio hierarchy
#' @param level level of aggregation in the hierarchy
#' @author Andrii Babii
#' @seealso 
#' @references
#' @examples
#' 
aggregate <-
function(Rp, wp, h, level = "Sector"){
    
    h = split(h$primary_id, h[level])
    for(j in 1:length(h)){
        for(i in length(h[[j]])){
            asset = h[[j]][i]
            r = as.data.frame(Rp)[asset] * as.data.frame(wp)[asset]
            r = as.xts(r)
        
            if (i == 1){
                rp = r
            } else{
                rp = rp + r
            }
            colnames(rp) = names(h[j])
        }
            if (j == 1){ 
                returns = rp
            } else { 
                returns = cbind(returns, rp) 
            } 
    }
    return(returns)
}

# Example

# 1. Generate data
list <- c("XOM", "IBM", "CVX", "WMT", "GE")
update_instruments.TTR(list, exchange="NYSE")
hierarchy <- buildHierarchy(ls_stocks(), c("type", "currency", "Sector"))
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
wp <- as.xts(matrix(rep(c(0.3, 0.1, 0.2, 0.1, 0.2), 5), 5, 5), index(Rp))
colnames(wp) <- colnames(Rp)

# 2. Aggregate portfolio
Rp <- aggregate(Rp, wp, hierarchy, "Sector")
Rp