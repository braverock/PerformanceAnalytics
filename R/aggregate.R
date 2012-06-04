#' aggregate portfolio up
#' 
#' @aliases aggregate
#' 
#' Aggregates the portfoio up to the chosen level using returns, weights and
#' portfolio hierarchy (from the buildHierarchy function)
#'
#' @aliases aggregate
#' @param Rp xts, data frame or matrix of portfolio returns
#' @param wp xts, data frame or matrix of portfolio weights. Names of columns 
#' in weights should correspond to the names of columns in returns
#' @param h  portfolio hierarchy returned by buildHierarchy
#' @param level aggregation level from the hierarchy
#' @author Andrii Babii
#' @seealso  \code{\link{buildHierarchy}}
#' TODO Replace example using portfolio dataset. It should work with returns 
#' in the same manner as Return.portfolio function in future
#' @references
#' @export
#' @examples
#' 
aggregate.returns <-
function(Rp, wp, h, level = "Sector")
{

    Rp = checkData(Rp, method = "xts")
    wp = checkData(wp, method = "xts")

    h = split(h$primary_id, h[level])
    returns = as.xts(matrix(NA, ncol = length(h), nrow = nrow(Rp)), index(Rp))
    for(j in 1:length(h)){
        rp = as.xts(matrix(0, ncol = 1, nrow = nrow(Rp)), index(Rp))
        for(i in 1:length(h[[j]])){
            asset = h[[j]][i]
            r = as.data.frame(Rp)[asset] * as.data.frame(wp)[asset]
            r = as.xts(r)
            rp = rp + r
        }
        returns[, j] = rp
        colnames(returns) = names(h)
    }
    return(returns)
}

aggregate.weights <-
function(wp, h, level = "Sector")
{
    Rp = checkData(Rp, method = "xts")
    wp = checkData(wp, method = "xts")

    h = split(h$primary_id, h[level])
    weights = wp[, 1:length(h)]
    
    for(j in 1:length(h)){
        W = as.xts(matrix(0, ncol = 1, nrow = nrow(wp)), index(wp))
        for(i in 1:length(h[[j]])){
            asset = h[[j]][i]
            w = as.data.frame(wp)[asset]
            w = as.xts(w)
            W = W + w
        }
        weights[, j] = W
        colnames(weights) = names(h)
    }
    return(weights)
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
wp <- as.xts(matrix(rep(c(0.3, 0.1, 0.2, 0.1, 0.2), 5), 5, 5, TRUE), index(Rp))
colnames(wp) <- colnames(Rp)

# 2. Aggregate portfolio
Rp
aggregate.returns(Rp, wp, hierarchy, level = "Sector")
aggregate.returns(Rp, wp, hierarchy, level = "type")
aggregate.weights(wp, hierarchy, level = "Sector")


###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2012 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: CalmarRatio.R 1905 2012-04-21 19:23:13Z braverock $
#
###############################################################################