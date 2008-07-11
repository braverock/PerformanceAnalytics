`style.fit` <-
function(R.fund, R.style, model=FALSE, method = c("constrained", "unconstrained", "normalized"), leverage = FALSE, ...) 
{
# INPUTS
# R.fund   Vector of a fund return time series
# R.style   Matrix of a style return time series
# 
# 
# OUTPUTS
# weights   Vector of optimal style index weights
# @ todo: TE  Tracking error between the calc'd and actual fund
# @ todo: Fp  Vector of calculated fund time series
# @ todo: R^2  Coefficient of determination

# 
# To implement style analysis as described in:
# http://www.stanford.edu/~wfsharpe/art/sa/sa.htm

    method = method[1]

    # Check to see if the required libraries are loaded
    if(!require("quadprog", quietly=TRUE))
        stop("package", sQuote("quadprog"), "is needed.  Stopping")

    R.fund = checkData(R.fund, method="data.frame")
    R.style = checkData(R.style, method="data.frame")

    # @todo: Missing data is not allowed, use = "pairwise.complete.obs" ?
    style.rows = dim(R.style)[1]
    style.cols = dim(R.style)[2]
    fund.rows = dim(R.fund)[1]
    fund.cols = dim(R.fund)[2]

    style.colnames = colnames(R.style)

    for(fund.col in 1:fund.cols){
        if(method == "constrained"){
            column.result = style.QPfit(R.fund = R.fund[,fund.col,drop=FALSE], R.style = R.style, leverage = leverage)
            if(fund.col == 1){
                result.weights = column.result$weights
                result.R2 = column.result$R.squared
                result.adjR2 = column.result$adj.R.squared
            }
            else{
                result.weights = cbind(result.weights, column.result$weights)
                result.R2 = cbind(result.R2, column.result$R.squared)
                result.adjR2 = cbind(result.adjR2, column.result$adj.R.squared)  
            }
        }
        else if(method == "unconstrained" | method == "normalized"){
            column.result = lm(R.fund[, fund.col] ~ ., data = R.style)
            column.weights = as.data.frame(coef(column.result)[-1])
#             colnames(column.weights)= style.colnames[fund.col]
            rownames(column.weights) = colnames(R.style)
            colnames(column.weights) = colnames(R.fund)[fund.col]

            R2 = as.data.frame(summary(column.result)$r.squared)
#             adjR2 = as.data.frame(summary(column.result)$adj.r.squared)
            colnames(R2) = colnames(R.fund)[fund.col]
#             colnames(adjR2) = colnames(R.fund)[fund.col] 
            rownames(R2) = "R-squared"
#             rownames(adjR2) = "Adj R-squared"

            if(method == "normalized") {
                column.weights = column.weights/sum(column.weights)
            }
            if(fund.col == 1){
                result.weights = column.weights
                result.R2 = R2
#                 result.adjR2 = adjR2
            }
            else{
                result.weights = cbind(result.weights, column.weights)
                result.R2 = cbind(result.R2, R2)
#                 result.adjR2 = cbind(result.adjR2, adjR2)
            }
        }
        else stop("Method is mis-specified.  Select from \"constrained\", \"unconstrained\", or  \"normalized\"")
    }
    result = list(weights = result.weights, R.squared = result.R2) #, adj.R.squared = result.adjR2 )

    return(result)

    # EXAMPLE:
    # > head(R.fund)
    #          SP500.TR
    # Jan 1996   0.0340
    # Feb 1996   0.0093
    # Mar 1996   0.0096
    # Apr 1996   0.0147
    # May 1996   0.0258
    # Jun 1996   0.0038
    # > head(R.style)
    #          Russell.1000.Growth Russell.1000.Value
    # Jan 1996              0.0335             0.0312
    # Feb 1996              0.0183             0.0076
    # Mar 1996              0.0013             0.0170
    # Apr 1996              0.0263             0.0038
    # May 1996              0.0349             0.0125
    # Jun 1996              0.0014             0.0008
    # > style.QPfit(R.fund, R.style)
    # [1] 0.5047724 0.4952276
    # > style.QPfit(R.fund, R.style, all=T)
    # $solution
    # [1] 0.5047724 0.4952276
    # 
    # $value
    # [1] -0.0008888153
    # 
    # $unconstrainted.solution
    # [1] 0.5040111 0.4815228
    # 
    # $iterations
    # [1] 2 0
    # 
    # $iact
    # [1] 1
    # 

}