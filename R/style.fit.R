`style.fit` <-
function(R.fund, R.style, model=FALSE, method = c("constrained", "unconstrained", "normalized"), leverage = FALSE, selection = c("none", "AIC"), ...) 
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
    selection = selection[1]

    # Check to see if the required libraries are loaded
#     if(!require("quadprog", quietly=TRUE))
#         stop("package", sQuote("quadprog"), "is needed.  Stopping")

    R.fund = checkData(R.fund)
    R.style = checkData(R.style)

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
            #      A formula has an implied intercept term.  See 'formula' for more details
            #      of allowed formulae.

            #      To remove the intercept term: 'y ~ x - 1' is a line through the
            #      origin.  A model with no intercept can be also specified as 'y ~ x
            #      + 0' or 'y ~ 0 + x'.

            column.lm = lm(R.fund[, fund.col] ~ 0 + ., data = R.style)
            if (selection == "AIC") { # @todo: add "BIC" case, where k = log(n) and n is style.cols?
                # @todo: support multiple colums
                column.result = step(column.lm) # find the model with minimum AIC value
                if(fund.col == 1 )
                    column.weights=data.frame(matrix(rep(0,length(style.colnames)*fund.cols), nrow = length(style.colnames), ncol = fund.cols),row.names=style.colnames)
                column.coef = as.data.frame(coef(column.result))

                if(length(coef(column.result))>0){
                    row.loc = match(rownames(column.coef), rownames(column.weights))
                    for(i in 1:length(row.loc)) column.weights[row.loc[i],fund.col]=column.coef[i,1]
                }
            }
            else {
                column.result = column.lm
                column.weights = as.data.frame(coef(column.lm))
            }
#             column.weights = as.data.frame(coef(column.result)[-1])
#             colnames(column.weights)= style.colnames[fund.col]
            rownames(column.weights) = colnames(R.style)
            colnames(column.weights) = colnames(R.fund)[fund.col]

            R2 = as.data.frame(summary(column.result)$r.squared)
            adjR2 = as.data.frame(summary(column.result)$adj.r.squared)
            colnames(R2) = colnames(R.fund)[fund.col]
            colnames(adjR2) = colnames(R.fund)[fund.col] 
            rownames(R2) = "R-squared"
            rownames(adjR2) = "Adj R-squared"

            if(method == "normalized") {
                column.weights = column.weights/sum(column.weights)
            }
            if(fund.col == 1){
                result.weights = column.weights
                result.R2 = R2
                result.adjR2 = adjR2
            }
            else{
                result.weights = cbind(result.weights, column.weights)
                result.R2 = cbind(result.R2, R2)
                result.adjR2 = cbind(result.adjR2, adjR2)
            }
        }
        else stop("Method is mis-specified.  Select from \"constrained\", \"unconstrained\", or  \"normalized\"")
    }
    result = list(weights = result.weights, R.squared = result.R2, adj.R.squared = result.adjR2 )

    return(result)

}