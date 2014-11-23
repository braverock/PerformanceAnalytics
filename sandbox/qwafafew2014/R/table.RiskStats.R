# Additional and re-organized tables for WB presentations

table.RiskStats <-
function (R, ci = 0.95, scale = NA, Rf = 0, MAR = .1/12, p= 0.95, digits = 4)
{# @author Peter Carl
    # Risk Statistics: Statistics and Stylized Facts

    y = checkData(R, method = "zoo")
    if(!is.null(dim(Rf)))
        Rf = checkData(Rf, method = "zoo")
    # Set up dimensions and labels
    columns = ncol(y)
    rows = nrow(y)
    columnnames = colnames(y)
    rownames = rownames(y)

    if(is.na(scale)) {
        freq = periodicity(y)
        switch(freq$scale,
            minute = {stop("Data periodicity too high")},
            hourly = {stop("Data periodicity too high")},
            daily = {scale = 252},
            weekly = {scale = 52},
            monthly = {scale = 12},
            quarterly = {scale = 4},
            yearly = {scale = 1}
        )
    }

    # for each column, do the following:
    for(column in 1:columns) {
        x = na.omit(y[,column,drop=FALSE])
        # for each column, make sure that R and Rf are for the same dates
        if(!is.null(dim(Rf))){ # if Rf is a column
            z = merge(x,Rf)
            zz = na.omit(z)
            x = zz[,1,drop=FALSE]
            Rf.subset = zz[,2,drop=FALSE]
        }
        else { # unless Rf is a single number
            Rf.subset = Rf
        }

        z = c(
                Return.annualized(x, scale = scale), 
                StdDev.annualized(x, scale = scale),
                SharpeRatio.annualized(x, scale = scale, Rf = Rf),
                DownsideDeviation(x,MAR=0)*sqrt(scale),# Add annualization to this function
                SortinoRatio(x)*sqrt(scale), # New function adds annualization
                PerformanceAnalytics:::AverageDrawdown(x),
                maxDrawdown(x),
                SterlingRatio(x),
                VaR(x, p=p,method="historical"),
                ES(x, p=p,method="historical"),
                skewness(x), 
                kurtosis(x),
                VaR(x, p=p),
                ES(x, p=p),
                SharpeRatio(x, p=p, Rf=Rf, FUN="ES", annualize=TRUE),
                length(x)
                )
        znames = c(
                "Annualized Return", 
                "Annualized Std Dev", 
                "Annualized Sharpe Ratio",
                "Annualized Downside Deviation",
                "Annualized Sortino Ratio",
                "Average Drawdown",
                "Maximum Drawdown",
                "Sterling Ratio (10%)",
                paste("Historical VaR (",base::round(p*100,1),"%)",sep=""),
                paste("Historical ETL (",base::round(p*100,1),"%)",sep=""),
                "Skewness",
                "Excess Kurtosis",
                paste("Modified VaR (",base::round(p*100,1),"%)",sep=""),
                paste("Modified ETL (",base::round(p*100,1),"%)",sep=""),
                paste("Annualized Modified Sharpe Ratio (ETL ", base::round(p*100,1),"%)",sep=""),
                "# Obs"
                )
        if(column == 1) {
            resultingtable = data.frame(Value = z, row.names = znames)
        }
        else {
            nextcolumn = data.frame(Value = z, row.names = znames)
            resultingtable = cbind(resultingtable, nextcolumn)
        }
    }
    colnames(resultingtable) = columnnames
    ans = base::round(resultingtable, digits)
    ans
}

table.PerfStats <-
function (R, scale = NA, Rf = 0, digits = 4)
{# @author Peter Carl
    # Performance Statistics: Statistics and Stylized Facts

    y = checkData(R)
    if(!is.null(dim(Rf)))
        Rf = checkData(Rf)
    # Set up dimensions and labels
    columns = ncol(y)
    rows = nrow(y)
    columnnames = colnames(y)
    rownames = rownames(y)

    if(is.na(scale)) {
        freq = periodicity(y)
        switch(freq$scale,
            minute = {stop("Data periodicity too high")},
            hourly = {stop("Data periodicity too high")},
            daily = {scale = 252},
            weekly = {scale = 52},
            monthly = {scale = 12},
            quarterly = {scale = 4},
            yearly = {scale = 1}
        )
    }

    # for each column, do the following:
    for(column in 1:columns) {
        x = na.omit(y[,column,drop=FALSE])
        # for each column, make sure that R and Rf are for the same dates
        if(!is.null(dim(Rf))){ # if Rf is a column
            z = merge(x,Rf)
            zz = na.omit(z)
            x = zz[,1,drop=FALSE]
            Rf.subset = zz[,2,drop=FALSE]
        }
        else { # unless Rf is a single number
            Rf.subset = Rf
        }

        z = c(
                Return.cumulative(x),
                Return.annualized(x, scale = scale), 
                StdDev.annualized(x, scale = scale),
                length(subset(x, x>0)),
                length(subset(x, x<=0)),
                length(subset(x, x>0))/length(x),
                mean(subset(x, x>0)),
                mean(subset(x, x<=0)),
                mean(x),
                AverageDrawdown(x),
                AverageRecovery(x)
                )
        znames = c(
                "Cumulative Return",
                "Annualized Return", 
                "Annualized Std Dev", 
                "# Positive Months",
                "# Negative Months",
                "% Positive Months",
                "Average Positive Month",
                "Average Negative Month",
                "Average Month",
                "Average Drawdown",
                "Average Months to Recovery"
                )
        if(column == 1) {
            resultingtable = data.frame(Value = z, row.names = znames)
        }
        else {
            nextcolumn = data.frame(Value = z, row.names = znames)
            resultingtable = cbind(resultingtable, nextcolumn)
        }
    }
    colnames(resultingtable) = columnnames
    ans = base::round(resultingtable, digits)
    ans
}

table.RiskContribution <- function(R, p, ..., weights=NULL, scale=NA, geometric = TRUE) {

    R = na.omit(R)
    if(is.null(weights)) {
        message("no weights passed in, assuming equal weighted portfolio")
        weights = rep(1/dim(R)[[2]], dim(R)[[2]])
    }
    if (is.na(scale)) {
      freq = periodicity(R)
      switch(freq$scale, minute = {
          stop("Data periodicity too high")
      }, hourly = {
          stop("Data periodicity too high")
      }, daily = {
          scale = 252
      }, weekly = {
          scale = 52
      }, monthly = {
          scale = 12
      }, quarterly = {
          scale = 4
      }, yearly = {
          scale = 1
      })
    }
    
    # Returns
    # ret.col = colMeans(R)*weights
    ret.col = Return.annualized(R, geometric=geometric)*weights
    percret.col = ret.col/sum(ret.col)
    result = cbind(t(ret.col), t(percret.col))
    # Standard Deviation
    sd.cols = StdDev(R, weights=weights, invert=TRUE, portfolio_method="component", p=(1-1/12))
    result = cbind(sd.cols$contribution*sqrt(scale), sd.cols$pct_contrib_StdDev, result)
    # VaR?
    var.cols = VaR(R, weights=weights, method="gaussian", portfolio_method="component", p=(1-1/12))
    result = cbind(var.cols$contribution, var.cols$pct_contrib_VaR, result)
    
    mvar.cols = VaR(R, weights=weights, method="gaussian", portfolio_method="component", p=(1-1/12))
    result = cbind(mvar.cols$contribution, mvar.cols$pct_contrib_VaR, result)
    
    # ES
    es.cols = ES(R, weights=weights, method="gaussian", portfolio_method="component", p=(1-1/12))
    result = cbind(es.cols$contribution, es.cols$pct_contrib_ES, result)

    mes.cols = ES(R, weights=weights, method="modified", portfolio_method="component", p=(1-1/12))
    result = cbind(weights, mes.cols$contribution, mes.cols$pct_contrib_MES, result)
    total = colSums(result)
    
    result = rbind(result, colSums(result))
    rownames(result) = c(colnames(R),"Total")
#     colnames(result) = c("Weights", "Contribution to mETL", "Percentage Contribution to mETL", "Contribution to gETL", "Percentage Contribution to gETL", "Contribution to Annualized StdDev", "Percentage Contribution to StdDev", "Contribution to Annualized E(R)", "Percentage Contribution to E(R)")

    colnames(result) = c("Weights", "Contribution to mETL", "%Contribution to mETL", "Contribution to gETL", "%Contribution to gETL", "Contribution to mVaR", "%Contribution to mVaR", "Contribution to gVaR", "%Contribution to gVaR", "Contribution to Annualized StdDev", "%Contribution to StdDev", "Contribution to Annualized E(R)", "%Contribution to E(R)")
    return(result)

}
