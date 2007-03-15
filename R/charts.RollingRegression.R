charts.RollingRegression = function (R, Rb, width = 12, rf = 0, darken = FALSE, main = NULL, ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # A wrapper to create a panel of RollingRegression charts that demonstrates
    # how the attributes change through time.

    # Inputs:
    # R: a matrix, data frame, or timeSeries, usually a set of monthly returns.
    #   The first column is assumed to be the returns of interest, the next
    #   columns are assumed to be relevant benchmarks for comparison.
    # Rb: a matrix, data frame, or timeSeries that is a set of returns of the
    #   same scale and periodicity as R.
    # rf: the risk free rate.  Remember to set this to the same periodicity
    #   as the data being passed in.
    # attribute: Used to select the regression parameter to use in the chart  May
    #   be any of:
    #     Alpha - shows the y-intercept
    #     Beta - shows the slope of the regression line
    #     R-Squared - shows the fit of the regression to the data
    #

    # Outputs:
    # A stack of three related timeseries line charts

    # FUNCTION:
    asset = checkDataMatrix(R)
    benchmark = checkDataMatrix(Rb)
    columns.a = ncol(asset)
    columns.b = ncol(benchmark)

    if(columns.a > 1 | columns.b > 1)
        legend.loc = "topleft"
    else
        legend.loc = NULL

    plot.new()

    layout(matrix(c(1,2,3)),height=c(1.3,1,1.3),width=1)

    par(mar=c(1,4,4,2))
    if(is.null(main)){
         main = paste("Rolling ",width,"-Month Regression",sep="")
    }

    chart.RollingRegression(asset, benchmark, width = width, rf = rf, darken = darken , attribute = "Alpha", xaxis = F, main = main, ylab = "Alpha", legend.loc=legend.loc, ...)

    par(mar=c(1,4,0,2))

    chart.RollingRegression(asset, benchmark, width = width, rf = rf, darken = darken, attribute = "Beta", main = "", ylab = "Beta", xaxis = F, ...)

    par(mar=c(5,4,0,2))

    chart.RollingRegression(asset, benchmark, width = width, rf = rf, attribute = "R-Squared", darken = darken, main = "", ylab = "R-Squared", ...)

}
