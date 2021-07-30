#' Compare CAPM estimated using robust estimators with that estimated by OLS 
#'
#' This function provies a simple plug and play option for user to compare the
#' SFM estimates by lm and lmrobdetMM functions, using the fit.models framework.
#' This will allow for an easier comparison using charts and tables
#'  
#' @aliases chart.CAPM
#' @param Ra an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param Rb return vector of the benchmark asset
#' @param Rf risk free rate, in same period as your returns   
#' @param family (Optional): 
#'         This is a string specifying the name of the family of loss function
#'         to be used (current valid options are "bisquare", "opt" and "mopt").
#'         Incomplete entries will be matched to the current valid options. 
#'         Defaults to "mopt".
#' @param ylimits Limits on the y-axis of the plots. Defaults to min-max
#' @param mainText MainText for the plot
#' @param legendPos Position of legends. See plot() function for more info.
#' @param makePct If Returns should be converted to percentage. Defaults to False
#' @param goodOutlier
#' @author Doug Martin, Dan Xia, Dhairya Jain
#' @references Sharpe, W.F. Capital Asset Prices: A theory of market
#' equilibrium under conditions of risk. \emph{Journal of finance}, vol 19,
#' 1964, 425-442. \cr Ruppert, David. \emph{Statistics and Finance, an
#' Introduction}. Springer. 2004. \cr Bacon, Carl. \emph{Practical portfolio
#' performance measurement and attribution}. Wiley. 2004. \cr
###keywords ts multivariate distribution models
#' @examples
#' 
#' data(managers)
#'     chart.SFM(managers[,1,drop=FALSE], 
#' 			managers[,8,drop=FALSE], 
#' 			Rf=.035/12)	
#'
#' @rdname chart.SFM
#' @export chart.SFM chart.CAPM
chart.SFM <- chart.CAPM <- 
function(Ra, Rb, Rf = 0, fit.models.chart = F, main = "Title", ylim = NULL, family = c("mopt", "opt", "bisquare"),
                                legend.loc = "topleft", goodOutlier = F, makePct = FALSE)
{
  if (dim(Ra)[2]!=1 || dim(Rb)[2]!=1)
    stop("Both Ra and Rb should be uni-dimentional vectors")
  if (fit.models.chart){
    SFM.fit.models(Ra, Rb, Rf)
  }
  else{
    Ra = checkData(Ra)
    Rb = checkData(Rb)
    if(!is.null(dim(Rf)))
      Rf = checkData(Rf)
    
    xRa = Return.excess(Ra,Rf)
    xRb = Return.excess(Rb,Rf)
    if (makePct) {
      xRb = xRb * 100
      xRa = xRa * 100
    }
    x = array(xRb)
    y = array(xRa)
    models <- SFM.coefficients(Ra, Rb, Rf, efficiency=0.95,family=family, method="Both")
    .plot_models(x, y, models, main, ylim, family, legend.loc, goodOutlier, makePct)
  }
}

.plot_models = function(x, y, models, mainText = NULL, ylimits = NULL, family = "mopt",
                        legendPos = "topleft", 
                        goodOutlier = F, makePct = FALSE){
  fit.mOpt <- models$robust$model
  fit.ls <- models$ordinary$model
  xlab <- "Benchmark Returns"
  ylab <- "Asset Returns"
  if (makePct){
    xlab = xlab + " (%)"
    ylab = ylab + " (%)"
  }
  plot(x,y, xlab = xlab, ylab = ylab, type = "n",
       ylim = ylimits, main = mainText, 
       cex.main = 1.5, cex.lab = 1.5)
  abline(a = fit.mOpt$coefficients[[1]], b = fit.mOpt$coefficients[[2]], col="black", lty=1, lwd=2)
  abline(fit.ls, col="red", lty=2, lwd=2)
  abline(fit.mOpt$coef[1]+3*fit.mOpt$scale, fit.mOpt$coef[2], lty=3, col="black")
  abline(fit.mOpt$coef[1]-3*fit.mOpt$scale, fit.mOpt$coef[2], lty=3, col="black")
  ids=which(fit.mOpt$rweights==0)
  if (length(ids) == 0) {
    points(x, y, pch = 20)
  } else {
    points(x[-ids], y[-ids], pch = 19)
    points(x[ids], y[ids], pch = 1, cex = 2.0)
  }
  legend(x = legendPos,
         legend = as.expression(c(bquote(" "~.(family)~"  " ~ hat(beta) == .(round(summary(fit.mOpt)$coefficients[2, 1], 2)) ~
                                           "(" ~ .(round(summary(fit.mOpt)$coefficients[2, 2], 2)) ~ ")"),
                                  bquote("  LS       " ~ hat(beta) == .(round(summary(fit.ls)$coefficients[2, 1], 2)) ~
                                           "(" ~ .(round(summary(fit.ls)$coefficients[2, 2], 2)) ~ ")"))),
         lty=c(1,2), col=c("black", "red"), bty="n", cex=1.5 )
  
  # Authors:  Doug Martin and Dan Xia 2020
}