#' Compare SFM estimated using robust estimators with that estimated by OLS 
#'
#' This function for single factor models (SFM’s) with a slope  and intercept 
#' allows the user to easily make a scatter plot of asset returns versus benchmark 
#' returns, such as the SP500, with two overlaid straight-line fits, one obtained 
#' using least squares (LS), which can be very adversely influenced by outliers, 
#' and one obtained using a highly robust regression estimate that is not much 
#' influenced by outliers.  The plot allows the user to see immediately whether 
#' or not any outliers result in a distorted LS fit that does not fit the bulk 
#' of the data, while the robust estimator results In a good fit to the bulk of
#' the data.  The plot contains a legend with LS slope estimate and the robust 
#' slope estimate, with estimate standard errors in parentheses.
#' @details The function chart.SFM computes the robust fit with the function 
#' lmrobdetMM contained in the  package RobStatTM available at CRAN. The function 
#' lmrobdetMM has a default robust regression estimate called the mopt estimate, 
#' which is used by chart.SFM. For details on lmrobdetMM, see reference [1]. The 
#' plot made by chart.SFM has two parallel dotted lines that define a strip in 
#' the asset returns versus benchmark returns space, and data points that fall 
#' outside that strip are defined as outliers, and as such are rejected, i.e., 
#' deleted by the lmrobdetMM estimator.
#' 
#' @param Ra an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param Rb return vector of the benchmark asset
#' @param Rf risk free rate, in same period as your returns
#' @param main Title of the generated plot. Defaults to "lm vs lmRobdetMM"
#' @param ylim Limits on the y-axis of the plots. Defaults to min-max
#' @param xlim Limits on the x-axis of the plots. Defaults to min-max
#' @param family (Optional): 
#'         This is a string specifying the name of the family of loss function
#'         to be used (current valid options are "bisquare", "opt" and "mopt").
#'         Incomplete entries will be matched to the current valid options. 
#'         Defaults to "mopt".
#'         
#' @param xlab Title of the x-axis of the plots. Defaults to "Benchmark Returns"
#' @param ylab Title of the y-axis of the plots. Defaults to "Asset Returns"
#' @param legend.loc Position of legends. See plot() function for more info.
#' @param makePct If Returns should be converted to percentage. Defaults to False
#' @author Dhairya Jain, Doug Martin, Dan Xia
#' @references Martin, R. D. and Xia, D. Z. (2021).  Robust Time Series Factor Models, 
#' SSRN: https://ssrn.com/abstract=3905345. \emph{To appear in the Journal of Asset Management in 2022} 
#' \cr Ruppert, David. \emph{Statistics and Finance, an
#' Introduction}. Springer. 2004. \cr Sharpe, W.F. Capital Asset Prices: A theory of market
#' equilibrium under conditions of risk. \emph{Journal of finance}, vol 19,
#' 1964, 425-442. \cr
###keywords ts multivariate distribution models
#' @examples
#'    data(managers)
#'    
#' 		mgrs <- managers["2002/"]  # So that all have managers have complete history
#'    names(mgrs)[7:10] <- c("LSEQ","SP500","Bond10Yr","RF") # Short names for last 3
#'    plot.zoo(mgrs)
#'    
#'    chart.SFM(mgrs$HAM1, mgrs$SP500, Rf=mgrs$RF)	
#' 			        
#'    for(k in 1:7){
#'         chart.SFM(mgrs[,k],mgrs$SP500,mgrs$RF,makePct = TRUE,
#'                        main = names(mgrs[,k]))
#'    }
#'    
#'   
#' @importFrom stats summary.lm
#' @rdname chart.SFM
#' @export chart.SFM
chart.SFM <- function(Ra, Rb, Rf = 0, main = NULL, ylim = NULL, xlim = NULL, 
                      family = "mopt", xlab = NULL, ylab = NULL, 
                      legend.loc = "topleft", makePct = FALSE){
  # @author Dhairya Jain
  
  # DESCRIPTION:
  # Wrapper Function to graph robust SFM estimation model vs OLS SFM estimates.
  
  # Inputs:
  # Ra: time series of returns for the asset being tested
  # Rb: time series of returns for the benchmark the asset is being gauged against
  # Rf: risk free rate in the same periodicity as the returns.  May be a time series
  #     of the same length as x and y.
  # family (Optional): 
  #         If method == "Robust": 
  #           This is a string specifying the name of the family of loss function
  #           to be used (current valid options are "bisquare", "opt" and "mopt").
  #           Incomplete entries will be matched to the current valid options. 
  #           Defaults to "mopt".
  #         Else: the parameter is ignored
  # main: Title of the generated plot. Defaults to " xlab ~ ylab "
  # ylim: Limits on the y-axis of the plots. Defaults to min-max
  # xlim: Limits on the x-axis of the plots. Defaults to min-max
  # family (Optional): 
  #   This is a string specifying the name of the family of loss function
  #   to be used (current valid options are "bisquare", "opt" and "mopt").
  #   Incomplete entries will be matched to the current valid options. 
  #   Defaults to "mopt".
  # xlab Title of the x-axis of the plots. Defaults to "Benchmark Returns"
  # ylab Title of the y-axis of the plots. Defaults to "Asset Returns"
  # legend.loc: Position of legends. See plot() function for more info.
  # makePct: If Returns should be converted to percentage. Defaults to False
  # Output:
  if (dim(Ra)[2]!=1 || dim(Rb)[2]!=1){
    stop("Both Ra and Rb should be uni-dimentional vectors")
  }
  # Graphs comparing models
  xlab <- ifelse(is.null(xlab), names(Rb), xlab)
  ylab <- ifelse(is.null(ylab), names(Ra), ylab)
  
  # Get the NCOL and colnames from Ra, and Rb
  Ra.ncols <- NCOL(Ra);
  Rb.ncols <- NCOL(Rb);
  Ra.colnames <- colnames(Ra);
  Rb.colnames <- colnames(Rb)
  
  # Get the excess returns of Ra, Rb over Rf
  xRa = Return.excess(Ra, Rf)
  xRb = Return.excess(Rb, Rf)
  
  # Scale the values to percentages if required
  if(makePct){
    xRa = xRa*100
    xRb = xRb*100
  }
  
  # Evaluate the model
  models <- getResults(xRa, xRb, Ra.ncols, Rb.ncols, family=family, method="Both", subset=T)
  
  # Initialize the xAxis and yAxis data
  x = array(xRb)
  y = array(xRa)
  
  # Plot the model
  .plot_models(x, y, models[[1]], main, ylim, xlim, family, xlab, ylab, legend.loc, makePct, F)
}

.plot_models = function(x, y, models, mainText = NULL, ylimits = NULL, xlimits = NULL, family = "mopt",
                        xlab = NULL, ylab = NULL, legendPos = "topleft", makePct = FALSE, lm.outliers=F){
  
  requireNamespace("stats",quietly=TRUE)
  
  # Define some constants
  g <- 1
  f <- 3
  
  # Get the robust and linear models 
  fit.Rob <- models$robust$model
  fit.ls <- models$LS$model
  
  # Set the xLabels and yLabels
  xlab <- ifelse(is.null(xlab), "Benchmark Returns", xlab)
  ylab <- ifelse(is.null(ylab), "Asset Returns", ylab)

  # Set the Title of the plot
  mainText <- ifelse(is.null(mainText), paste(xlab, "~", ylab), mainText)
  
  # Change the xLabels and yLabels to percentage if required (After setting the title)
  if (makePct){
    xlab = paste(xlab," (%)")
    ylab = paste(ylab," (%)")
  }
  
  # Plot the data points
  plot(x,y, xlab = xlab, ylab = ylab, type = "n",
       ylim = ylimits, xlim = xlimits, main = mainText, 
       cex.main = 1.5, cex.lab = 1.5)
  
  # Plot the respective lines for LS and Rob
  abline(fit.Rob, col="black", lty=1, lwd=2)
  abline(fit.ls, col="red", lty=2, lwd=2)

  # Plot the outlier Lines and get the points that lie within the boundaries
  if (lm.outliers){
    # The upper outlier line
    abline(g*fit.ls$coef[1]+f*summary.lm(fit.ls)$sigma[1], fit.ls$coef[2], lty=3, col="black")
    # The lower outlier line  
    abline(g*fit.ls$coef[1]-f*summary.lm(fit.ls)$sigma[1], fit.ls$coef[2], lty=3, col="black")
    # Get the line equation 
    define_line <- function(X){
      return (g*fit.ls$coef[1] + X*fit.ls$coef[2])
    }
    # Find the outlier data points
    ids = which(abs(y - define_line(x))> f*summary.lm(fit.ls)$sigma[1]) 
  }
  else{
    # The upper outlier line
    abline(g*fit.Rob$coef[1]+f*fit.Rob$scale.S, fit.Rob$coef[2], lty=3, col="black")
    # The lower outlier line  
    abline(g*fit.Rob$coef[1]-f*fit.Rob$scale.S, fit.Rob$coef[2], lty=3, col="black")
    # Get the line equation 
    define_line <- function(X){
      return (g*fit.Rob$coef[1] + X*fit.Rob$coef[2])
    }
    # Find the outlier data points
    ids = which(abs(y - define_line(x))> f*fit.Rob$scale.S) 
    # ids=which(fit.Rob$rweights==0) # This was used perviously, but gives wrong output
  }
  
  # Make the non-outliers to be small dark circles
  if (length(ids) == 0) {
    # Case of no outliers, all are dark circles
    points(x, y, pch = 20)
  } 
  else {
    # Non outliers are small dark circles
    points(x[-ids], y[-ids], pch = 19)
    # Outliers are light big circles
    points(x[ids], y[ids], pch = 1, cex = 2.0)
  }
  
  # Create the legend for the model Lines
  legend(x = legendPos,
         legend = as.expression(c(bquote(" "~.(family)~"  " ~ hat(beta) == .(round(summary(fit.Rob)$coefficients[2, 1], 2)) ~
                                           "(" ~ .(round(summary(fit.Rob)$coefficients[2, 2], 2)) ~ ")"),
                                  bquote("  LS       " ~ hat(beta) == .(round(summary(fit.ls)$coefficients[2, 1], 2)) ~
                                           "(" ~ .(round(summary(fit.ls)$coefficients[2, 2], 2)) ~ ")"))),
         lty=c(1,2), col=c("black", "red"), bty="n", cex=1.5 )
}