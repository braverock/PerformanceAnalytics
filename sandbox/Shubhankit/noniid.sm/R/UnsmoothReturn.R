#' @title  Unsmooth Time Series Return 
#' 
#' @description Creates a table of estimates of moving averages for comparison across
#' multiple instruments or funds as well as their standard error and
#' smoothing index , which is Compenent Decomposition of Table of Unsmooth Returns
#' 
#' @details The estimation method is based on a maximum likelihood estimation of a moving average 
#' process (we use the innovations algorithm proposed by \bold{Brockwell and Davis} [1991]). The first 
#' step of this approach consists in computing a series of de-meaned observed returns:
#' \deqn{X(t) = R(0,t)- \mu}
#' where \eqn{\mu} is the expected value of the series of observed returns.
#' As a consequence, the above equation can be written as :
#' \deqn{X(t)= \theta(0)\eta(t) + \theta(1)\eta(t-1) + .....   + \theta(k)\eta(t-k)}
#' with the additional assumption that : \bold{\eqn{\eta(k)= N(0,\sigma(\eta)^2)}}
#' The structure of the model and the two constraints suppose that the complete integration of 
#'information in the price of the considered asset may take up to k periods because of its illiquidity. 
#'In addition, according to Getmansky et al., this model is in line with previous models of nonsynchronous trading such as the one developed by \bold{Cohen, Maier, Schwartz and Whitcomb} 
#' [1986].
#' Smoothing has an impact on the third and fourth moments of the returns distribution too.
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param q number of series lags
#' @param ... any other passthru parameter
#' @references   Cavenaile, Laurent, Coen, Alain and Hubner, Georges,\emph{ The Impact of Illiquidity and Higher Moments of Hedge Fund Returns on Their Risk-Adjusted Performance and Diversification Potential} (October 30, 2009). Journal of Alternative Investments, Forthcoming. Available at SSRN: \url{http://ssrn.com/abstract=1502698} Working paper is at \url{http://www.hec.ulg.ac.be/sites/default/files/workingpapers/WP_HECULg_20091001_Cavenaile_Coen_Hubner.pdf}
#' @author Shubhankit Mohan
#' @keywords ts smooth return models
#' @seealso Reutrn.Geltner Reutrn.GLM  Return.Okunev
#' @rdname UnSmoothReturn
#' @examples
#' library(PerformanceAnalytics)
#' library(tseries)
#' data(managers)
#' UnsmoothReturn(managers,3)
#' @export 

UnsmoothReturn<-
  function(R = NULL,q=2,  ...)
  {
    columns = 1
    columnnames = NULL
    #Error handling if R is not NULL
    if(!is.null(R)){
      x = checkData(R)
      columns = ncol(x)
      n = nrow(x)
      count = q
      columns = ncol(x)
      columnnames = colnames(x)
      
      # Calculate AutoCorrelation Coefficient
      for(column in 1:columns) { # for each asset passed in as R
        y = R[,column]
        y=na.omit(y)
        
        acflag6 = acf(y,plot=FALSE,lag.max=6)[[1]][2:7]
        values = sum(acflag6*acflag6)/(sum(acflag6)*sum(acflag6))
        
        if(column == 1) {
          result.df = data.frame(Value = values)
          colnames(result.df) = columnnames[column]
        }
        else {
          nextcol = data.frame(Value = values)
          colnames(nextcol) = columnnames[column]
          result.df = cbind(result.df, nextcol)
        }
      }
      return(as.numeric(result.df)*R)  # Unsmooth Return
      
    }  
  }