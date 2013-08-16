#' @title
#' Drawdown alpha
#'
#' @description
#' Then the difference between the actual rate of return and the rate of
#' return of the instrument estimated by \eqn{\beta_DD{w_T}} is called CDaR
#' alpha and is given by
#'
#' \deqn{\alpha_DD = w_T - \beta_DD{w_T^M}}
#'
#' here \eqn{\beta_DD} is the beta drawdown. The code for beta drawdown can 
#' be found here \code{BetaDrawdown}.
#'
#' Postive \eqn{\alpha_DD} implies that the instrument did better than it was
#' predicted, and consequently, \eqn{\alpha_DD} can be used as a performance
#' measure to rank instrument and to identify those that outperformed their 
#' CAPM predictions
#'
#'
#'@param R an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns
#'@param Rm Return series of the optimal portfolio an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns
#'@param p confidence level for calculation ,default(p=0.95)
#'@param weights portfolio weighting vector, default NULL, see Details
#' @param geometric utilize geometric chaining (TRUE) or simple/arithmetic chaining (FALSE) to aggregate returns, default TRUE
#' @param type The type of BetaDrawdown if specified alpha then the alpha value given is taken (default 0.95). If "average" then alpha = 0 and if "max" then alpha = 1 is taken.
#'@param \dots any passthru variable
#'
#'@references
#'Zabarankin, M., Pavlikov, K., and S. Uryasev. Capital Asset Pricing Model 
#'(CAPM) with Drawdown Measure.Research Report 2012-9, ISE Dept., University 
#'of Florida,September 2012.
#'
#'@examples
#'
#'AlphaDrawdown(edhec[,1],edhec[,2]) ## expected value : 0.5141929
#'
#'AlphaDrawdown(edhec[,1],edhec[,2],type="max") ## expected value : 0.8983177
#'
#'AlphaDrawdown(edhec[,1],edhec[,2],type="average") ## expected value : 1.692592
#'@export


AlphaDrawdown<-function(R,Rm,p=0.95,weights = NULL,geometric = TRUE,type=c("alpha","average","max"),...){
    # DESCRIPTION:
    # This function calculates the drawdown alpha given the return series 
    # and the optimal return series
    # 
    # INPUTS:
    # The return series of the portfolio , the return series of the optimal
    # portfolio. The confidence level, the weights and the type of cumulative
    # returns.

    # OUTPUT:
    # The drawdown alpha is given as the output


    # TODO  ERROR HANDLING
    if(ncol(R) != ncol(Rm)){
        stop("The number of columns in R and Rm should be equal")
    }
    x = checkData(R)
    xm = checkData(Rm)
    beta = BetaDrawdown(R,Rm,p = p,weights=weights,geometric=geometric,type=type,...)
    if(!is.null(weights)){
        x = Returns.portfolio(R,weights)
    }
    if(geometric){
        cumul_x = cumprod(x+1)-1
        cumul_xm = cumprod(xm+1)-1
    }
    else{
        cumul_x = cumsum(x)
        cumul_xm = cumsum(xm)
    }
    x_expected = mean(cumul_x)
    xm_expected = mean(cumul_xm)
    alpha = x_expected - beta*xm_expected
    return(alpha)
}





