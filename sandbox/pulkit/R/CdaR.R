#'@title
#' Calculate Uryasev's proposed Conditional Drawdown at Risk (CDD or CDaR)
#' measure
#' 
#' @description
#' For some confidence level \eqn{p}, the conditional drawdown is the the mean
#' of the worst \eqn{p\%} drawdowns.
#' 
#' @aliases CDD CDaR
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param weights portfolio weighting vector, default NULL, see Details
#' @param geometric utilize geometric chaining (TRUE) or simple/arithmetic chaining (FALSE) to aggregate returns,
#' default TRUE
#' @param invert TRUE/FALSE whether to invert the drawdown measure.  see
#' Details.
#' @param p confidence level for calculation, default p=0.95
#' @param \dots any other passthru parameters
#' @author Brian G. Peterson
#' @seealso   \code{\link{CdarMultiPath}} \code{\link{AlphaDrawdown}} 
#'\code{\link{MultiBetaDrawdown}} \code{\link{BetaDrawdown}}
#' @references Chekhlov, A., Uryasev, S., and M. Zabarankin. Portfolio
#' Optimization With Drawdown Constraints. B. Scherer (Ed.) Asset and Liability
#' Management Tools, Risk Books, London, 2003
#' http://www.ise.ufl.edu/uryasev/drawdown.pdf
#' @keywords ts multivariate distribution models
#' @examples
#' library(lpSolve)
#' data(edhec)
#' t(round(CDaR(edhec),4))
#' 
#' @export 


CDaR<-function (R, weights = NULL, geometric = TRUE, invert = TRUE, p = 0.95, ...) 
{
  #p = .setalphaprob(p)
  if (is.vector(R) || ncol(R) == 1) {
    R = na.omit(R)
    nr = nrow(R)
    # checking if nr*p is an integer
    if((p*nr) %% 1 == 0){
    drawdowns = -Drawdowns(R)
    drawdowns = drawdowns[order(drawdowns),increasing = TRUE]
    print(drawdowns)
    # average of the drawdowns greater the (1-alpha).100% largest drawdowns 
    result = -(1/((1-p)*nr))*sum(drawdowns[((p)*nr):nr])
    }
    else{
        # CDaR using the CVaR function
        # result = ES(Drawdowns(R),p=p,method="historical")
        # if nr*p is not an integer
      f.obj = c(rep(0,nr),rep(((1/(1-p))*(1/nr)),nr),1)
      
      # k varies from 1:nr
      # constraint : -uk +zk +y >= 0
      f.con = cbind(-diag(nr),diag(nr),1)
      f.dir = c(rep(">=",nr))
      f.rhs = c(rep(0,nr))
      
      # constraint : uk -uk-1 >= -rk
      ut = diag(nr)
      ut[-1,-nr] = ut[-1,-nr] - diag(nr - 1)
      f.con = rbind(f.con,cbind(ut,matrix(0,nr,nr),0))
      f.dir = c(rep(">=",nr))
      f.rhs = c(f.rhs,-R)
      
      # constraint : zk >= 0
      f.con = rbind(f.con,cbind(matrix(0,nr,nr),diag(nr),0))
      f.dir = c(rep(">=",nr))
      f.rhs = c(f.rhs,rep(0,nr))
      
      # constraint : uk >= 0 
      f.con = rbind(f.con,cbind(diag(nr),matrix(0,nr,nr),0))
      f.dir = c(rep(">=",nr))
      f.rhs = c(f.rhs,rep(0,nr))
      
      val = lp("min",f.obj,f.con,f.dir,f.rhs)
      val_disp = lp("min",f.obj,f.con,f.dir,f.rhs,compute.sens = TRUE )
      result = -val$objval
    }
    if (invert) 
      result <- -result

    return(result)
  }
  else {
    R = checkData(R, method = "matrix")
    if (is.null(weights)) {
      result = matrix(nrow = 1, ncol = ncol(R))
      for (i in 1:ncol(R)) {
        result[i] <- CDaR(R[, i, drop = FALSE], p = p, 
                         geometric = geometric, invert = invert, ... = ...)
      }
      dim(result) = c(1, NCOL(R))
      colnames(result) = colnames(R)
      rownames(result) = paste("Conditional Drawdown ", round(p,3)*100, "%", sep = "")
    }
    else {
      portret <- Return.portfolio(R, weights = weights, 
                                  geometric = geometric)
      result <- CDaR(portret, p = p, geometric = geometric, 
                    invert = invert, ... = ...)
    }
    return(result)
  }
}
