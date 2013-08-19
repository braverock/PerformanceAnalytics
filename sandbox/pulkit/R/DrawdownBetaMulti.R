#'@title
#'Drawdown Beta for Multiple path
#' 
#'@description
#'The drawdown beta is formulated as follows
#'
#'\deqn{\beta_DD^i = \frac{{\sum_{s=1}^S}{\sum_{t=1}^T}p_s{q_t^\asterisk}{(w_{s,k^{\asterisk}(s,t)^i}-w_{st}^i)}}{D_{\alpha}(w^M)}}
#' here \eqn{\beta_DD} is the drawdown beta of the instrument for multiple sample path.
#'\eqn{k^{\asterisk}(s,t)\in{argmax_{t_{\tau}{\le}k{\le}t}}w_{sk}^p(x^\asterisk)}
#'
#'The numerator in \eqn{\beta_DD} is the average rate of return of the 
#'instrument over time periods corresponding to the \eqn{(1-\alpha)T} largest
#'drawdowns of the optimal portfolio, where \eqn{w_t - w_k^{\asterisk}(t)} 
#'is the cumulative rate of return of the instrument from the optimal portfolio
#' peak time \eqn{k^\asterisk(t)} to time t.
#'
#'The difference in CDaR and standard betas can be explained by the 
#'conceptual difference in beta definitions: the standard beta accounts for
#'the fund returns over the whole return history, including the periods 
#'when the market goes up, while CDaR betas focus only on market drawdowns 
#'and, thus, are not affected when the market performs well.
#'
#'@param R an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns
#'@param Rm Return series of the optimal portfolio an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns
#'@param p confidence level for calculation ,default(p=0.95)
#'@param weights portfolio weighting vector, default NULL, see Details
#' @param geometric utilize geometric chaining (TRUE) or simple/arithmetic chaining (FALSE) to aggregate returns, default TRUE
#' @param type The type of BetaDrawdown if specified alpha then the alpha value given is taken (default 0.95). If "average" then
#' alpha = 0 and if "max" then alpha = 1 is taken.
#'@param \dots any passthru variable.
#'
#'@author Pulkit Mehrotra
#' @seealso \code{\link{ES}} \code{\link{maxDrawdown}} \code{\link{CdarMultiPath}} 
#'\code{\link{AlphaDrawdown}} \code{\link{CDaR}} \code{\link{BetaDrawdown}}

#'@references
#'Zabarankin, M., Pavlikov, K., and S. Uryasev. Capital Asset Pricing Model 
#'(CAPM) with Drawdown Measure.Research Report 2012-9, ISE Dept., University 
#'of Florida,September 2012.
#'
#'@examples
#'MultiBetaDrawdown(cbind(edhec,edhec),cbind(edhec[,2],edhec[,2]),sample = 2,ps=c(0.4,0.6))
#'BetaDrawdown(edhec[,1],edhec[,2]) #expected value 0.5390431
#'@export

MultiBetaDrawdown<-function(R,Rm,sample,ps,h=0,p=0.95,weights=NULL,geometric=TRUE,type=c("alpha","average","max"),...){

    # DESCRIPTION:
    #
    # The function is used to find the Drawdown Beta for multiple sample path.
    # 
    # INPUT:
    # The Return series of the portfolio and the optimal portfolio
    # is taken as the input.
    #
    # OUTPUT:
    # The Drawdown beta for multiple sample path is given as the output.   


    x = checkData(R)
    xm = checkData(Rm)
    columnnames = colnames(R)
    columns = ncol(R)
    drawdowns_m = Drawdowns(Rm)
    type = type[1]
    nr = nrow(Rm)
    if(type=="average"){
        p = 0
    }
    if(type == "max"){
        p = 1
    }
    # if nr*p is not an integer
    #f.obj = c(rep(0,nr),rep((1/(1-p))*(1/nr),nr),1)
    drawdowns = -as.matrix(drawdowns_m)
    # Optimization to define Q for the optimal portfolio 
    # The objective function is defined 
    f.obj = NULL
    for(i in 1:sample){
      for(j in 1:nr){
        f.obj = c(f.obj,ps[i]*drawdowns[j,i])
      }
    }
    f.con = NULL
    # constraint 1: ps.qst = 1
    for(i in 1:sample){
      for(j in 1:nr){
        f.con = c(f.con,ps[i])
      }
    }
    f.con = matrix(f.con,nrow =1)
    f.dir = "=="
    f.rhs = 1
    # constraint 2 : qst >= 0
    for(i in 1:sample){
      for(j in 1:nr){
        r<-rep(0,sample*nr)
        r[(i-1)*sample+j] = 1
        f.con = rbind(f.con,r)
      }
    }
    f.dir = c(f.dir,rep(">=",sample*nr))
    f.rhs = c(f.rhs,rep(0,sample*nr))
    
    
    # constraint 3 : qst =< 1/(1-alpha)*T
    for(i in 1:sample){
      for(j in 1:nr){
        r<-rep(0,sample*nr)
        r[(i-1)*sample+j] = 1
        f.con = rbind(f.con,r)
      }
    }
    f.dir = c(f.dir,rep("<=",sample*nr))
    f.rhs = c(f.rhs,rep(1/(1-p)*nr,sample*nr))
    
    val = lp("max",f.obj,f.con,f.dir,f.rhs)
    q = matrix(val$solution,ncol = sample)
    # TODO INCORPORATE WEIGHTS

    if(geometric){
        cumul_xm = cumprod(xm+1)-1
    }
    else{
        cumul_xm = cumsum(xm)
    }
    # Function to calculate Drawdown beta for multipath
    multiDDbeta<-function(x){
        boolean = (cummax(cumul_xm)==cumul_xm)
        index = NULL
        for(i in 1:sample){
            for(j in 1:nrow(Rm)){
                if(boolean[j,i] == TRUE){
                    index = c(index,j)
                    b = j
                    }
                    else{
                        index = c(index,b)
                        }
                        }
        }
        index = matrix(index,ncol = sample)
        beta_dd = 0
        for(i in 1:sample){
            beta_dd = beta_dd + sum(ps[i]*q[,i]*(as.numeric(x[index[,i],i])-x[,i]))
        }
        beta_dd = beta_dd/CdarMultiPath(Rm,ps=ps,p=p,sample = sample)
        return(beta_dd)
    }

    result = NULL

    for (i in 1:(ncol(R)/sample)) {
        ret<-NULL
        for(j in 1:sample){
            ret<-cbind(ret,R[,(j-1)*ncol(R)/sample+i])
        }
      result <-c(result, multiDDbeta(ret))
    }
    result = matrix(result,nrow = 1)
    colnames(result) = colnames(R)[1:(ncol(R)/sample)]
    #colnames(result) = colnames(R)[1:ncol(R)/sample]
    rownames(result) = paste("Conditional Drawdown","(",p*100,"%)",sep="")
    return(result)
 }




