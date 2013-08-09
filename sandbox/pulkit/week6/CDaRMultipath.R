#'@title
#'Conditional Drawdown at Risk for Multiple Sample Path
#'
#'@desctipion
#'
#' For a given \eqn{\alpha \epsilon [0,1]} in the multiple sample-paths setting,CDaR, 
#' denoted by \eqn{D_{\alpha}(w)}, is the average of \eqn{(1-\alpha).100\%} drawdowns 
#' of the set {d_st|t=1,....T,s = 1,....S}, and is defined by 
#'
#' \deqn{D_\alpha(w) = \max_{{q_st}{\epsilon}Q}{\sum_{s=1}^S}{\sum_{t=1}^T}{p_s}{q_st}{d_st}},
#' 
#' where 
#' 
#' \deqn{Q = \left\{ \left\{ q_st\right\}_{s,t=1}^{S,T} | \sum_{s = 1}^S \sum_{t = 1}^T{p_s}{q_st} = 1, 0{\leq}q_st{\leq}\frac{1}{(1-\alpha)T}, s = 1....S, t = 1.....T \right\}}
#' 
#' For \eqn{\alpha = 1} , \eqn{D_\alpha(w)} is defined by (3) with the constraint 
#' \eqn{0{\leq}q_st{\leq}\frac{1}{(1-\alpha)T}}, in Q replaced by \eqn{q_st{\geq}0}
#' 
#'  As in the case of a single sample-path, the CDaR definition includes two special cases : 
#' (i) for \eqn{\alpha = 1},\eqn{D_1(w)} is the maximum drawdown, also called drawdown from 
#' peak-to-valley, and (ii) for \eqn{\alpha} = 0, \eqn{D_\alpha(w)} is the average drawdown
#'  
#'@param R an xts, vector, matrix,data frame, timeSeries or zoo object of multiple sample path returns
#'@param ps the probability for each sample path 
#'@param scen the number of scenarios in the Return series
#'@param instr the number of instruments in the Return series
#'@param geometric utilize geometric chaining (TRUE) or simple/arithmetic 
#'chaining (FALSE) to aggregate returns, default TRUE
#'@param p confidence level for calculation ,default(p=0.95)
#'@param \dots any other passthru parameters
#'
#'@references
#'Zabarankin, M., Pavlikov, K., and S. Uryasev. Capital Asset Pricing Model (CAPM)
#' with Drawdown Measure.Research Report 2012-9, ISE Dept., University of Florida, 
#' September 2012 


CdarMultiPath<-function (R,ps,sample,instr, geometric = TRUE,p = 0.95, ...) 
{
  
  #p = .setalphaprob(p)
  R = na.omit(R)
  nr = nrow(R)

  # ERROR HANDLING and TESTING
  #if(sample == instr){

  #}

  multicdar<-function(x){
  # checking if nr*p is an integer
  if((p*nr) %% 1 == 0){
    drawdowns = as.matrix(Drawdowns(x))
    drawdowns = drawdowns(order(drawdowns),decreasing = TRUE)
    # average of the drawdowns greater the (1-alpha).100% largest drawdowns 
    result = (1/((1-p)*nr(x)))*sum(drawdowns[((1-p)*nr):nr])
  }
  else{ # if nr*p is not an integer
    #f.obj = c(rep(0,nr),rep((1/(1-p))*(1/nr),nr),1)
   drawdowns = -as.matrix(Drawdowns(x))   
    
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
    
    # constraint 1:
    # f.con = cbind(-diag(nr),diag(nr),1)
    # f.dir = c(rep(">=",nr))
    # f.rhs = c(rep(0,nr))
    
    #constatint 2:
    # ut = diag(nr)
    # ut[-1,-nr] = ut[-1,-nr] - diag(nr - 1)
    # f.con = rbind(f.con,cbind(ut,matrix(0,nr,nr),1))
    # f.dir = c(rep(">=",nr))
    # f.rhs = c(f.rhs,-R)
    
    #constraint 3:
    # f.con = rbind(f.con,cbind(matrix(0,nr,nr),diag(nr),1))
    # f.dir = c(rep(">=",nr))
    # f.rhs = c(f.rhs,rep(0,nr))
    
    #constraint 4:
    # f.con = rbind(f.con,cbind(diag(nr),matrix(0,nr,nr),1))
    # f.dir = c(rep(">=",nr))
    # f.rhs = c(f.rhs,rep(0,nr))
    val = lp("max",f.obj,f.con,f.dir,f.rhs)
    result = val$objval
  }
}
    R = checkData(R, method = "matrix")
    result = matrix(nrow = 1, ncol = ncol(R)/sample)

    for (i in 1:(ncol(R)/sample)) {
        ret<-NULL
        for(j in 1:sample){
            ret<-cbind(ret,R[,(j-1)*ncol(R)/sample+i])
        }
      result[i] <- multicdar(ret)
    }
    dim(result) = c(1, NCOL(R)/sample)
    colnames(result) = colnames(R)[1:ncol(R)/sample]
    rownames(result) = paste("Conditional Drawdown ", 
                             p * 100, "%", sep = "")
  return(result)
}
