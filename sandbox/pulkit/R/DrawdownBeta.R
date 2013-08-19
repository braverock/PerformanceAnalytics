#'@title
#'Drawdown Beta for single path
#' 
#'@description
#'The drawdown beta is formulated as follows
#'
#'\deqn{\beta_DD = \frac{{\sum_{t=1}^T}{q_t^\asterisk}{(w_{k^{\asterisk}(t)}-w_t)}}{D_{\alpha}(w^M)}}
#' here \eqn{\beta_DD} is the drawdown beta of the instrument.
#'\eqn{k^{\asterisk}(t)\in{argmax_{t_{\tau}{\le}k{\le}t}}w_k^M}
#'
#'\eqn{q_t^\asterisk=1/((1-\alpha)T)} if \eqn{d_t^M} is one of the 
#'\eqn{(1-\alpha)T} largest drawdowns \eqn{d_1^{M} ,......d_t^M} of the 
#'optimal portfolio and \eqn{q_t^\asterisk = 0} otherwise. It is assumed 
#'that \eqn{D_\alpha(w^M) {\neq} 0} and that \eqn{q_t^\asterisk} and 
#'\eqn{k^{\asterisk}(t) are uniquely determined for all \eqn{t = 1....T}
#'
#'The numerator in \eqn{\beta_DD} is the average rate of return of the 
#'instrument over time periods corresponding to the \eqn{(1-\alpha)T} largest
#'drawdowns of the optimal portfolio, where \eqn{w_t - w_k^{\asterisk}(t)} 
#'is the cumulative rate of return of the instrument from the optimal portfolio#' peak time \eqn{k^\asterisk(t)} to time t.
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
#'@references
#'Zabarankin, M., Pavlikov, K., and S. Uryasev. Capital Asset Pricing Model 
#'(CAPM) with Drawdown Measure.Research Report 2012-9, ISE Dept., University 
#'of Florida,September 2012.
#'
#'@examples
#'BetaDrawdown(edhec[,1],edhec[,2]) 

BetaDrawdown<-function(R,Rm,h=0,p=0.95,weights=NULL,geometric=TRUE,type=c("alpha","average","max"),...){

    # DESCRIPTION:
    #
    # The function is used to find the Drawdown Beta.
    # 
    # INPUT:
    # The Return series of the portfolio and the optimal portfolio
    # is taken as the input.
    #
    # OUTPUT:
    # The Drawdown beta is given as the output.   


    x = checkData(R)
    xm = checkData(Rm)
    columnnames = colnames(R)
    columns = ncol(R)
    drawdowns_m = Drawdowns(Rm)
    type = type[1]
    if(type=="average"){
        p = 0
    }
    if(type == "max"){
        p = 1
    }
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
    DDbeta<-function(x){
        q = NULL
        q_quantile = quantile(drawdowns_m,1-p)
        print(drawdowns_m)
        for(i in 1:nrow(Rm)){

            if(drawdowns_m[i]<q_quantile){
              q = c(q,1/((1-p)*nrow(x)))
            }
            else q=c(q,0)
        }
        boolean = (cummax(cumul_xm)==cumul_xm)
        index = NULL
        for(j in 1:nrow(Rm)){
            if(boolean[j] == TRUE){
                index = c(index,j)
                b = j
            }
            else{
                index = c(index,b)
            }
        }
        beta_dd = sum((as.numeric(x[index])-x)*q)/CDaR(Rm,p=p)
        print((as.numeric(x[index])-x)*q)
        return(beta_dd)
    }

    for(column in 1:columns){
        column.beta = DDbeta(cumul_x[,column])
        if(column == 1){
            beta = column.beta
        }
        else{ 
            beta = cbind(beta,column.beta)
        }
    }
    
    if(columns==1){
        return(beta)
    }
    colnames(beta) = columnnames
    rownames(beta) = paste("Drawdown Beta (p =",p*100,"%)")
    beta = reclass(beta,R)
    return(beta)

}


