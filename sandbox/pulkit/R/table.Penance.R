#' @title
#' Table for displaying the Mximum Drawdown and the Time under Water
#'
#' @description
#' \code{table.Penance} Displays the table showing mean,Standard Deviation , phi, sigma , MaxDD,time at which MaxDD occurs, MaxTuW and the penance.For more 
#' details about MaxDD , Time under Water see code \code{MaxDD} and \code{TuW}
#' respoectively. 
#'  
#' @param R Returns
#' @param confidence the confidence interval
#' @param type The type of distribution "normal" or "ar"."ar" stands for Autoregressive.
#' @author Pulkit Mehrotra
#' @seealso \code{\link{chart.Penance}} \code{\link{MaxDD}} \code{\link{TuW}}
#' @references Bailey, David H. and Lopez de Prado, Marcos, Drawdown-Based Stop-Outs and the "Triple Penance" Rule(January 1, 2013).
#' @export

table.Penance<-function(R,confidence=0.95,type=c("ar","norm")){
  # DESCRIPTION:
  # Maximum Drawdown and Time under Water considering first-order serial correlation
  # 
  # Input:
  # R log returns 
  # 
  # Output:
  # Creates a Table showing mean stdDev phi sigma MaxDD t* MaxTuW and Penance
  #
  # Function:
  x = checkData(R)
  columns = ncol(x) 
  columnnames = colnames(x)
  rownames = c("mean","stdDev","phi","sigma","MaxDD(in %)","t*","MaxTuW","Penance")
  for(column in 1:columns){
    col_val = na.omit(x[,column])
    phi = cov(col_val[-1],col_val[-length(col_val)])/(cov(col_val[-length(col_val)]))
    sigma_infinity = StdDev(col_val)
    sigma = sigma_infinity*((1-phi^2)^0.5)
    column_MinQ<-c(mean(col_val),sigma_infinity,phi,sigma)
    column_MinQ <- c(column_MinQ,MaxDD(x[,column],confidence,type=type[1]))
    column_TuW = TuW(x[,column],confidence,type=type[1])
    v = c(column_MinQ,column_TuW,Penance(x[,column],confidence,type=type[1]))
    v = round(v,4)
    if(column == 1){
      result = data.frame(Value = v, row.names = rownames)
    }
    else{
      nextcolumn = data.frame(Value = v,row.names = rownames)
      result = cbind(result,nextcolumn)
    }
  }
  colnames(result) = columnnames
  result
}
