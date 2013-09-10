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
#' 
#' @author Pulkit Mehrotra
#' @seealso \code{\link{chart.Penance}} \code{\link{MaxDD}} \code{\link{TuW}}
#' @references Bailey, David H. and Lopez de Prado, Marcos, Drawdown-Based Stop-Outs and the "Triple Penance" Rule(January 1, 2013).
#' @export

table.Penance<-function(R,confidence=0.95){
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
    column_MinQ <- c(column_MinQ,get_minq(x[,column],confidence))
    column_TuW = get_TuW(x[,column],confidence)
    v = c(column_MinQ,column_TuW,(column_TuW/column_MinQ[5])-1)
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
