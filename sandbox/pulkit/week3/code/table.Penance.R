#' @title
#' Table for displaying the Mximum Drawdown and the Time under Water
#'
#' @description
#' \code{table.Penance} Displays the table showing mean,Standard Deviation , phi, sigma , MaxDD,time at which MaxDD occurs, MaxTuW and the penance.
#'  
#' @param R Returns
#' @param confidence the confidence interval
#' 
#' @reference Bailey, David H. and Lopez de Prado, Marcos, Drawdown-Based Stop-Outs and the ‘Triple Penance’ Rule(January 1, 2013).

table.Penance<-function(R,confidence){
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
    tp = data.frame()
    for(i in 1:columns){
	phi = cov(x[,i][-1],x[,i][-length(x[,i])])/(cov(x[,i][-length(x[,i])]))
	sigma_infinity = StdDev(x[,i])
	sigma = sigma_infinity*((1-phi^2)^0.5)
	column_MinQ<-c(mean(x[,i]),sigma_infinity,phi,sigma)
        column_MinQ <- c(column_MinQ,get_minq(x[,i],confidence))
        column_TuW = get_TuW(x[,i],confidence)
        tp <- rbind(tp,c(column_MinQ,column_TuW,column_MinQ[5]/column_TuW))
    }
  row.names(tp)<-colnames(R)
  colnames(tp) = c("mean","stdDev","phi","sigma","MaxDD(in %)","t*","MaxTuW","Penance")
  print(tp)
  
}
