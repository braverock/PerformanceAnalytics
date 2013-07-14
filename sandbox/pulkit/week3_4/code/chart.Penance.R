#'@title
#'Penance vs phi plot
#'
#'A plot for Penance vs phi for the given portfolio
#'
#'@param R an xts, vector, matrix, data frame,
#'timeSeries or zoo object of asset returns.
#'@param confidence the confidence level
#'
#'@reference Bailey, David H. and Lopez de Prado, Marcos,Drawdown-Based Stop-Outs and the ‘Triple Penance’ Rule(January 1, 2013).

chart.Penance<-function(R,confidence,...){
    x = checkData(R)
    columns = ncol(x)
    columnnames = colnames(x)
    phi = 1:columns
    penance = 1:columns
    for(column in 1:columns){
        phi[column] = cov(x[,column][-1],x[,column][-length(x[,column])])/(cov(x[,column][-length(x[,column])]))
        penance[column]<-get_minq(x[,column],confidence)[1]/get_TuW(x[,column],confidence)
    }
    plot(x=phi,y=penance,xlab="Phi",ylab = "Penance",main="Penance vs Phi")
    text(phi,penance,columnnames,pos = 4)
}





