BenchmarkSRPlots<-function(R=NULL,ylab = NULL,xlab = NULL,lwd = 2,pch = 1,cex = 1,avgSR = NULL,columns = NULL,...){
  
  
  if(!is.null(R)){
    x = checkData(R)
    columns = ncol(x)
    avgSR = mean(SharpeRatio(R))
  }
  else{
    if(is.null(avgSR) | is.null(S)){
      stop("The average SR and the number of strategies should not be NULL")
    }
    
  }
  corr = table.Correlation(edhec,edhec)
  corr_avg = 0
  for(i in 1:(columns-1)){
    for(j in (i+1):columns){
      corr_avg = corr_avg + corr[(i-1)*columns+j,]
    }
  }
  corr_avg = corr_avg*2/(columns*(columns-1))
  
  rho = seq(0,1,length.out=30)
  SR_B = avgSR*sqrt(columns/(1+(columns-1)*rho))
  df1<-data.frame(x=rho,y=SR_B)
  df1$model<-"A"
  df2<-data.frame(x=corr_avg[1,1],y=BenchmanrkSR(R))
  df2$model<-"B"
  dfc<-rbind(df1,df2)
  ggplot(dfc,aes(x,y,group=model)) +geom_point()+geom_line()+xlab("Correlation")+ylab("Benchmark Sharpe Ratio")+ggtitle("Benchmark SR vs Correlation")
}
