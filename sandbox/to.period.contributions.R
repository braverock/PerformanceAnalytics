to.period.contributions <- function(C, period = c("years", "quarters", "months", "weeks"), ...){
  period = period[1] 
  columnnames = colnames(C)
  # @TODO make sure period > frequency of C
  
  # Calculate period return of portfolio from contributions
  pret = rowSums(C)
  pret = xts(pret, order.by=index(C))
  lag.cum.ret <- na.fill(lag(cumprod(1+pret),1),1) 
  wgt.contrib = C * rep(lag.cum.ret, NCOL(C))
  
  # Calculate aggregation periods
  ep = endpoints(C, period)
  dates = index(C)[endpoints(C, period)]

  # Summarize weighted contributions by period
  period.wgt.contrib = apply(wgt.contrib, 2, function (x, ep) period.apply(x, INDEX=ep, FUN=sum), ep=ep)
  period.wgt.contrib = as.xts(period.wgt.contrib, order.by=dates)

  # Normalize to the beginning of period value
  period.contrib = NULL
  for(i in 1:length(dates)) {
    if(i==1){
      span = paste0("::", dates[i])
    }else{
      span = paste0(dates[i-1], "::", dates[i])
    }
    period.contrib = rbind(period.contrib, colSums(wgt.contrib[span]/rep(head(lag.cum.ret[span],1),NCOL(wgt.contrib))))
  }
  period.contrib = as.xts(period.contrib, order.by = dates)
  period.contrib = cbind(period.contrib, rowSums(period.contrib))
  colnames(period.contrib) = c(columnnames, "Portfolio Return")
  return(period.contrib)
}