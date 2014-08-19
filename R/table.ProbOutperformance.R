#' Calculates Count of trailing periods where a fund outperformed its benchmark and calculates the proportion of those periods, this is commonly used in marketing as the probability of outperformance on a N year basis
#' 
#'  
#' @param R an xts, timeSeries or zoo object of asset returns
#' @param Rb an xts, timeSeries or zoo object of the benchmark returns
#' @param period_lengths a vector of periods the user wants to evaluate this over i.e. c(1,3,6,9,12,18,36)
#' @author Kyle Balkissoon
#' @keywords Performance Reporting Fund vs Benchmark
#' 
#' @export table_ProbOutperformance

table.ProbOutPerformance = function(R,Rb,period_lengths=c(1,3,6,9,12,18,36)){
  if(nrow(R)!=nrow(Rb)){
    stop("R and Rb must be the same length")
  }
  
  
  ###Create Trailing frequency analysis
  R_periods = xts(data.frame(matrix(ncol=length(period_lengths),nrow=nrow(R))),order.by=index(R))
  colnames(R_periods) = paste0("period_",period_lengths)
  Rb_periods = R_periods
  for(i in 1:nrow(R_periods)){
    for(p_len in period_lengths){
      #if there aren't enough occurences yet don't calculate anything
      if(p_len>i){}else{
      tdf = first(R,i)
      tdf_b = first(Rb,i)
    eval(parse(text=paste0("R_periods[",i,",]$period_",p_len," = Return.cumulative(last(tdf,",p_len,"))")))
    eval(parse(text=paste0("Rb_periods[",i,",]$period_",p_len," = Return.cumulative(last(tdf_b,",p_len,"))")))
    }}}
    
    
  ##Calculate periods ahead 
  #Differences
  diff_mat = R_periods-Rb_periods
  
  ##Result
  result = data.frame(period_lengths)
  result[,2] = NA
  result[,3]=NA
    for(p_len in 1:length(period_lengths)){
      result[p_len,2] = eval(parse(text=paste0("sum(ifelse(as.numeric(diff_mat$period_",period_lengths[p_len],")>0,1,0),na.rm=T)")))
      result[p_len,3] = eval(parse(text=paste0("sum(ifelse(as.numeric(diff_mat$period_",period_lengths[p_len],")<0,1,0),na.rm=T)")))   
          }
  result[,4] = result[,2]+result[,3]
  result[,5] = result[,2]/result[,4]
  result[,6] = result[,3]/result[,4]
  
  colnames(result) = c("period_lengths",colnames(R),colnames(Rb),"total periods",paste0("prob_",colnames(R),"_outperformance"),paste0("prob_",colnames(Rb),"_outperformance"))
  return(result)
  
}