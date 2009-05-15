`chart.Events` <-
function (R, dates, prior=12, post=12, main = NULL, xlab=NULL, ...)
{ # @author Peter Carl
  R = checkData(R[,1,drop=FALSE]) 
  if(is.null(main)) 
    main = paste(colnames(R), "Event Study")
  for(i in 1:length(dates)){
    date = dates[i]
    origin = grep(date,index(R))
    if(length(origin)==0) # test to make sure it found it, throw error if not
        stop("Date not found or dates don't match the index of the data.")
    x1 = matrix(R[(max(0,origin-prior)):origin, ],ncol=1) # get the trailing data
    if(origin-prior<0)
        x1 = rbind(matrix(rep(NA,abs(origin-prior)+1),ncol=1),x1) # pad it if needed
    x2 = matrix(R[(origin+1):min(length(R),origin+post) ],ncol=1) # get the next data
    if(origin+post>length(R))
        x2 = rbind(x2,matrix(rep(NA,((origin+post)-length(R))),ncol=1)) # pad it if needed
    x = rbind(x1,x2) # connect the two pieces

    if(date == dates[1])
      y = x
    else
      y = cbind(y,x) # make a matrix
  }

  colnames(y) = as.character(dates)
  x.xts=xts(y, order.by=time(R)[1:nrow(y)]) # Format for chart.TimeSeries with fake labels
  event.line = format(time(x.xts)[prior+1], "%m/%y")
  if(is.null(xlab))
    xlab = "Periods to Event" #improve 

  chart.TimeSeries(x.xts, xlab = xlab, xaxis.labels = seq(-prior, post, by=1), event.lines = event.line, main = main, ...)

}
  # OTHER NOTES:
  #   chart.AlignDD could use method = c("worst", "start") and use either 
  #   'Trough' or 'From' dates from sortDrawdowns.  post would be set to
  #   max(recovery). prior would be set to max('To Trough') or zero.
  #   Use table.Drawdowns for dates.

#   time(managers) = as.Date(time(managers))
#   time(R) = as.Date(time(R))
#   R=Drawdowns(managers[,2,drop=F])
#   n=table.Drawdowns(managers[,2,drop=F])                          
#   chart.Events(Drawdowns(managers[,2,drop=F]), dates = n$Trough, prior=max(na.omit(n$"To Trough")), post=max(na.omit(n$Recovery)), lwd=2, colorset=redfocus, legend.loc=NULL, main = "Worst Drawdowns")

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: chart.Events.R,v 1.1 2009-05-15 01:40:31 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
