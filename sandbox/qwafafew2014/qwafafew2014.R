# R Code from the presentation to the Chicago Chapter of QWAFAFEW
# Titled "Bridging Tail Risk and Drawdowns"
# Presented November 13, 2014
# By Peter Carl
#  ------------------------------------------------------------------------
# This code was extracted from the Rmarkdown slides qwafafew2014.Rpres using
# > knitr:::purl('qwafafew2014.Rpres')
#
# Individual functions will be extracted and modified from this script to be 
# included in a future release of the  PerformanceAnalytics package.  
# Development code for that package is managed at R-Forge and may be 
# installed with:
# > install.packages("PerformanceAnalytics", repos="http://R-Forge.R-project.org")

## ----setup, echo=FALSE, cache=FALSE--------------------------------------
# Load needed packages
library(pander)
# require(Cairo)
library(PerformanceAnalytics)
require(foreach) # for mbb
require(doMC)
registerDoMC(1)

# Only available on R-Forge
#library("noniid.sm", lib.loc="~/R/library")
#library("noniid.pm", lib.loc="~/R/library")

# load some local functions
source('./R/mbb.R')
source('./R/table.RiskStats.R')
source('./R/Baily_LopezdePrado_stop-out.R')

# Rpres options
# panderOptions('table.split.cells', Inf) 
panderOptions('table.style', 'rmarkdown')

# Prep data and graphics
op <- par(no.readonly = TRUE)
x.R = Return.read('./data/Futures Trend 201409a.csv')
manager.col=18 # Switch to 11?
peer.cols = c(1:17,19:22)
index.cols = c(23, 25)
start.date="1999-05-31::"
colorset = c(rep('red', length(manager.col)), 
  rep('darkorange', length(index.cols)), 
  rep("gray50", length(peer.cols)))
lineset = c(1, 1, 2,  rep(1, length(peer.cols)))
pchset = c(16, 16, 15,  rep(16, length(peer.cols)))
lwdset = c(4, 2, 1,  rep(1, length(peer.cols)))
ddt = table.Drawdowns(x.R[,manager.col], top=1)
period.areas = paste(ddt$From, ddt$To, sep="::")
period.color="gray90"

# Not used:
# CairoFonts(
#   regular="Cambria:style=Regular",
#   bold="Cambria:style=Bold",
#   italic="Cambria:style=Italic",
#   bolditalic="Cambria:style=Bold Italic,BoldItalic",
#   symbol="Symbol"
# )


## ----cumreturns, echo=FALSE, cache=TRUE, fig.height=8, fig.width=12------
par(mar = c(3, 5, 4, 3)+0.1) #c(bottom, left, top, right)
chart.CumReturns(x.R[,c(manager.col, index.cols, peer.cols)], colorset=colorset, legend.loc=NULL, lty=lineset, lwd=lwdset, ylog=TRUE, wealth.index=TRUE, main="Cumulative Returns", cex.axis=1.2, cex.lab=1.5, cex.main=2)
par(op)


## ----distribution, echo=FALSE, cache=TRUE, fig.height=8, fig.width=12----
# @TODO add gaussian VaR and ETL lines to hist using note.lines and note.labels
# c(bottom, left, top, right)
par(oma = c(5,3,2,1), mar=c(0,1.5,0,1.5))
layout(matrix(1:2, ncol=2, byrow=TRUE))
chart.Histogram(x.R[,manager.col], main="", show.outliers=TRUE, methods=c("add.normal", "add.risk"), colorset = c("gray50", "#00008F", "red",
  "#23FFDC", "#ECFF13", "#FF4A00", "#800000"), cex.axis=1.2)
abline(v=0, col="darkgray", lty=2)
chart.QQPlot(x.R[,manager.col], main="", pch=16, envelope=0.95, cex=2, col = c("gray40", "red", "gray50",
  "#23FFDC", "#ECFF13", "#FF4A00", "#800000"), cex.axis=1.2)
abline(v=0, col="darkgray", lty=2)
par(op)


## ----tailsTable1, echo=FALSE, results='asis', fig.align="center"---------
# @TODO How to format numbers in table?
# @TODO Highlight certain numbers?

table = table.RiskStats(x.R[,manager.col], p=1-1/12)
table1= table[1:8,,drop=FALSE]
# mat1=matrix(c(rep(1,8),rep(4,8)),ncol=2)
cat("<table style=\"border: solid 1px #ffffff; border-collapse: collapse;\" border=\"0px\" cellspacing=\"0px\" cellpadding=\"0px\" width=\"100%\" align=\"center\"><tr>")
cat("<td style=\"border:1px solid #ffffff \">")
kable(table1, format="html")
#print(xtable(table1, digits=mat1),type='html')
cat("</td>")
cat("<td style=\"border:1px solid #ffffff \">")
table2= table[9:15,,drop=FALSE]
# mat2=matrix(c(rep(1,7),rep(4,7)),ncol=2)
kable(table2, format="html")
#print(xtable(table2, digits=mat2),type='html')
cat("</td>")
cat("</tr></table>")


## ----BarVaR, echo=FALSE, cache=TRUE, fig.height=8, fig.width=12----------
par(mar = c(3, 5, 2, 3)+0.1) #c(bottom, left, top, right)
chart.BarVaR(na.omit(x.R[,manager.col]), colorset=colorset, lty=c(1,2), lwd=4, main="", methods=c("ModifiedES", "HistoricalES"),  p=1-1/12, show.greenredbars = FALSE, legend.loc = "topleft", cex.axis=1.2, cex.lab=1.5, legend.cex=1.5, ylab="Monthly Return", period.areas=period.areas, period.color=period.color) 
par(op)


## ----sensitivity, echo=FALSE, cache=TRUE, fig.height=8, fig.width=12-----
par(mar = c(5, 5, 2, 3)+0.1) #c(bottom, left, top, right)
chart.VaRSensitivity(x.R[,manager.col], methods=c("ModifiedES","HistoricalES", "GaussianES"), legend.loc="bottomright", clean="boudt", lty=c(2,1,2), ylim=c(-.25,0), lwd=3, ylab="Expected Tail Loss", xlab="p", las=1, main="", cex.axis=1.2, cex.lab=1.5, cex.axis=1.2, cex.lab=1.5, cex.legend=2)
abline(v = 1-1/12, col = "darkblue", lty = 2, lwd=2)
par(op)


## ----drawdowns, echo=FALSE, cache=TRUE, fig.height=8, fig.width=12-------
par(mar = c(3, 5, 2, 3)+0.1) #c(bottom, left, top, right)
chart.Drawdown(x.R[start.date,c(manager.col, index.cols, peer.cols)], colorset=colorset, lwd=lwdset, legend.loc=NULL, lty=lineset, main="", cex.axis=1.2, cex.lab=1.5, period.areas=period.areas, period.color=period.color)
par(op) 


## ----drawdownsTable, results='asis', echo=FALSE--------------------------
emphasize.rows(1)
set.alignment(rep("center",7))
pandoc.table(table.Drawdowns(x.R[,manager.col]), "Top Five Drawdowns", split.tables=Inf, justify='centre')


## ----meansDD, echo=FALSE, cache=TRUE, fig.height=8, fig.width=12---------
x.AR=Return.annualized(x.R[start.date,c(manager.col, index.cols, peer.cols)])
x.MDD=maxDrawdown(x.R[start.date,c(manager.col, index.cols, peer.cols)])
par(mar = c(5, 5, 2, 3)+0.1) #c(bottom, left, top, right)
plot(x.MDD~x.AR, col=colorset, pch=pchset, xlab= "Annualized Return", ylab = "Max Drawdown", cex=2, cex.axis=1.2, cex.lab=1.5)
abline(v=mean(x.AR), col='darkgray', lty=2, lwd=2)
abline(h=median(x.MDD), col='darkgray', lty=2, lwd=2)
par(op)


## ----volDD, echo=FALSE, cache=TRUE, fig.height=8, fig.width=12-----------
x.SD=sd.annualized(x.R[start.date,c(manager.col, index.cols, peer.cols)])
x.MDD=maxDrawdown(x.R[start.date,c(manager.col, index.cols, peer.cols)])
par(mar = c(5, 5, 2, 3)+0.1) #c(bottom, left, top, right)
plot(x.MDD~x.SD, col=colorset, pch=pchset, xlab= "Annualized Std Dev", ylab = "Max Drawdown", cex=2, cex.axis=1.2, cex.lab=1.5)
abline(v=mean(x.SD), col='darkgray', lty=2, lwd=2)
abline(h=median(x.MDD), col='darkgray', lty=2, lwd=2)
par(op)


## ----NormDD, echo=FALSE, cache=TRUE, fig.height=8, fig.width=12----------
targetVol=0.15/sqrt(12) # de-"Annualized"
targetMean=0.05/12 # de-"Annualized"

x.Mean=apply(x.R, MARGIN=2, FUN="mean", na.rm = TRUE)
x.SD=StdDev(x.R)

# Apply z-score
x.Z = apply(x.R, MARGIN=2, FUN=function(x){ (x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE) })
# x.Z now has mean=0, sd=1
x.N= targetMean + x.Z * (rep(1, nrow(x.R)) %o% rep(targetVol,NCOL(x.R)))
x.N = as.xts(x.N, by=index(x.R))
x.NDD = PerformanceAnalytics:::Drawdowns(x.N)
par(mar = c(3, 5, 2, 3)+0.1) #c(bottom, left, top, right)
chart.TimeSeries(x.NDD[start.date,c(manager.col, index.cols, peer.cols)], colorset=colorset, lwd=lwdset, legend.loc=NULL, lty=lineset, main="", cex.axis=1.2, cex.lab=1.5)
par(op)


## ----AC, echo=FALSE, cache=TRUE, fig.height=8, fig.width=12--------------
x.ac <- acf(na.omit(x.R[,manager.col]), plot=FALSE)
par(mar = c(4, 5, 2, 3)+0.1) #c(bottom, left, top, right)
bp<-barplot(x.ac$acf[2:22], ylim=range(pretty(x.ac$acf[2:22])), bordre=NA, col="grey40", main="Autocorrelation Since Inception", xlab="Lag", ylab="acf", cex.axis=1.2, cex.lab=1.5, cex.main=2)
axis(1, labels = x.ac$lag[2:22], at=bp)
abline(h=0)
ci=0.95
clim = qnorm((1 + ci)/2)/sqrt(x.ac$n.used)
abline(h = c(clim, -clim), col = "blue", lty = 2, lwd=2)
box()
par(op)


## ----rankAC, echo=FALSE, cache=TRUE, fig.height=8, fig.width=12----------
# trailing 36 months
t.AC = table.Autocorrelation(last(x.R[,c(manager.col, index.cols, peer.cols)],36))
y=colSums(t.AC[1:6,])
layout(matrix(1:2,ncol=1), heights=c(3,2))
par(mar = c(1, 5, 3, 3)+0.1) #c(bottom, left, top, right)
barplot(y[order(y)], col=colorset[order(y)], border = NA, axisnames=FALSE, ylim=range(pretty(y)), cex.axis=1.2, cex.lab=1.5, cex.main=2, ylab="Sum of Lag 1-6 AC", main="Trailing 36-month Autocorrelation")
box()
barplot(as.numeric(t.AC[7,order(y)]), col=colorset[order(y)], ylim=range(pretty(c(0,1))), axisnames=FALSE, border=NA, cex.axis=1.2, cex.lab=1.5, ylab="Q(6) p-value")
box()
par(op)


## ----rollAC, echo=FALSE, cache=TRUE, fig.height=8, fig.width=12----------
x.rho = NULL
for(i in colnames(x.R[,c(manager.col, index.cols, peer.cols)])){
  x.rho1 <- rollapply(na.omit(x.R[,i]), width = 36, FUN=function(x) {sum(acf(x, plot = FALSE, lag.max = 6)$acf[2:7])})
  x.rho = cbind(x.rho, x.rho1)
}
par(mar = c(3, 5, 2, 3)+0.1) #c(bottom, left, top, right)
chart.TimeSeries(x.rho["1999::"], colorset=colorset, legend.loc=NULL, lwd=lwdset, lty=lineset, main="36-Month Rolling Sum of Lag 1-6 Autocorrelation", cex.axis=1.2, cex.lab=1.5, period.areas=period.areas, period.color=period.color)
par(op)


## ----, echo=FALSE, cache=TRUE--------------------------------------------
test <- mbb(x.R[,manager.col], N=length(x.R[,manager.col]), k=6, nrep=10000)
test.xts = as.xts(test, order.by = index(x.R[,manager.col])) # make it time series data
# calc MDD
test.mdd <- foreach(i=1:NCOL(test.xts),.combine=c, .inorder=TRUE) %dopar% {x=as.numeric(maxDrawdown(test.xts[,i]))}


## ----mbb10k, echo=FALSE, cache=TRUE, fig.height=8, fig.width=12----------
par(mar = c(5, 5, 4, 3)+0.1) #c(bottom, left, top, right)
chart.Histogram(test.mdd, col="gray", main="Distribution of Maximum Drawdowns", xlim=c(0,1), xlab="Drawdown Losses", breaks=seq(0, 1, by=.02), cex.axis=1.2, cex.lab=1.5, cex.main=2)
q=quantile(test.mdd, 0.5)
abline(v=q, col='black', lty=2, lwd=2)
plot.dims = par("usr") # c(x1, x2, y1, y2)
text(x=q,y=plot.dims[4]*(1-.02), label="50%", offset = .7, pos = 2, cex = 1.5, srt=90, col = "black")
q=quantile(test.mdd, 0.95)
abline(v=q, col='black', lty=2, lwd=2)
text(x=q,y=plot.dims[4]*(1-.02), label="95%", offset = .7, pos = 2, cex = 1.5, srt=90, col = "black")
x.DD = table.Drawdowns(x.R[,manager.col], top=5)
abline(v=-x.DD$Depth, col='red', lwd=1)


## ----NormMaxQL, echo=FALSE, cache=TRUE, fig.height=8, fig.width=12-------
x.maxql = apply(x.R["1998::",c(manager.col, index.cols, peer.cols)], MARGIN=2, FUN=MaxQL, confidence=0.95, type="normal") 
x.MDD = -maxDrawdown(x.R[,c(manager.col, index.cols, peer.cols)])
par(mar = c(3, 5, 4, 3)+0.1) #c(bottom, left, top, right)
df.bar <- barplot(x.maxql[order(x.maxql)], col=colorset[order(x.maxql)], border = NA, axisnames=FALSE, ylim=c(min(pretty(x.MDD)),0), cex.axis=1.2, cex.lab=1.5, ylab="MaxQL 95%", main="Assuming Normally Distributed, iid")
points(y=x.MDD[order(x.maxql)], x=df.bar, pch=23, col="black", bg=colorset[order(x.maxql)], 
       cex=2) # observed
par(op)


## ----CDaR, echo=FALSE, cache=TRUE, fig.height=8, fig.width=12------------
dd = PerformanceAnalytics:::Drawdowns(na.omit(x.R[,manager.col]))
par(mar = c(5, 5, 4, 3)+0.1) #c(bottom, left, top, right)
chart.Histogram(-dd, main="Distribution of Drawdowns", xlim=c(0,1), xlab="Drawdown Losses", breaks =seq(0,1,by=.03), cex.axis=1.2, cex.lab=1.5, cex.main=2)
q=quantile(-dd, p=0.90, type=8)
abline(v=q, col='black', lty=2, lwd=2) 
plot.dims = par("usr") # c(x1, x2, y1, y2)
text(x=q,y=plot.dims[4]*(1-.02), label="90%", offset = .7, pos = 2, cex = 1.5, srt=90, col = "black")

dar = quantile(dd, p=1-0.90, type=8)
cdar = -1*mean(dd[dd<dar])
points(x=cdar, y=plot.dims[4]*(1-.98), pch=16, col="red", cex=2)
text(x=cdar,y=plot.dims[4]*(1-.95), label=paste("CDaR (90%): ", round(cdar*100,1),"%"), offset = 0, pos = 4, cex = 1.5, srt=90, col = "black")
abline(v=-x.DD$Depth, col='red', lwd=1)
par(op) 


## ----rankCDaR, echo=FALSE, cache=TRUE, fig.height=8, fig.width=12, grdevice="cairo"----
# Barplot of CDaR measures by manager
CDaR <- function(R, p=0.95, ...){
  R = checkData(R)
  R = na.omit(R)
  nr = nrow(R)
  dd = coredata(-PerformanceAnalytics:::Drawdowns(R))
  dd = dd[order(dd),increasing = TRUE]
  # result = -(1/((1-p)*nr))*sum(dd[((p)*nr):nr])
  dar = quantile(dd, p=0.90, type=8)
  result = -1*mean(dd[dd>dar])
  result
}
x.CDaR90 = apply(x.R["1998::",c(manager.col, index.cols, peer.cols)], MARGIN = 2, FUN = CDaR, p=0.9)
par(mar = c(3, 5, 4, 3)+0.1) #c(bottom, left, top, right)
barplot(x.CDaR90[order(x.CDaR90)], col=colorset[order(x.CDaR90)], border = NA, axisnames=FALSE, ylim=range(pretty(x.CDaR90)), cex.axis=1.2, cex.lab=1.5, ylab="CDaR 90%")
par(op)


## ----rollMaxDD, echo=FALSE, cache=TRUE, fig.height=8, fig.width=12-------
# Rolling 12-month max DD
x.rMDD = NULL # initialize rolling maximum drawdown results
for(i in colnames(x.R)){
  x.rMDD1 <- rollapply(na.omit(x.R[,i]), width = 12, align="right", FUN=maxDrawdown)
  x.rMDD = cbind(x.rMDD, x.rMDD1)
} 
par(mar = c(3, 5, 4, 2)+0.1) #c(bottom, left, top, right)
chart.TimeSeries(-1*x.rMDD["1998::",c(manager.col, index.cols, peer.cols)], colorset=colorset, legend.loc=NULL, lwd=lwdset, lty=lineset, main="Rolling 12m Max Drawdown", cex.main=2, cex.axis=1.2, cex.lab=1.5, period.areas=period.areas, period.color=period.color)
par(op)


## ----DDrollDD, echo=FALSE, cache=TRUE, fig.height=8, fig.width=12--------
par(mar = c(3, 5, 4, 3)+0.1) #c(bottom, left, top, right)
chart.TimeSeries(cbind(PerformanceAnalytics:::Drawdowns(x.R["1998::", manager.col]), -1*x.rMDD["1998::", manager.col]), colorset=c("gray40", "red"), legend.loc=NULL, lwd=c(3,4), lty=c(1,1), main="Drawdowns and 12-month Rolling Drawdowns", cex.main=2, cex.axis=1.2, cex.lab=1.5, period.areas=period.areas, period.color=period.color) 
par(op)


## ----CED, echo=FALSE, cache=TRUE, fig.height=8, fig.width=12-------------
# Distribution of rolling 12-month max DD's
par(mar = c(5, 5, 4, 3)+0.1) #c(bottom, left, top, right)
chart.Histogram(x.rMDD[,manager.col], col="gray", main="Distribution of Rolling 12-month Maximum Drawdowns", xlim=c(0,1), breaks =seq(0,1,by=.02), cex.axis=1.2, cex.lab=1.5, cex.main=2, xlab="Rolling 12-month Max Drawdown")
q=quantile(x.rMDD[,manager.col], 0.90, na.rm=TRUE)
abline(v=q, col='black', lty=2, lwd=2) # 90% 12m DD 
plot.dims = par("usr") # c(x1, x2, y1, y2)
text(x=q,y=plot.dims[4]*(1-.02), label="90%", offset = .7, pos = 2, cex = 1.5, srt=90, col = "black")

x.qrMDD=apply(x.rMDD["1998::",], MARGIN=2, FUN=quantile, probs=0.90, na.rm=TRUE) # this is the quantile
x.CED = NULL # Calculate the CED from the rolling MDD obs > quantile MDD
for(i in 1:NCOL(x.R)){
  .CED = mean(x.rMDD[x.rMDD[,i] > x.qrMDD[i], i])
  x.CED = c(x.CED, .CED)
}
points(x=x.CED[manager.col], y=plot.dims[4]*(1-.98), pch=16, col="red", cex=2)
text(x=x.CED[manager.col],y=plot.dims[4]*(1-.95), label=paste("CED (90%): ", round(x.CED[manager.col]*100,1),"%"), offset = 0, pos = 4, cex = 1.5, srt=90, col = "black")
abline(v=-x.DD$Depth, col='red', lwd=1)
par(op)


## ----rankCED, echo=FALSE, cache=TRUE, fig.height=8, fig.width=12---------
# Bar plot of 90% 12m MDD
# @TODO Functionalize the CED calc
x.CED = x.CED[c(manager.col, index.cols, peer.cols)]
par(mar = c(3, 5, 4, 3)+0.1) #c(bottom, left, top, right)
barplot(-1*x.CED[order(-x.CED)], col=colorset[order(-x.CED)], border = NA, axisnames=FALSE, ylim=c(min(pretty(-1*x.CED)),0), cex.axis=1.2, cex.lab=1.5, ylab="90% 12m Max Drawdown", main="Conditional Expected Drawdown by Manager", cex.main=2)
par(op)


