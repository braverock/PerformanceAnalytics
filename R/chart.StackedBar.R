    # mar: a numerical vector of the form c(bottom, left, top, right) which
    # gives the number of lines of margin to be specified on the four sides
    # of the plot. The default is c(5, 4, 4, 2) + 0.1

    # The first row is the cumulative returns line plot
# x = resultmatrix.byobjfun[["SR.modVaR.inception"]]
# layout(rbind(1,2), height=c(3,1), width=1)
# par(mar=c(1,4,4,2))
# barplot(x,col=gray.colors(11,start=0,end=1),space=0, main="SR modVaR From Inception Weights")
# par(mar=c(2,4,2,2))
# plot.new()
# legend("center",legend=colnames(x),fill=gray.colors(11,start=0,end=1),cex=.7,ncol=3, box.col="black")
# gray.colors(w.columns,start=0,end=1)
`chart.StackedBar` <- 
function (w, colorset = NULL, main = NULL, space = 0, legend.cex = 0.7, cex = 1, las=3, legend.loc="under", cex.names = 1, ... ) 
{

    w = checkData(w,method="matrix")
    w.columns = ncol(w)
    w.rows = nrow(w)

    if(is.null(colorset))
        colorset=1:nrow(w)

    if(!is.null(legend.loc)){
        if(legend.loc == "under"){
            layout(rbind(1,2), height=c(5,1), width=1)
            if(las > 1)
                par(mar=c(max(stringDims(colnames(w))$width)/2, 4, 4, 2)*cex.names +.1, cex = cex)
            else
                par(mar=c(1,4,4,2)+.1)
            legend.tmp = NULL
        }
        else
            legend.tmp = legend.loc
    }
    else
        legend.tmp = NULL



    barplot(w,col=colorset,space=space, main=main, legend.loc = legend.tmp, las = las, cex.names = cex.names, ...)

    if(!is.null(legend.loc) & legend.loc =="under"){
        par(mar=c(2,2,1,1)+.1)
        plot.new()
        if(w.rows <4)
            ncol= w.rows
        else
            ncol = 4
        legend("center",legend=rownames(w),fill=colorset,cex=legend.cex,ncol=ncol, box.col="black")
    }
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: chart.StackedBar.R,v 1.4 2008-02-27 04:01:10 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.3  2008/02/26 04:56:59  peter
# - fixed label calculation to handle correct dimension
#
# Revision 1.2  2008/02/26 04:38:53  peter
# - now handles multiple columns for fund
# - legend "under" draws correctly
# - bottom margin fits to text with cex=1
#
# Revision 1.1  2008/02/23 05:54:37  peter
# - primitive for weight displays and other charts
#
###############################################################################