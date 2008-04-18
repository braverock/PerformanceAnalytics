`chart.StackedBar` <- 
function (w, colorset = NULL, main = NULL, space = .2, legend.cex = 0.7, las=3, legend.loc="under", cex.names = 1, beside = FALSE, ylim = NULL, horiz=FALSE, element.color = "darkgray", unstacked = F, ... ) 
{

    # @todo: Set axis color to element.color
    # @todo: Set border color to element.color

    w = checkData(w,method="matrix")
    w.columns = ncol(w)
    w.rows = nrow(w)

    if(is.null(colorset))
        colorset=1:nrow(w)

    if(unstacked & dim(w)[2] == 1){ # only one column is being passed into 'w', so we'll unstack the bars
        if(las > 1) # set the bottom border to accomodate labels
            # mar: a numerical vector of the form c(bottom, left, top, right) which
            # gives the number of lines of margin to be specified on the four sides
            # of the plot. The default is c(5, 4, 4, 2) + 0.1
            par(mar=c(max(stringDims(rownames(w))$width)/2, 4, 4, 2)*cex.names+.1) #requires Hmisc
        barplot(t(w), main = main, col = colorset[1], ylim = ylim, las = las, horiz = FALSE, beside=FALSE, cex.names = cex.names, space = space, ...)
    }

    else { # multiple columns being passed into 'w', so we'll stack the bars and put a legend underneith

        if(!is.null(legend.loc) ){
            if(legend.loc =="under") # put the legend under the chart
                layout(rbind(1,2), height=c(6,1), width=1)
            else
                legend.tmp = NULL # @todo: this area may be used for other locations later
        }

        if(las > 1) # set the bottom margin to accomodate names
            par(mar=c(max(stringDims(colnames(w))$width)/2, 4, 4, 2)*cex.names +.1)
        else
            par(mar=c(1,4,4,2)+.1)

        barplot(w,col=colorset,space=space, main=main, las = las, cex.names = cex.names, beside = beside, ylim = ylim, ...)

        if(!is.null(legend.loc)){
            if(legend.loc =="under"){ # draw the legend under the chart
                par(mar=c(0,2,0,1)+.1) # set the margins of the second panel
                plot.new()
                if(w.rows <4)
                    ncol= w.rows
                else
                    ncol = 4
                legend("center", legend=rownames(w), cex = legend.cex, fill=colorset, ncol=ncol, box.col=element.color)
            } # if legend.loc is null, then do nothing
        }
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
# $Id: chart.StackedBar.R,v 1.5 2008-04-18 03:56:15 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.4  2008/02/27 04:01:10  peter
# - added cex.names for sizing xaxis tags
#
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