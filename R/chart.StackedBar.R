`chart.StackedBar` <- 
function (w, colorset = NULL, main = NULL, space = .2, legend.cex = 0.8, las=3, legend.loc="under", cex.names = 1, beside = FALSE, ylim = NULL, horiz=FALSE, element.color = "darkgray", unstacked = F, ... ) 
{

    # @todo: Set axis color to element.color
    # @todo: Set border color to element.color

    w = checkData(w,method="matrix")
    w.columns = ncol(w)
    w.rows = nrow(w)

    if(is.null(colorset))
        colorset=1:w.columns

    if(unstacked & dim(w)[1] == 1){ # only one row is being passed into 'w', so we'll unstack the bars
        if(las > 1) {# set the bottom border to accomodate labels
            # mar: a numerical vector of the form c(bottom, left, top, right) which
            # gives the number of lines of margin to be specified on the four sides
            # of the plot. The default is c(5, 4, 4, 2) + 0.1

            # Kludgy, but it works.  Converts the length of the string into "lines" used by par("mar")
            # by dividing the length of the string in inches by what the plot uses as the height of 
            # a character by default, par("cin").  That gives a value that seems to work in most cases.
            # When that value is just plain too small, this calc provides a minimum value of "5", or
            # the usual default for the bottom margin.
            bottommargin = max(c(5, strwidth(colnames(weights),units="in")/par("cin")[1]) * cex.names)
            par(mar = c(bottommargin, 4, 4, 2) +.1)
        }
# par(mai=c(max(strwidth(colnames(w), units="in")), 0.82, 0.82, 0.42)*cex.names)D
        barplot(w, main = main, col = colorset[1], ylim = ylim, las = las, horiz = FALSE, beside=FALSE, cex.names = cex.names, space = space, ...)
    }

    else { # multiple columns being passed into 'w', so we'll stack the bars and put a legend underneith

        if(!is.null(legend.loc) ){
            if(legend.loc =="under") # put the legend under the chart
                layout(rbind(1,2), height=c(6,1), width=1)
            else
                par(mar=c(5,4,4,2)+.1) # @todo: this area may be used for other locations later
        }

        if(las > 1) {# set the bottom margin to accomodate names
            # See note above.
            bottommargin = max(c(5,strwidth(rownames(weights),units="in")/par("cin")[1] * cex.names))
            par(mar = c(bottommargin, 4, 4, 2) +.1)

            }
        else
            par(mar=c(5,4,4,2) +.1)

        barplot(t(w),col=colorset,space=space, main=main, las = las, cex.names = cex.names, beside = beside, ylim = ylim, ...)

        if(!is.null(legend.loc)){
            if(legend.loc =="under"){ # draw the legend under the chart
                par(mar=c(0,2,0,1)+.1) # set the margins of the second panel
                plot.new()
                if(w.columns <4)
                    ncol= w.columns
                else
                    ncol = 4
                legend("center", legend=colnames(w), cex = legend.cex, fill=colorset, ncol=ncol, box.col=element.color)
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
# $Id: chart.StackedBar.R,v 1.6 2008-07-08 03:56:34 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.5  2008-04-18 03:56:15  peter
# - added a legend at the bottom
# - added smarts for displaying single column or stacked
# - made bottom margin sensitive to length of label names
#
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