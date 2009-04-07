`chart.Boxplot` <-
function (R, horizontal = TRUE, names = TRUE, as.Tufte = FALSE, sort.by = c(NULL, "mean", "median", "variance"), colorset = "black", symbol.color = "red", mean.symbol = 1, median.symbol = "|", outlier.symbol = 1, show.data = FALSE, add.mean = TRUE, sort.ascending = FALSE, xlab="Return", main = "Return Distribution Comparison", element.color = "darkgray", ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # A wrapper to create box and whiskers plot, but with some sensible defaults
    # useful for comparing distributions.

    # mar: a numerical vector of the form c(bottom, left, top, right) which
    # gives the number of lines of margin to be specified on the four sides
    # of the plot. The default is c(5, 4, 4, 2) + 0.1

    R = checkData(R, method="data.frame")

    columns = ncol(R)
    rows = nrow(R)
    columnnames = colnames(R)

    column.order = NULL

    sort.by = sort.by[1]

    #op <- par(no.readonly=TRUE)

    if(names){
        par(mar=c(5,12,4,2) + 0.1)
    }

    if(length(colorset) < columns)
        colorset = rep(colorset, length.out = columns)

    if(length(symbol.color) < columns)
        symbol.color = rep(symbol.color, length.out = columns)

    if(length(mean.symbol) < columns)
        mean.symbol = rep(mean.symbol, length.out = columns)

    means = sapply(R, mean, na.rm = TRUE)

    switch(sort.by,
        mean = {
            column.order = order(means)
            ylab = paste("Sorted by Mean", sep="")
        },
        median = {
            medians = sapply(R, median, na.rm = TRUE)
            column.order = order(medians)
            ylab = paste("Sorted by Median", sep="")
        },
        variance = {
            variances = sapply(R, var, na.rm = TRUE)
            column.order = order(variances)
            ylab = paste("Sorted by Variance", sep="")
        },
        {
            column.order = 1:columns
            ylab = paste("Unsorted", sep="")
        }
    ) # end switch

    if(as.Tufte){
        boxplot(R[,column.order], horizontal = TRUE, names = names, main = main, xlab = xlab, ylab = "", pars = list(boxcol = "white", medlty = "blank", medpch = median.symbol, medlwd = 2, medcex = .8, medcol = colorset[column.order], whisklty = c(1,1), whiskcol = colorset[column.order], staplelty = "blank", outpch = outlier.symbol, outcex = .5, outcol = colorset[column.order] ), axes = FALSE, ...)
    }
    else{
        boxplot(R[,column.order], horizontal = TRUE, names = names, main = main, xlab = xlab, ylab = "", pars = list(boxcol = colorset[column.order], medlwd = 1, medcol = colorset[column.order], whisklty = c(1,1), whiskcol = colorset[column.order], staplelty = 1, staplecol = colorset[column.order], staplecex = .5, outpch = outlier.symbol, outcex = .5, outcol = colorset[column.order] ), axes = FALSE, boxwex=.6, ...)
    } # end else

    if(add.mean)
        points(means[column.order], 1:columns, pch = mean.symbol[column.order], col=symbol.color[column.order])

    if(names){
        labels = columnnames
        axis(2, cex.axis = 0.8, col = element.color, labels = labels[column.order], at = 1:columns, las = 1)
    }
    else{
        labels = ""
        axis(2, cex.axis = 0.8, col = element.color, labels = labels[column.order], at = 1:columns, las = 1, tick = FALSE)
    }
    axis(1, cex.axis = 0.8, col = element.color)
    

#     if(names)
#         title(sub=ylab)
#     else
#         title(sub=ylab)
    box(col=element.color)

    abline(v=0, lty="solid",col=element.color)

    #par(op)
}


# dottypes = c(rep(16, length(manager.columns)), closedsymbols[1:length(index.columns)], rep(1, length(peer.columns)))

# > chart.Boxplot(sbc.zoo,names=F,as.Tufte=T,add.mean = TRUE, sort.by="variance", colorset=colorset,symbol.color=colorset, mean.symbol=dottypes)


###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2008 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: chart.Boxplot.R,v 1.6 2009-04-07 22:18:25 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.5  2008-06-02 16:05:19  brian
# - update copyright to 2004-2008
#
# Revision 1.4  2007/09/26 02:56:58  peter
# - changed zero line to solid
# - removed subtitles
#
# Revision 1.2  2007/02/07 13:24:49  brian
# - fix pervasive comment typo
#
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################