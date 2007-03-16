`download.SP500PriceReturns` <-
function(start = "1998-01-01", end = NULL, compression = "m", method = "compound")
{ # @ author Peter Carl

    #  Download returns of the "market" asset.

    # Required inputs are just start and stop dates.
    # Outputs a table of log returns

    # FUNCTION:

    # Download the "market" returns, in this case, the S&P 500 INDEX (^GSPC)
    # from finance.yahoo.com

    # Method can be either "simple" or "compound", default is "compound"
    # @todo: Offer a menu of "market" assets

    x = get.hist.quote("^gspc", start = start, end = end, quote = "Close", compression = compression)
    # @todo: make the data type returned selectable (e.g., df, ts, zoo, matrix)

    # Yahoo returns yield data as price series, so we need to calculate returns
    market.returns = CalculateReturns(x, method = method)

    colnames(market.returns)="SP500"
    market.returns
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: download.SP500PriceReturns.R,v 1.6 2007-03-16 14:03:42 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
#
###############################################################################
