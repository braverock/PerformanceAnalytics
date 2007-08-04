`download.SP500PriceReturns` <-
function(start = "1998-01-01", end = NULL, compression = c("m","d"), method = c("compound","simple"))
{ # @ author Peter Carl

    #  Download returns of the "market" asset.

    # Required inputs are just start and stop dates.
    # Outputs a table of log returns
    # @todo: Offer a menu of "market" assets

    # FUNCTION:

    # Download the "market" returns, in this case, the S&P 500 INDEX (^GSPC)
    # from finance.yahoo.com

    # Method can be either "simple" or "compound", default is "compound"
    method = method[1]
    compression = compression[1]

    x = get.hist.quote("^gspc", start = start, end = end, quote = "Close", compression = compression)
    # @todo: make the data type returned selectable (e.g., df, ts, zoo, matrix)

    # Yahoo returns yield data as price series, so we need to calculate returns
    market.returns = Return.calculate(x, method = method)

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
# $Id: download.SP500PriceReturns.R,v 1.9 2007-08-04 15:15:53 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.8  2007/04/14 13:55:46  brian
# - standardize enumerated arguments,
# - assign default value to a string string if no value passed in to avoid warnings
#
# Revision 1.7  2007/03/20 14:31:05  brian
# - restored CVS revision log data
#
# Revision 1.6  2007/03/16 14:03:42  peter
# - added cvs footer
#
# Revision 1.5 2007-03-04 08:59 brian
# - minor changes to pass R CMD check
#
# Revision 1.4 2007-02-27 15:38 brian
# - change compression param to an enumerated type
#
# Revision 1.3 2007-02-27 10:33 peter
# - added compression as attribute
#
# Revision 1.2 2007-02-27 10:01  peter
# - modifications to return calculation function
#
# Revision 1.1 2007-02-27 09:19 peter
# - added function wrapper for downloading and calculating S&P500 price returns
###############################################################################