rollingRegression <-
function(formula, data, width, ...)
{# @author Douglas Bates

    # DESCRIPTION:
    # This code was posted to the R-help mailing list in response to a question
    # posted by Ajay Shah.  For the full discussion, see:
    # http://www.mail-archive.com/r-help@stat.math.ethz.ch/msg19544.html

    # @todo: make inputs consistant with other functions

    mCall = match.call()
    mCall$width = NULL
    mCall[[1]] = as.name("lm")
    mCall$x = mCall$y = TRUE # now mCall contains lm(y = TRUE, x = TRUE)
    bigfit = eval(mCall, parent.frame())
    ncoef = length(coef(bigfit))
    nr = nrow(data)
    width = as.integer(width)[1]
    stopifnot(width >= ncoef, width <= nr)
    y = bigfit$y
    x = bigfit$x
    terms = bigfit$terms
    inds = embed(seq(nr), width)[, rev(seq(width))]
    sumrys <- lapply(seq(nrow(inds)),
        function(st) {
            ind = inds[st,]
            fit = lm.fit(x[ind, , drop = FALSE], y[ind])
            fit$terms = terms
            class(fit) = "lm"
            summary(fit)
        })
    list(coefficients = sapply(sumrys, function(sm) coef(sm)[,"Estimate"]),
        Std.Error = sapply(sumrys, function(sm) coef(sm)[,"Std. Error"]),
        sigma = sapply(sumrys, "[[", "sigma"),
        r.squared = sapply(sumrys, "[[", "r.squared"))
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2008 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: rollingRegression.R,v 1.2 2008-06-02 16:05:19 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.1  2007/02/07 18:58:13  brian
# - add rollingRegression fn and .Rd to package
#
###############################################################################