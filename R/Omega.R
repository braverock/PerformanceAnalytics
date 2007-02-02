`Omega` <-
function (R, L = 0, method = c("simple", "interp", "binomial", "blackscholes"), output = c("point", "full"), rf = 0)
{ # @author Peter Carl

    # DESCRIPTION
    # Keating and Shadwick (2002) proposed Omega (referred to as Gamma in their
    # original paper) as a way to capture all of the higher moments of the
    # returns distribution.  Mathematically, Omega is:
    #   integral[L to b](1 - F(r))dr / integral[a to L](F(r))dr
    # where the cumulative distribution F is defined on the interval (a,b).
    # L is the loss threshold that can be specified as zero, return from a
    # benchmark index, or an absolute rate of return - any specified level.
    # When comparing alternatives using Omega, L should be common.  Input data
    # can be transformed prior to calculation, which may be useful for
    # introducing risk aversion.

    # This function returns a vector of Omega, useful for plotting.  The
    # steeper, the less risky.  Above it's mean, a steeply sloped Omega also
    # implies a very limited potential for further gain.

    # Omega has a value of 1 at the mean of the distribution.

    # Omega is sub-additive.  The ratio is dimensionless.

    # Kazemi, Schneeweis, and Gupta (2003), in "Omega as a Performance Measure"
    # shows that Omega can be written as:
    #   Omega(L) = C(L)/P(L)
    # where C(L) is essentially the price of a European call option written
    # on the investment and P(L) is essentially the price of a European put
    # option written on the investment.  The maturity for both options is
    # one period (e.g., one month) and L is the strike price of both options.

    # The numerator and the denominator can be expressed as:
    #   exp(-rf) * E[max(x - L, 0)]
    #   exp(-rf) * E[max(L - x, 0)]
    # with exp(-rf) calculating the present values of the two, where rf is
    # the per-period riskless rate.

    # The first three methods implemented here focus on that observation.
    # The first method takes the simplification described above.  The second
    # uses the Black-Scholes option pricing as implemented in fOptions.  The
    # third uses the binomial pricing model from fOptions.  The second and
    # third methods are not implemented here.

    # The fourth method, "interp", creates a linear interpolation of the cdf of
    # returns, calculates Omega as a vector, and finally interpolates a function
    # for Omega as a function of L.

    # FUNCTION

    x = checkDataVector(R)

    if(method == "simple") {
        numerator = exp(-rf) * mean(max(x - L, 0))
        denominator = exp(-rf) * mean(max(L - x, 0))
        omega = numerator/denominator
    }

    if(method == "interp") {
    #

        a = min(x)
        b = max(x)

        xcdf = ecdf(x)
        f <- approxfun(xcdf$x,xcdf$y,method="linear",ties="ordered")

        if(output == "full") {
            omega = cumsum(1-f(xcdf$x))/cumsum(f(xcdf$x))
        }
        else {
        # returns only the point value for L
            # to get a point measure for omega, have to interpolate
            omegafull = cumsum(1-f(xcdf$x))/cumsum(f(xcdf$x)) # ????????
            g <- approxfun(xcdf$x,omegafull,method="linear",ties="ordered")
            omega = g(L)
        }
    }
    result = omega
    result

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: Omega.R,v 1.1 2007-02-02 19:06:15 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
###############################################################################