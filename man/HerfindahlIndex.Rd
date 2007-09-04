\name{HerfindahlIndex}
\alias{HerfindahlIndex}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ calculate Normalized Herfindahl Autocorrelation Index  }
\description{
The Herfindahl Concentration Index was originally developed to measure concentration of manufacurers or suppliers in a marketplace.  It has been adapted to provide a normalized measure of liquidity risk.  The index will produces  a number from zero to one.  A low number indicates low liquidity risk.  A number trending towards one indicates a higher liquidity risk.
}
\usage{
HerfindahlIndex(Ra, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Ra}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{\dots}{ any other passthru parameters }
}
\value{
Normalized Herfindal AutoCorrelation Index, a value ranging from 0 to 1
}
\details{
Equation:

\emph{enter equations}

The Herfindahl index is an adaptation of a measure used to identify concentration across industries using market share as an input.  Here, however, we use it to identify which funds have a particular positive autocorrelation coefficient that dominates its other autocorrelation coefficients.  

This function considers only the positive autocorrelation coefficients and normalizes their values so that they sum to one.  This measure does not measure total autocorrelation.  Instead, a high score indicates a dominant positive autocorrelation in one period -- which could be any period in the autocorrelation series.  This is, in effect, a measure of the concentration of autocorrelation for the fund.

Autocorrelation in hedge funds is thought to be suggestive of illiquidity where funds hold securities that are difficult to price, or deliberate smoothing of monthly returns by the manager.  We would note that there are several possible sources of autocorrelation (including the fee structure), so interpretation of this measure is tricky.

When used as a concentration index, the values are generally evaluated as follows:

Between 0 and 0.1 indicates an unconcentrated index

0.1 to 0.18 indicates moderate concentration

above 0.18 indicates high concentration

We are not aware of any similar ``rule of thumb'' for the Herfindahl autocorrelation index as presented here to measure low liquidity risk versus high liquidity risk.  Some relativly illiquid hedge fund styles may display a Herfindahl close to 0.5, while three month US Treasuries display a Herfindahl close to 0.05.

We hypothesize that the Herfindahl autocorrelation index may be useful in investment analysis in two major ways.  First, it may be used to rank-order several instruments by their relative liquidity risk as part of the investment decision.  Second, it may also be used on a rolling basis to show stability of the liquidity risk posed by a manager, or change in the liquidity risk in a varying liquidity market.

}
\references{
,p. 27
}
\author{ Brian Peterson }
\examples{
data(managers)
data(edhec)
HerfindahlIndex(managers)
HerfindahlIndex(edhec)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }