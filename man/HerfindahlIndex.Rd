\name{HerfindahlIndex}
\alias{HerfindahlIndex}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ calculate Normalized Herfindahl Autocorrelation Index  }
\description{
The Herfindahl Concentration Index was developed to measure concentration of manufacurers or suppliers in a marketplace.  It has been adapted to provide a normalized measure of liquidity risk.  The index will produces  a number from zero to one.  A low number indicates low liquidity risk.  A number trending towards one indicates a higher liquidity risk.
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
Normalized Herfindal AutoCorrelation Index from 0-1
}
\details{
Equation:

\emph{enter equations}

When used as a concentration index, the values are generally evaluated as follows:

Between 0 and 0.1 indicates an unconcentrated index

0.1 to 0.18 indicates moderate concentration

above 0.18 indicates high concentration

we are not aware of any similar ``rule of thumb'' for the Herfindahl autocorrelation index as presented here to measure low liquidity risk versus high liquidity risk.  Some relativly illiquid hedge fund styles may display a Herfindahl close to 0.5, while three month US Treasuries display a Herfindahl close to 0.05.

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