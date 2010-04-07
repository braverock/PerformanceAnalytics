\name{CDD}
\alias{CDD}
\alias{CDaR}
\concept{drawdown}
\concept{conditional drawdown}
\concept{conditional drawdown at risk}
\title{
Calculate Uryasev's proposed Conditional Drawdown at Risk (CDD or CDaR) measure
}
\description{
For some confidence level \eqn{p}, the conditional drawdown is the the mean of the worst \eqn{p\%} drawdowns. 
}
\usage{
CDD(R, weights = NULL, geometric = TRUE, invert = TRUE, p = 0.95, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{weights}{ portfolio weighting vector, default NULL, see Details}
  \item{geometric}{ generate geometric (TRUE) or simple (FALSE) returns, default TRUE }
  \item{invert}{ TRUE/FALSE whether to invert the drawdown measure.  see Details. }
  \item{p}{ confidence level for calculation, default p=0.95 }
  \item{\dots}{ any other passthru parameters }
}
\references{
Chekhlov, A., Uryasev, S., and M. Zabarankin. Portfolio Optimization With Drawdown Constraints. B. Scherer (Ed.) Asset and Liability Management Tools, Risk Books, London, 2003
http://www.ise.ufl.edu/uryasev/drawdown.pdf
}
\seealso{
\code{\link{ES}}
\code{\link{maxDrawdown}}
}
\author{
Brian G. Peterson
}
\examples{
data(edhec)
t(round(CDD(edhec),4))
}
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }