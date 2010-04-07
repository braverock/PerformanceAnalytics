\name{maxDrawdown}
\alias{maxDrawdown}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ caclulate the maximum drawdown from peak equity }
\description{
To find the maximum drawdown in a return series, we need to first calculate the cumulative returns and the maximum cumulative return to that point.  
Any time the cumulative returns dips below the maximum cumulative returns, it's a drawdown.  
Drawdowns are measured as a percentage of that maximum cumulative return, in effect, measured from peak equity.
}
\usage{
maxDrawdown(R, weights=NULL, geometric = TRUE, invert=TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{weights}{ portfolio weighting vector, default NULL, see Details}
  \item{geometric}{ generate geometric (TRUE) or simple (FALSE) returns, default TRUE }
  \item{invert}{ TRUE/FALSE whether to invert the drawdown measure.  see Details. }
  \item{\dots}{ any other passthru parameters }
}
\details{
The option to \code{invert} the measure should appease both academics and practitioners.
The default option \code{invert=TRUE} will provide the drawdown as a positive number. 
This should be useful for optimization (which usually seeks to minimize a value), and for tables (where having negative signs in front of every number may be considered clutter).
Practitioners will argue that drawdowns denote losses, and should be internally consistent with the quantile (a negative number), for which \code{invert=FALSE} will provide the value they expect.  
Individually, different preferences may apply for clarity and compactness.  
As such, we provide the option,  but make no value judgment on which approach is preferable. 
 
}
\references{
Bacon, C. \emph{Practical Portfolio Performance Measurement and Attribution}. Wiley. 2004. p. 88 \cr

}
\author{ Peter Carl }
\seealso{
    \code{\link{findDrawdowns}} \cr
    \code{\link{sortDrawdowns}} \cr
    \code{\link{table.Drawdowns}} \cr
    \code{\link{table.DownsideRisk}} \cr
    \code{\link{chart.Drawdown}} \cr
}
\examples{
data(edhec)
t(round(maxDrawdown(edhec[,"Funds of Funds"]),4))
data(managers)
t(round(maxDrawdown(managers),4))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
