\name{apply.fromstart}
\alias{apply.fromstart}
\concept{inception}
\concept{from inception}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ calculate a function over an expanding window always starting from the beginning of the series }
\description{
A function to calculate a function over an expanding window from the start of the timeseries.  This wrapper allows easy calculation of \dQuote{from inception} statistics.
}
\usage{
apply.fromstart(R, FUN = "mean" , gap = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{FUN}{any function that can be evaluated using a single set of returns (e.g., rolling beta won't work, but \code{\link{Return.annualized}} will)}
  \item{gap}{the number of data points from the beginning of the series required to \dQuote{train} the calculation}
  \item{\dots}{ any other passthru parameters }
}
\author{ Peter Carl }
\seealso{
    \code{\link[zoo]{rollapply}}
}
\examples{
data(managers)
apply.fromstart(managers[,1,drop=FALSE], FUN="mean", width=36)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }