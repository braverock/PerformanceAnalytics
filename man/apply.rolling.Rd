\name{apply.rolling}
\alias{apply.rolling}
\concept{rolling window}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ calculate a function over a rolling window }
\description{
Creates a results timeseries of a function applied over a rolling window.

Wrapper function for \code{\link[zoo]{rollapply}} to hide some of the complexity of managing single-column zoo objects.
}
\usage{
apply.rolling(R, width = 12, FUN = "mean", na.pad = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{FUN}{any function that can be evaluated using a single set of returns (e.g., rolling beta won't work, but \code{\link{Return.annualized}} will)}
  \item{width}{ number of periods to apply rolling function window over }
  \item{na.pad}{ TRUE/FALSE If TRUE it adds any times that would not otherwise have been in the result with a value of NA. If FALSE those times are dropped. }
  \item{\dots}{ any other passthru parameters }
}
\value{
A timeseries in a zoo object of the calculation results
}
\author{ Peter Carl }
\seealso{
    \code{\link{apply}} \cr
    \code{\link[zoo]{rollapply}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }