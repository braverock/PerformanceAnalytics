\name{table.Correlation}
\alias{table.Correlation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ calculate correlalations of multicolumn data }
\description{
This is a wrapper for calculating correlation and significance against each column of the data provided.
}
\usage{
table.Correlation(Ra, Rb, n, trim = TRUE, na.rm = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Ra}{ a vector of returns to test, e.g., the asset to be examined }
  \item{Rb}{ a matrix, data.frame, or timeSeries of benchmark(s) to test the asset against. }
  \item{n}{ ~~Describe \code{n} here~~ }
  \item{trim}{ TRUE/FALSE, whether to keep alignment caused by NA's }
  \item{na.rm}{ TRUE/FALSE Remove NA's from the returns? }
  \item{\dots}{ any other passthru parameters }
}
\value{
Calculates and presents correlation and significance against each column of the data provided.
}
\references{ }
\author{ Peter Carl }
\note{}
\seealso{}
\examples{
data(edhec)
data(edhec)
edhec.length = dim(edhec)[1]
start = rownames(edhec[1,])
start
end = rownames(edhec[edhec.length,])
sp500 = download.SP500PriceReturns(start="1996-12-31",end=end)
table.Correlation(sp500,edhec)
ctable = table.Correlation(sp500,edhec, conf.level=.99)
dotchart(ctable[,1],labels=rownames(ctable),xlim=c(-1,1))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
