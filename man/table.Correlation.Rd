\name{table.Correlation}
\alias{table.Correlation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ calculate correlalations of multicolumn data }
\description{
This is a wrapper for calculating correlation and significance against each column of the data provided.
}
\usage{
table.Correlation(Ra, Rb, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Ra}{ a vector of returns to test, e.g., the asset to be examined }
  \item{Rb}{ a matrix, data.frame, or timeSeries of benchmark(s) to test the asset against. }
  \item{\dots}{ any other passthru parameters to \code{\link{cor.test}} }
}
\value{
A dataframe with correlation and significance against each column of the data provided as \code{Rb}.
}
\references{ }
\author{ Peter Carl }
\note{}
\seealso{\code{\link{cor.test}}}
\examples{
# First we load the data
data(edhec)
edhec.length = dim(edhec)[1]
start = rownames(edhec[1,])
start
end = rownames(edhec[edhec.length,])
edhec.zoo = zoo(edhec, order.by = rownames(edhec))
rf.zoo = download.RiskFree(start = start, end = end)
sp500.zoo = download.SP500PriceReturns(start = "1996-12-31", end = end)

# Now we have to align it as "monthly" data
time(edhec.zoo) = as.yearmon(time(edhec.zoo))
time(sp500.zoo) = as.yearmon(time(sp500.zoo))
time(rf.zoo) = as.yearmon(time(rf.zoo))
data.zoo = merge(edhec.zoo,sp500.zoo)
time(data.zoo) = as.Date(time(data.zoo),format="\%b \%Y")
time(rf.zoo) = as.Date(time(rf.zoo),format="\%b \%Y")

table.Correlation(data.zoo[,14,drop=FALSE],data.zoo[,1:13])
ctable = table.Correlation(data.zoo[,14,drop=FALSE],data.zoo[,1:13], conf.level=.99)
dotchart(ctable[,1],labels=rownames(ctable),xlim=c(-1,1))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
