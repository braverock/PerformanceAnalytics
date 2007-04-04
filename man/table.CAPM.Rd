\name{table.CAPM}
\alias{table.CAPM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Asset-Pricing Model Summary: Statistics and Stylized Facts }
\description{
Takes a set of returns and relates them to a market benchmark. Provides a set of measures related to the excess return single index model, or CAPM.
}
\usage{
table.CAPM(Ra, Rb, scale = 12, rf = 0, digits = 4)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Ra}{ a vector of returns to test, e.g., the asset to be examined }
  \item{Rb}{ a matrix, data.frame, or timeSeries of benchmark(s) to test the asset against. }
  \item{scale}{ number of periods in a year (daily scale = 252, monthly scale = 12, quarterly scale = 4) }
  \item{rf}{ risk free rate, in same period as your returns }
  \item{digits}{ number of digits to round results to }
}
\details{ This table will show statistics pertaining to an asset against a set of benchmarks, but cannot \(yet\) be used to show statistics for a set of assets against a benchmark. }
\value{ A data frame with columns named for the columns in \code{Rb} and rows containing statistics.  For example: \cr
\code{table.CAPM(monthlyReturns.ts@Data[, 1],seriesData(monthlyReturns.ts[, 2]))} \cr
\preformatted{
                    S&P500TR
Alpha                 0.0065
Beta                  0.2909
R-squared             0.0987
Annualized Alpha      0.0809
Correlation           0.3142
Correlation p-value   0.0547
Tracking Error        0.0149
Active Premium       -0.0083
Information Ratio    -0.5606
Treynor Ratio         0.3959}

}
\references{ }
\author{ Peter Carl }
\seealso{
    \code{\link{CAPM.alpha}}
    \code{\link{CAPM.beta}}
    \code{\link{TrackingError}}
    \code{\link{ActivePremium}}
    \code{\link{InformationRatio}}
    \code{\link{TreynorRatio}}
}
\examples{
data(edhec)
edhec.length = dim(edhec)[1]
start = rownames(edhec[1,])
start
end = rownames(edhec[edhec.length,])
rf = download.RiskFree(start = start, end = end)
sp500 = download.SP500PriceReturns(start = "1996-12-31", end = end)

table.CAPM(edhec[,"Funds.of.Funds"], sp500, rf = rf)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
