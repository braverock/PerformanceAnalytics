\name{download.RiskFree}
\alias{download.RiskFree}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ download S&P Prices and calculate S&P returns }
\description{
This function downloads daily/monthly 13-week US Treasury Bill  \emph{prices} as the risk free asset using \code{\link{get.hist.quote}} for "^irx^" and calculate \emph{returns} using \code{\link{CalculateReturns}}.
}
\usage{
download.RiskFree(start = "1998-01-01", end = NULL, compression=c("m","d"), method=c("simple","compound"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{start}{start date in YYYY-mm-dd format, default "1998-01-01"}
  \item{end}{end date in YYYY-mm-dd format}
  \item{compression}{"m" for monthly returns and "d" for daily returns, default monthly, as passthru to \code{\link{get.hist.quote}}}
  \item{method}{"simple" or "compound" as a passthru to \code{\link{CalculateReturns}},default simple}
}
\value{
matrix of simple or compound returns on the 13-week US Treasury Bill at the daily/monthly scale specified
}
\details{
}
\references{ }
\author{ Peter Carl }
\note{
}
\seealso{
   \code{\link{CalculateReturns}}
   \code{\link{download.SP500PriceReturns}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }