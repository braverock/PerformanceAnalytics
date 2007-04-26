\name{download.RiskFree}
\alias{download.RiskFree}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ download 13-week US Treasury Bill Prices and calculate 13-week US Treasury Bill returns }
\description{
This function downloads daily/monthly 13-week US Treasury Bill  \emph{prices} as the risk free asset using \code{\link[tseries]{get.hist.quote}} for "\^irx\^" and calculate \emph{returns} using \code{\link{CalculateReturns}} as a good proxy for the Risk Free Rate.
}
\usage{
download.RiskFree(start = "1998-01-01", end = NULL, compression=c("m","d"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{start}{start date in YYYY-mm-dd format, default "1998-01-01"}
  \item{end}{end date in YYYY-mm-dd format}
  \item{compression}{"m" for monthly returns and "d" for daily returns, default monthly, as passthru to \code{\link[tseries]{get.hist.quote}}}
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
   \code{\link{CalculateReturns}} \cr
   \code{\link{download.SP500PriceReturns}} \cr
   \code{\link[tseries]{get.hist.quote}}
}
\examples{
rf = download.RiskFree(start = "1997-12-01", end = "2007-01-01")
class(rf)
head(rf)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }