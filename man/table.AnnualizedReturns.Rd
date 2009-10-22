\name{table.AnnualizedReturns}
\alias{table.AnnualizedReturns}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{  Annualized Returns Summary: Statistics and Stylized Facts }
\description{
Table of Annualized Return, Annualized Std Dev, and Annualized Sharpe
}
\usage{
table.AnnualizedReturns(R, scale = NA, Rf = 0, geometric = TRUE, digits = 4)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{geometric}{ generate geometric (TRUE) or simple (FALSE) returns, default TRUE }
  \item{scale}{ number of periods in a year (daily scale = 252, monthly scale = 12, quarterly scale = 4) }
  \item{Rf}{ risk free rate, in same period as your returns }
  \item{digits}{ number of digits to round results to }
}

\author{ Peter Carl }
\seealso{
    \code{\link{Return.annualized}} \cr
    \code{\link{StdDev.annualized}} \cr
    \code{\link{SharpeRatio.annualized}}
}
\examples{
data(managers)
table.AnnualizedReturns(managers[,1:8])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
