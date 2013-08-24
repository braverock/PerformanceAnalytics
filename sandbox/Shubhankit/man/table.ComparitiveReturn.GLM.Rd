\name{table.ComparitiveReturn.GLM}
\alias{table.ComparitiveReturn.GLM}
\title{Compenent Decomposition of Table of Unsmooth Returns for GLM Model}
\usage{
  table.ComparitiveReturn.GLM(R, n = 3, digits = 4)
}
\arguments{
  \item{R}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of asset returns}

  \item{ci}{confidence interval, defaults to 95\%}

  \item{n}{number of series lags}

  \item{digits}{number of digits to round results to}
}
\description{
  Creates a table of comparitive changes in Normality
  Properties for Third and Fourth Moment Vectors i.e.
  Skewness and Kurtosis for Orignal and Unsmooth Returns
  Respectively
}
\author{
  Peter Carl, Brian Peterson, Shubhankit Mohan
}
\references{
  Okunev, John and White, Derek R., \emph{ Hedge Fund Risk
  Factors and Value at Risk of Credit Trading Strategies}
  (October 2003). Available at SSRN:
  \url{http://ssrn.com/abstract=460641}
}
\keyword{GLM}
\keyword{models}
\keyword{return}
\keyword{ts}
\keyword{unsmooth}

