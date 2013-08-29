\name{table.UnsmoothReturn}
\alias{table.UnsmoothReturn}
\title{Table of Unsmooth Returns}
\usage{
  table.UnsmoothReturn(R, n = 3, p = 0.95, digits = 4)
}
\arguments{
  \item{R}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of asset returns}

  \item{n}{number of series lags}

  \item{p}{confidence level for calculation, default p=.99}

  \item{digits}{number of digits to round results to}
}
\description{
  Creates a table of estimates of moving averages for
  comparison across multiple instruments or funds as well
  as their standard error and smoothing index , which is
  Compenent Decomposition of Table of Unsmooth Returns
}
\details{
  The estimation method is based on a maximum likelihood
  estimation of a moving average process (we use the
  innovations algorithm proposed by \bold{Brockwell and
  Davis} [1991]). The first step of this approach consists
  in computing a series of de-meaned observed returns:
  \deqn{X(t) = R(0,t)- \mu} where \eqn{\mu} is the expected
  value of the series of observed returns. As a
  consequence, the above equation can be written as :
  \deqn{X(t)= \theta(0)\eta(t) + \theta(1)\eta(t-1) + .....
  + \theta(k)\eta(t-k)} with the additional assumption that
  : \bold{\eqn{\eta(k)= N(0,\sigma(\eta)^2)}} The structure
  of the model and the two constraints suppose that the
  complete integration of information in the price of the
  considered asset may take up to k periods because of its
  illiquidity. In addition, according to Getmansky et al.,
  this model is in line with previous models of
  nonsynchronous trading such as the one developed by
  \bold{Cohen, Maier, Schwartz and Whitcomb} [1986].
  Smoothing has an impact on the third and fourth moments
  of the returns distribution too.
}
\author{
  Peter Carl, Brian Peterson, Shubhankit Mohan
}
\references{
  Cavenaile, Laurent, Coen, Alain and Hubner,
  Georges,\emph{ The Impact of Illiquidity and Higher
  Moments of Hedge Fund Returns on Their Risk-Adjusted
  Performance and Diversification Potential} (October 30,
  2009). Journal of Alternative Investments, Forthcoming.
  Available at SSRN: \url{http://ssrn.com/abstract=1502698}
  Working paper is at
  \url{http://www.hec.ulg.ac.be/sites/default/files/workingpapers/WP_HECULg_20091001_Cavenaile_Coen_Hubner.pdf}
}
\seealso{
  Reutrn.Geltner Reutrn.GLM Return.Okunev
}
\keyword{models}
\keyword{return}
\keyword{smooth}
\keyword{ts}

