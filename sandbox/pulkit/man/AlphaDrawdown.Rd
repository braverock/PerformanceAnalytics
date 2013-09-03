\name{AlphaDrawdown}
\alias{AlphaDrawdown}
\title{Drawdown alpha}
\usage{
  AlphaDrawdown(R, Rm, p = 0.95, weights = NULL,
    geometric = TRUE, type = c("alpha", "average", "max"),
    ...)
}
\arguments{
  \item{R}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of asset returns}

  \item{Rm}{Return series of the optimal portfolio an xts,
  vector, matrix, data frame, timeSeries or zoo object of
  asset returns}

  \item{p}{confidence level for calculation
  ,default(p=0.95)}

  \item{weights}{portfolio weighting vector, default NULL,
  see Details}

  \item{geometric}{utilize geometric chaining (TRUE) or
  simple/arithmetic chaining (FALSE) to aggregate returns,
  default TRUE}

  \item{type}{The type of BetaDrawdown if specified alpha
  then the alpha value given is taken (default 0.95). If
  "average" then alpha = 0 and if "max" then alpha = 1 is
  taken.}

  \item{\dots}{any passthru variable}
}
\description{
  Then the difference between the actual rate of return and
  the rate of return of the instrument estimated by
  \eqn{\beta_DD{w_T}} is called CDaR alpha and is given by

  \deqn{\alpha_DD = w_T - \beta_DD{w_T^M}}

  here \eqn{\beta_DD} is the beta drawdown. The code for
  beta drawdown can be found here \code{BetaDrawdown}.

  Postive \eqn{\alpha_DD} implies that the instrument did
  better than it was predicted, and consequently,
  \eqn{\alpha_DD} can be used as a performance measure to
  rank instrument and to identify those that outperformed
  their CAPM predictions
}
\examples{
data(edhec)
AlphaDrawdown(edhec[,1],edhec[,2])

AlphaDrawdown(edhec[,1],edhec[,2],type="max") # expected value : 0.8983177

AlphaDrawdown(edhec[,1],edhec[,2],type="average") # expected value : 1.692592
}
\author{
  Pulkit Mehrotra
}
\references{
  Zabarankin, M., Pavlikov, K., and S. Uryasev. Capital
  Asset Pricing Model (CAPM) with Drawdown Measure.Research
  Report 2012-9, ISE Dept., University of Florida,September
  2012.
}
\seealso{
  \code{\link{CdarMultiPath}} \code{\link{CDaR}}
  \code{\link{MultiBetaDrawdown}}
  \code{\link{BetaDrawdown}}
}

