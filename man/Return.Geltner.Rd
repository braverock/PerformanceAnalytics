\name{Return.Geltner}
\alias{Return.Geltner}
\title{calculate Geltner liquidity-adjusted return series}
\usage{
Return.Geltner(Ra, ...)
}
\arguments{
  \item{Ra}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of asset returns}

  \item{\dots}{any other passthru parameters}
}
\description{
David Geltner developed a method to remove estimating or
liquidity bias in real estate index returns.  It has since
been applied with success to other return series that show
autocorrelation or illiquidity effects.
}
\details{
The theory is that by correcting for autocorrelation, you
are uncovering a "true" return from a series of observed
returns that contain illiquidity or manual pricing effects.

The Geltner autocorrelation adjusted return series may be
calculated via:

\deqn{ }{Geltner.returns = [R(t) -
R(t-1)*acf(R(t-1))]/1-acf(R(t-1)) }\deqn{
R_{G}=\frac{R_{t}-(R_{t-1}\cdot\rho_{1})}{1-\rho_{1}}
}{Geltner.returns = [R(t) -
R(t-1)*acf(R(t-1))]/1-acf(R(t-1)) }

where \eqn{\rho_{1}}{acf(R(t-1))} is the first-order
autocorrelation of the return series \eqn{R_{a}}{Ra} and
\eqn{R_{t}}{R(t)} is the return of \eqn{R_{a}}{Ra} at time
\eqn{t} and \eqn{R_{t-1}}{R(t-1)} is the one-period lagged
return.
}
\examples{
data(managers)
head(Return.Geltner(managers[,1:3]),n=20)
}
\author{
Brian Peterson
}
\references{
"Edhec Funds of Hedge Funds Reporting Survey : A
Return-Based Approach to Funds of Hedge Funds
Reporting",Edhec Risk and Asset Management Research Centre,
January 2005,p. 27

Geltner, David, 1991, Smoothing in Appraisal-Based Returns,
Journal of Real Estate Finance and Economics, Vol.4,
p.327-345.

Geltner, David, 1993, Estimating Market Values from
Appraised Values without Assuming an Efficient Market,
Journal of Real Estate Research, Vol.8, p.325-345.
}
\keyword{distribution}
\keyword{models}
\keyword{multivariate}
\keyword{ts}

