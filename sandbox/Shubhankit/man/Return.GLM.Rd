\name{Return.GLM}
\alias{Return.GLM}
\title{Getmansky Lo Markov Unsmooth Return Model}
\usage{
  Return.GLM(Ra, q = 3)
}
\arguments{
  \item{Ra}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of asset returns}

  \item{q}{order of autocorrelation coefficient}
}
\description{
  True returns represent the flow of information that would
  determine the equilibrium value of the fund's securities
  in a frictionless market. However, true economic returns
  are not observed. Instead, Rot denotes the reported or
  observed return in period t, which is a weighted average
  of the fund's true returns over the most recent k þ 1
  periods, includingthe current period. This averaging
  process captures the essence of smoothed returns in
  several respects. From the perspective of
  illiquidity-driven smoothing, is consistent with several
  models in the nonsynchronous tradingliterat ure. For
  example, Cohen et al. (1 986, Chapter 6.1) propose a
  similar weighted-average model for observed returns.
}
\details{
  The Geltner autocorrelation adjusted return series may be
  calculated via:
}
\examples{
data(edhec)
Return.GLM(edhec,4)
}
\author{
  R
}
\references{
  "An econometric model of serial correlation and
  illiquidity in hedge fund returns Mila Getmansky1, Andrew
  W. Lo*, Igor Makarov MIT Sloan School of Management, 50
  Memorial Drive, E52-432, Cambridge, MA 02142-1347, USA
  Received 16 October 2002; received in revised form 7
  March 2003; accepted 15 May 2003 Available online 10 July
  2004
}
\keyword{distribution}
\keyword{models}
\keyword{multivariate}
\keyword{ts}

