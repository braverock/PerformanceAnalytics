\name{QP.Norm}
\alias{Normalized.CalmarRatio}
\alias{Normalized.SterlingRatio}
\alias{QP.Norm}
\alias{SterlingRatio.Normalized}
\title{QP function fo calculation of Sharpe Ratio}
\usage{
  QP.Norm(R, tau, scale = NA)

  SterlingRatio.Normalized(R, tau = 1, scale = NA,
    excess = 0.1)
}
\arguments{
  \item{R}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of asset returns}

  \item{scale}{number of periods in a year (daily scale =
  252, monthly scale = 12, quarterly scale = 4)}

  \item{excess}{for Sterling Ratio, excess amount to add to
  the max drawdown, traditionally and default .1 (10\%)}
}
\description{
  calculate a Normalized Calmar or Sterling reward/risk
  ratio
}
\details{
  Normalized Calmar and Sterling Ratios are yet another
  method of creating a risk-adjusted measure for ranking
  investments similar to the \code{\link{SharpeRatio}}.

  Both the Normalized Calmar and the Sterling ratio are the
  ratio of annualized return over the absolute value of the
  maximum drawdown of an investment. The Sterling ratio
  adds an excess risk measure to the maximum drawdown,
  traditionally and defaulting to 10\%.

  It is also traditional to use a three year return series
  for these calculations, although the functions included
  here make no effort to determine the length of your
  series.  If you want to use a subset of your series,
  you'll need to truncate or subset the input data to the
  desired length.

  Many other measures have been proposed to do similar
  reward to risk ranking. It is the opinion of this author
  that newer measures such as Sortino's
  \code{\link{UpsidePotentialRatio}} or Favre's modified
  \code{\link{SharpeRatio}} are both \dQuote{better}
  measures, and should be preferred to the Calmar or
  Sterling Ratio.
}
\examples{
data(managers)
    Normalized.CalmarRatio(managers[,1,drop=FALSE])
    Normalized.CalmarRatio(managers[,1:6])
    Normalized.SterlingRatio(managers[,1,drop=FALSE])
    Normalized.SterlingRatio(managers[,1:6])
}
\author{
  Brian G. Peterson
}
\references{
  Bacon, Carl. \emph{Magdon-Ismail, M. and Amir Atiya,
  Maximum drawdown. Risk Magazine, 01 Oct 2004.
}
\seealso{
  \code{\link{Return.annualized}}, \cr
  \code{\link{maxDrawdown}}, \cr
  \code{\link{SharpeRatio.modified}}, \cr
  \code{\link{UpsidePotentialRatio}}
}
\keyword{distribution}
\keyword{models}
\keyword{multivariate}
\keyword{ts}

