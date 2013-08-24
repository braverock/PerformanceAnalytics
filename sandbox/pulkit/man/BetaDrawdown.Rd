\name{BetaDrawdown}
\alias{BetaDrawdown}
\title{Drawdown Beta for single path}
\usage{
  BetaDrawdown(R, Rm, h = 0, p = 0.95, weights = NULL,
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

  \item{\dots}{any passthru variable.}
}
\description{
  The drawdown beta is formulated as follows

  \deqn{\beta_DD =
  \frac{{\sum_{t=1}^T}{q_t^\**}{(w_{k^{\**}(t)}-w_t)}}{D_{\alpha}(w^M)}}
  here \eqn{\beta_DD} is the drawdown beta of the
  instrument.
  \eqn{k^{\**}(t)\in{argmax_{t_{\tau}{\le}k{\le}t}}w_k^M}

  \eqn{q_t^\**=1/((1-\alpha)T)} if \eqn{d_t^M} is one of
  the \eqn{(1-\alpha)T} largest drawdowns \eqn{d_1^{M}
  ,......d_t^M} of the optimal portfolio and \eqn{q_t^\** =
  0} otherwise. It is assumed that \eqn{D_\alpha(w^M)
  {\neq} 0} and that \eqn{q_t^\**} and \eqn{k^{\**}(t)} are
  uniquely determined for all \eqn{t = 1....T}

  The numerator in \eqn{\beta_DD} is the average rate of
  return of the instrument over time periods corresponding
  to the \eqn{(1-\alpha)T} largest drawdowns of the optimal
  portfolio, where \eqn{w_t - w_k^{\**}(t)} is the
  cumulative rate of return of the instrument from the
  optimal portfolio peak time \eqn{k^\**(t)} to time t.

  The difference in CDaR and standard betas can be
  explained by the conceptual difference in beta
  definitions: the standard beta accounts for the fund
  returns over the whole return history, including the
  periods when the market goes up, while CDaR betas focus
  only on market drawdowns and, thus, are not affected when
  the market performs well.
}
\examples{
data(edhec)
BetaDrawdown(edhec[,1],edhec[,2])
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
  \code{\link{ES}} \code{\link{maxDrawdown}}
  \code{\link{CdarMultiPath}} \code{\link{AlphaDrawdown}}
  \code{\link{MultiBetaDrawdown}} \code{\link{CDaR}}
}

