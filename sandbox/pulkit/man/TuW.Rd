\name{TuW}
\alias{TuW}
\title{Maximum Time Under Water}
\usage{
  TuW(R, confidence, type = c("ar", "normal"), ...)
}
\arguments{
  \item{R}{return series}

  \item{confidence}{the confidence interval}

  \item{type}{The type of distribution "normal" or
  "ar"."ar" stands for Autoregressive.}
}
\description{
  \code{TriplePenance} calculates the maximum Maximum Time
  under water for a particular confidence interval is given
  by

  For a particular sequence \eqn{\left\{\pi_t\right\}}, the
  time under water \eqn{(TuW)} is the minimum number of
  observations, \eqn{t>0}, such that \eqn{\pi_{t-1}<0} and
  \eqn{\pi_t>0}.

  For a normal distribution Maximum Time Under Water is
  given by the following expression.
  \deqn{MaxTuW_\alpha=\biggl(\frac{Z_\alpha{\sigma}}{\mu}\biggr)^2}

  For a Autoregressive process the Time under water is
  found using the golden section algorithm.

  The non normal time dependent process is defined by

  \deqn{\triangle{\pi_{\tau}}=(1-\phi)\mu +
  \phi{\delta_{\tau-1}} + \sigma{\epsilon_{\tau}}}

  The random shocks are iid distributed
  \eqn{\epsilon_{\tau}~N(0,1)}. These random shocks follow
  an independent and identically distributed Gaussian
  Process, however \eqn{\triangle{\pi_\tau}} is neither an
  independent nor an identically distributed Gaussian
  Process. This is due to the parameter \eqn{\phi}, which
  incorporates a first-order serial-correlation effect of
  auto-regressive form.
}
\examples{
data(edhec)
TuW(edhec,0.95,"ar")
TuW(edhec[,1],0.95,"normal") # expected value 103.2573
}
\author{
  Pulkit Mehrotra
}
\references{
  Bailey, David H. and Lopez de Prado, Marcos,
  Drawdown-Based Stop-Outs and the ‘Triple Penance’
  Rule(January 1, 2013).
}
\seealso{
  \code{\link{chart.Penance}} \code{\link{MaxDD}}
  \code{\link{table.Penance}}
}

