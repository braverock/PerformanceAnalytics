\name{Penance}
\alias{Penance}
\title{Penance}
\usage{
  Penance(R, confidence = 0.95, type = c("ar", "normal"),
    ...)
}
\arguments{
  \item{R}{Returns}

  \item{confidence}{the confidence interval}

  \item{type}{The type of distribution "normal" or
  "ar"."ar" stands for Autoregressive.}

  \item{\dots}{any other passthru variable}
}
\description{
  A plot for Penance vs phi for the given portfolio The
  relationship between penance and phi is given by

  \deqn{penance = \frac{Maximum Time under
  water}{t_\alpha^{*}-1}}

  Penance Measures how long it takes to recover from the
  maximum drawdown as a multiple of the time it took to
  reach the bottom. Penance is smaller, the higher the
  value of \eqn{\phi(Phi)} and the higher the ratio
  \eqn{\frac{\mu}{\sigma}}. Positive serial autocorrelation
  leads to smaller Penance due to greater periods under
  water.
}
\examples{
data(edhec)
Penance(edhec,0.95,"ar")
Penance(edhec[,1],0.95,"normal")
}
\author{
  Pulkit Mehrotra
}
\references{
  Bailey, David H. and Lopez de Prado, Marcos,
  Drawdown-Based Stop-Outs and the "Triple Penance"
  Rule(January 1, 2013).
}
\seealso{
  \code{\link{chart.Penance}} \code{\link{table.Penance}}
  \code{\link{TuW}}
}

