\name{MonteSimulTriplePenance}
\alias{MonteSimulTriplePenance}
\title{Monte Carlo Simulation for the Triple Penance Rule}
\usage{
  MonteSimulTriplePenance(size, phi, mu, sigma, dp0, bets,
    confidence)
}
\arguments{
  \item{size}{size of the Monte Carlo experiment}

  \item{phi}{AR(1) coefficient}

  \item{mu}{unconditional mean}

  \item{sigma}{Standard deviation of the random shock}

  \item{dp0}{Bet at origin (initialization of AR(1))}

  \item{bets}{Number of bets in the cumulative process}

  \item{confidence}{Confidence level for quantile}
}
\description{
  The following process is simulated using the monte carlo
  process and the maximum drawdown is calculated using it.
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
MonteSimulTriplePenance(10^6,0.5,1,2,1,25,0.95) # Expected Value Quantile (Exact) = 6.781592
}
\author{
  Pulkit Mehrotra
}
\references{
  Bailey, David H. and Lopez de Prado, Marcos,
  Drawdown-Based Stop-Outs and the ‘Triple Penance’
  Rule(January 1, 2013).
}

