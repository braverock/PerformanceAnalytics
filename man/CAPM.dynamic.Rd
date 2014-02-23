\name{CAPM.dynamic}
\alias{CAPM.dynamic}
\alias{SFM.dynamic}
\title{Time-varying conditional single factor model beta}
\usage{
CAPM.dynamic(Ra, Rb, Rf = 0, Z, lags = 1, ...)
}
\arguments{
  \item{Ra}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of the asset returns}

  \item{Rb}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of the benchmark asset return}

  \item{Rf}{risk free rate, in same period as your returns}

  \item{Z}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of k variables that reflect public
  information}

  \item{lags}{number of lags before the current period on
  which the alpha and beta are conditioned}

  \item{\dots}{any other passthrough parameters}
}
\description{
CAPM is estimated assuming that betas and alphas change
over time. It is assumed that the market prices of
securities fully reflect readily available and public
information. A matrix of market information variables,
\eqn{Z} measures this information. Possible variables in
\eqn{Z} could be the divident yield, Tresaury yield, etc.
The betas of stocks and managed portfolios are allowed to
change with market conditions:
}
\details{
\deqn{\beta_{p}(z_{t})=b_{0p}+B_{p}'z_{t}}{beta(zt) = b0 +
Bp'zt}

where \eqn{z_{t}=Z_{t}-E[Z]}{zt = Zt - E[Z]}

- a normalized vector of the deviations of \eqn{Z_{t}}{Zt},
\eqn{B_{p}}{Bp}

- a vector with the same dimension as \eqn{Z_{t}}{Zt}.

The coefficient \eqn{b_{0p}}{b0} can be interpreted as the
"average beta" or the beta when all infromation variables
are at their means. The elements of \eqn{B_{p}}{Bp} measure
the sensitivity of the conditional beta to the deviations
of the \eqn{Z_{t}}{Zt} from their means. In the similar way
the time-varying conditional alpha is modeled:
\deqn{\alpha_{pt}=\alpha_{p}(z_{t})=\alpha_{0p}+A_{p}'z_{t}}{alpha(zt)
= a0 + Ap'zt} The modified regression is therefore:
\deqn{r_{pt+1}=\alpha_{0p}+A_{p}'z_{t}+b_{0p}r_{bt+1}+B_{p}'[z_{t}r_{bt+1}]+
\mu_{pt+1}}
}
\examples{
data(managers)
CAPM.dynamic(managers[,1,drop=FALSE], managers[,8,drop=FALSE],
             Rf=.035/12, Z=managers[, 9:10])

CAPM.dynamic(managers[80:120,1:6], managers[80:120,7,drop=FALSE],
             Rf=managers[80:120,10,drop=FALSE], Z=managers[80:120, 9:10])

CAPM.dynamic(managers[80:120,1:6], managers[80:120,8:7],
              managers[80:120,10,drop=FALSE], Z=managers[80:120, 9:10])
}
\author{
Andrii Babii
}
\references{
J. Christopherson, D. Carino, W. Ferson. \emph{Portfolio
Performance Measurement and Benchmarking}. 2009.
McGraw-Hill. Chapter 12. \cr Wayne E. Ferson and Rudi
Schadt, "Measuring Fund Strategy and Performance in
Changing Economic Conditions," \emph{Journal of Finance},
vol. 51, 1996, pp.425-462 \cr
}
\seealso{
\code{\link{CAPM.beta}}
}

