\name{DrawdownGPD}
\alias{DrawdownGPD}
\title{Modelling Drawdown using Extreme Value Theory

It has been shown empirically that Drawdowns can be modelled using Modified Generalized Pareto
distribution(MGPD), Generalized Pareto Distribution(GPD) and other particular cases of MGPD such
as weibull distribution \eqn{MGPD(\gamma,0,\psi)} and unit exponential distribution\eqn{MGPD(1,0,\psi)}

Modified Generalized Pareto Distribution is given by the following formula

\dqeqn{G_{\eta}(m) = \begin{array}{l} 1-(1+\eta\frac{m^\gamma}{\psi})^(-1/\eta), if \eta \neq 0 \\ 1- e^{-frac{m^\gamma}{\psi}}, if \eta = 0,\end{array}}

Here \eqn{\gamma{\epsilon}R} is the modifying parameter. When \eqn{\gamma<1} the corresponding densities are
strictly decreasing with heavier tail; the GDP is recovered by setting \eqn{\gamma = 1} .\eqn{\gamma \textgreater 1}

The GDP is given by the following equation. \eqn{MGPD(1,\eta,\psi)}

\deqn{G_{\eta}(m) = \begin{array}{l} 1-(1+\eta\frac{m}{\psi})^(-1/\eta), if \eta \neq 0 \\ 1- e^{-frac{m}{\psi}}, if \eta = 0,\end{array}}

The weibull distribution is given by the following equation \eqn{MGPD(\gamma,0,\psi)}

\deqn{G(m) =  1- e^{-frac{m^\gamma}{\psi}}}

The unit exponential distribution is given by the following equation \eqn{MGPD(1,0,\psi)}

\deqn{G(m) =  1- e^{-m}}}
\usage{
  DrawdownGPD(R, type = c("gpd", "pd", "weibull"),
    threshold = 0.9)
}
\arguments{
  \item{R}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of asset return}

  \item{type}{The type of distribution
  "gpd","pd","weibull"}

  \item{threshold}{The threshold beyond which the drawdowns
  have to be modelled}
}
\description{
  Modelling Drawdown using Extreme Value Theory

  It has been shown empirically that Drawdowns can be
  modelled using Modified Generalized Pareto
  distribution(MGPD), Generalized Pareto Distribution(GPD)
  and other particular cases of MGPD such as weibull
  distribution \eqn{MGPD(\gamma,0,\psi)} and unit
  exponential distribution\eqn{MGPD(1,0,\psi)}

  Modified Generalized Pareto Distribution is given by the
  following formula

  \dqeqn{G_{\eta}(m) = \begin{array}{l}
  1-(1+\eta\frac{m^\gamma}{\psi})^(-1/\eta), if \eta \neq 0
  \\ 1- e^{-frac{m^\gamma}{\psi}}, if \eta = 0,\end{array}}

  Here \eqn{\gamma{\epsilon}R} is the modifying parameter.
  When \eqn{\gamma<1} the corresponding densities are
  strictly decreasing with heavier tail; the GDP is
  recovered by setting \eqn{\gamma = 1} .\eqn{\gamma
  \textgreater 1}

  The GDP is given by the following equation.
  \eqn{MGPD(1,\eta,\psi)}

  \deqn{G_{\eta}(m) = \begin{array}{l}
  1-(1+\eta\frac{m}{\psi})^(-1/\eta), if \eta \neq 0 \\ 1-
  e^{-frac{m}{\psi}}, if \eta = 0,\end{array}}

  The weibull distribution is given by the following
  equation \eqn{MGPD(\gamma,0,\psi)}

  \deqn{G(m) = 1- e^{-frac{m^\gamma}{\psi}}}

  The unit exponential distribution is given by the
  following equation \eqn{MGPD(1,0,\psi)}

  \deqn{G(m) = 1- e^{-m}}
}
\references{
  Mendes, Beatriz V.M. and Leal, Ricardo P.C., Maximum
  Drawdown: Models and Applications (November 2003).
  Coppead Working Paper Series No. 359. Available at SSRN:
  http://ssrn.com/abstract=477322 or
  http://dx.doi.org/10.2139/ssrn.477322.
}

