\name{CAPM.utils}
\alias{CAPM.utils}
\alias{CAPM.RiskPremium}
\alias{CAPM.CML}
\alias{CAPM.CML.slope}
\alias{CAPM.SML.slope}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ utility functions for CAPM CML, SML, and RiskPremium }
\description{
The Capital Asset Pricing Model, from which the popular \code{\link{SharpeRatio}} is derived, is a theory of market equilibrium.
These utility functions provide values for various measures proposed in the CAPM.
}
\usage{
CAPM.CML.slope(Rb, rf = 0 )
CAPM.CML(Ra, Rb, rf = 0)
CAPM.RiskPremium(Ra, rf = 0)
CAPM.SML.slope(Rb, rf = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Ra}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{Rb}{ return vector of the benchmark asset }
  \item{rf}{ risk free rate, in same period as your returns }
}
\value{
CAPM value for function called
}
\details{
The CAPM provides a justification for passive or index investing by positing that assets that are not on the efficient frontier will either rise or lower in price until they are on the efficient frontier of the market portfolio.

The CAPM Risk Premium on an investment is the measure of how much the asset's performance differs from the risk free rate.  Negative Risk Premium generally indicates that the investment is a bad investment, and the money should be allocated to the risk free asset or to a different asset with a higher risk premium.

The Capital Market Line relates the excess expected return on an efficient market portfolio to it's Risk.  The slope of the CML is the Sharpe Ratio for the market portfolio. The Security Market line is constructed by calculating the line of Risk Premium over \code{\link{CAPM.beta}}.  For the benchmark asset this will be 1 over the risk premium of the benchmark asset.   The CML also describes the only path allowed by the CAPM to a portfolio that outperforms the efficient frontier: it describes the line of reward/risk that a leveraged portfolio will occupy.  So, according to CAPM, no portfolio constructed of the same assets can lie above the CML.

Probably the most complete criticism of CAPM in actual practice (as opposed to structural or theory critiques) is that it posits a market equilibrium, but is most often used only in a partial equilibrium setting, for example by using the S\&P 500 as the benchmark asset.  A better method of using and testing the CAPM would be to use a general equilibrium model that took global assets from all asset classes into consideration.

Chapter 7 of Ruppert(2004) gives an extensive overview of CAPM, its assumptions and deficiencies.

\code{CAPM.RiskPremium} is the premium returned to the investor over the risk free asset

\deqn{\overline{(R_{a}-R_{f})}}{mean(Ra-rf)}

\code{CAPM.CML} calculates the expected return of the asset against the benchmark Capital Market Line

\code{CAPM.CML.slope} calculates the slope of the Capital Market Line for looking at how a particular asset compares to the CML

\code{CAPM.SML.slope} calculates the slope of the Security Market Line for looking at how a particular asset compares to the SML created by the benchmark

}
\references{
Sharpe, W.F. The Sharpe Ratio,\emph{Journal of Portfolio Management},Fall 1994, 49-58. \cr
Sharpe, W.F. Capital Asset Prices: A theory of market equilibrium under conditions of risk. \emph{Journal of finance}, vol 19, 1964, 425-442. \cr
Ruppert, David. \emph{Statistics and Finance, an Introduction}. Springer. 2004. \cr
}
\author{ Brian G. Peterson }
\note{}
\seealso{
    \code{\link{CAPM.beta}}
    \code{\link{CAPM.alpha}}
    \code{\link{SharpeRatio}}
    \code{\link{InformationRatio}}
    \code{\link{TrackingError}}
    \code{\link{ActivePremium}}
}
\examples{
data(managers)
managers.length = dim(managers)[1]
start = rownames(managers[1,])
start
end = rownames(managers[managers.length,])
rf = managers[,10,drop=FALSE]

Rb = managers[,"SP500.TR",drop=FALSE]

CAPM.CML.slope(Rb, rf )

CAPM.CML(managers[,"HAM1",drop=FALSE], Rb, rf)

CAPM.RiskPremium(Rb, rf)

CAPM.RiskPremium(managers[,"HAM1",drop=FALSE], rf)

CAPM.SML.slope(Rb, rf)

# should create plots like in Ruppert 7.1 7.2

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
