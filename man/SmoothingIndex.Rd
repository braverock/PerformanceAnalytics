\name{SmoothingIndex}
\alias{SmoothingIndex}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ calculate Normalized Getmansky Smoothing Index  }
\description{
Proposed by Getmansky et al to provide a normalized measure of "liquidity risk."
}
\usage{
SmoothingIndex(Ra, neg.thetas = FALSE, MAorder = 2, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Ra}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{neg.thetas}{ if FALSE, function removes negative coefficients (thetas) when calculating the index }
  \item{MAorder}{ specify the number of periods used to calculate the moving average, defaults to 2 }  \item{\dots}{ any other passthru parameters }
}
\value{
a value ranging from 0 to 1 (enforced only when neg.thetas = FALSE)
}
\details{
To measure the effects of smoothing, Getmansky, Lo, et al (2004) define a "smoothing profile" as a vector of coefficients for an MLE fit on returns using a two-period moving-average process. 

The moving-average process of order $k=2$ (specified using \code{MAorder}) gives $R_t = \theta_{0} R_{t} + \theta_1 R_{t -1} + \theta_2 R_{t-2}$, under the constraint that the sum of the coefficients is equal to 1. In \R, the \code{arima} function allows us to create an MA(2) model using an "ARIMA(p,d,q)" model, where $p$ is the number of autoregressive terms (AR), $d$ is the degree of differencing, and $q$ is the number of lagged forecast errors (MA) in the prediction equation.  The \code{order} parameter allows us to specify the three components (p, d, q) as an argument, e.g., \code{order = c(0, 0, 2)}.  The \code{method} specifies how to fit the model, in this case using maximum likelihood estimation (MLE) in a fashion similar to the estimation of standard moving-average time series models, using:

\code{arima(ra, order=c(0,0,2), method="ML", transform.pars=TRUE, include.mean=FALSE)}

\code{include.mean}: Getmansky, et al. (2004) p 555 "By applying the above procedure
to observed de-meaned returns...", so we set that parameter to 'FALSE'.

\code{transform.pars}: ibid, "we impose the additional restriction that the estimated MA(k)
process be invertible," so we set the parameter to 'TRUE'.

The coefficients, $\theta_{j}$, are then normalized to sum to interpreted as a "weighted average of the fund's true returns over the most recent $k + 1$ periods, including the current period."

If these weights are disproportionately centered on a small number of lags, relatively little serial correlation will be induced. However, if the weights are evenly distributed among many lags, this would show higher serial correlation. 

The paper notes that because $\theta_j \in [0, 1]$, $\xi$ is also confined to the unit interval, and is minimized when all the $\theta_j$'s are identical.  That  implies a value of $1/(k + 1)$ for $\xi$, and a maximum value of $\xi = 1$ when one coefficient is 1 and the rest are 0.  In the context of smoothed returns, a lower value of $\xi$ implies more smoothing, and the upper bound of 1 implies no smoothing.

The "smoothing index," represented as $\xi$, is calculated the same way the Herfindahl index.  The Herfindal measure is well known in the industrial organization literature as a measure of the concentration of firms in a given industry where $y_j$ represents the market share of firm $j$.

This method (as well as the implementation described in the paper), does not enforce $\theta_j \in [0, 1]$, so $\xi$ is not limited to that range either.  All we can say is that lower values are "less liquid" and higher values are "more liquid" or mis-specified.  In this function, setting the parameter neg.thetas = FALSE does enforce the limitation, eliminating negative autocorrelation coefficients from the calculation (the papers below do not make an economic case for eliminating negative autocorrelation, however).

Interpretation of the resulting value is difficult.  All we can say is that lower values
appear to have autocorrelation structure like we might expect of "less liquid" instruments.  Higher values appear "more liquid" or are poorly fit or mis-specified.

}
\references{
Chan, Nicholas, Mila Getmansky, Shane M. Haas, and Andrew W. Lo. 2005. Systemic Risk
  and Hedge Funds. NBER Working Paper Series (11200).
Getmansky, Mila, Andrew W. Lo, and Igor Makarov. 2004. An Econometric Model of Serial
  Correlation and Illiquidity in Hedge Fund Returns. Journal of Financial Economics (74):
  529-609.
}
\author{ Peter Carl }
\examples{
data(managers)
data(edhec)
indexes=merge(managers[,8:10],edhec)
indexes=na.omit(indexes)
sapply(indexes,FUN=SmoothingIndex)
SmoothingIndex(managers[,8,drop=FALSE])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }