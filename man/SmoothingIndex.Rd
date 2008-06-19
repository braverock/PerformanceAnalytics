\name{SmoothingIndex}
\alias{SmoothingIndex}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ calculate Normalized Getmansky Smoothing Index  }
\description{
Proposed by Getmansky et al to provide a normalized measure of liquidity risk.  The index will produces  a number from zero to one.  A low number indicates low liquidity risk.  A number trending towards one indicates a higher liquidity risk.
}
\usage{
SmoothingIndex(Ra, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Ra}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{neg.thetas}{ if FALSE, function removes negative coefficients (thetas) when calculating the index }
  \item{\dots}{ any other passthru parameters }
}
\value{
a value ranging from 0 to 1 (enforced only when neg.thetas = FALSE)
}
\details{
This function first creates a maximum likelihood estimation fit for a two-period MA process, using:
\code{arima(ra, order=c(0,0,2), method="ML", transform.pars=TRUE, include.mean=FALSE)}

\code{include.mean}: Getmansky, et al. JFE 2004 p 555 "By applying the above procedure
to observed de-meaned returns...", so we set that parameter to 'FALSE'.

\code{transform.pars}: ibid, "we impose the additional restriction that the estimated MA(k)
process be invertible," so we set the parameter to 'TRUE'.

From ibid, page 555: "Because of the scaling property Eq. (52) of the MA(k) likelihood function, a
simple procedure for obtaining estimates of our smoothing model with the
normalization Eq. (49) is to transform estimates (θ; sigma) from standard
MA(k) estimation packages such as SAS or RATS by dividing each θi by 1 + θ1 +
θ2 ... θk and multiplying sigma by the same factor. The likelihood function
remains unchanged but the transformed smoothing coefficients will now satisfy
Eq. (49)."

From the arima function above, MA2\$coef contains two coefficients, and no intercept value.
The calculation adjusts for that.

"Because θj ∈ [0, 1], ξ is also confined to the unit interval, and is minimized when all
the θj ’s are identical, which implies a value of 1/(k + 1) for ξ, and is maximized when
one coefficient is 1 and the rest are 0, in which case ξ = 1. In the context of smoothed
returns, a lower value of ξ implies more smoothing, and the upper bound of 1 implies no
smoothing, hence we shall refer to ξ as a 'smoothing index'."

The method as described in the paper, does not enforce
θj ∈ [0, 1], so ξ is not limited to that range either.  The \code{neg.thetas} attribute, when set to 'TRUE', drops negative values of θj and the result will remain within [0, 1].  

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