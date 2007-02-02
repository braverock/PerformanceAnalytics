\name{VaR.CornishFisher}
\alias{VaR.CornishFisher}
\alias{modifiedVaR}
\alias{VaR.traditional}
\alias{VaR.mean}
\alias{VaR.multicolumn}
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
VaR.CornishFisher(R, p = 0.99, modified = TRUE, column = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ ~~Describe \code{R} here~~ }
  \item{p}{ ~~Describe \code{p} here~~ }
  \item{modified}{ ~~Describe \code{modified} here~~ }
  \item{column}{ ~~Describe \code{column} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(R, p=0.99, modified = TRUE, column=1)
{   # @author Brian G. Peterson (completed/debugged fn)
    # @author Diethelm Wuertz (prototype function)


    # Description:

    # The limitations of mean Value-at-Risk are well covered in the literature.
    # Laurent Favre and Jose-Antonio Galeano published a paper in the
    # Fall 2002, volume 5 of the Journal of Alternative Investment,
    # "Mean-Modified Value-at-Risk optimization With Hedge Funds",
    # that proposed a modified VaR calculation that takes the higher moments
    # of non-normal distributions (skewness, kurtosis) into account, and
    # collapses to standard (traditional) mean-VaR if the return stream follows a
    # standard distribution.
    # This measure is now widely cited and used in the literature,
    # and is usually referred to as "Modified VaR" or "Modified Cornish-Fisher VaR"

    # Diethelm Wuertz's original function was called monthlyVaR, but did not
    # contain the required modifications to get to a monthly or an annualized number.
    # I have converted it to VaR.CornishFisher, and made the assumption of p=0.99, with an option for p=0.95 and
    # a collapse to normal mean VaR.

    # FUNCTION:

    # compute zc for the probability we want
    if ( p >= 0.51 ) {
        # looks like p was a percent like .99
        p = 1-p
    }
    zc = qnorm(p)

    # data type conditionals
    if (class(R) == "numeric") {
        y = as.matrix(R)
    }
    if (class(R) == "matrix") {
        y = R[, column]
    }
    if (class(R) == "data.frame") {
        y = R[, column]
    }
    if (class(R) == "timeSeries") {
        y = R@Data[, column]
    }
    if (class(R) == "vector") {
        y = array(R=R)
    }
    class(R)

    r = as.vector(y)
    if (!is.numeric(r)) stop("The selected column is not numeric")

    if (modified) {
        s = skewness(r) #skewness of the distribution
        k = kurtosis(r) #(excess) kurtosis
        Zcf = zc + (((zc^2-1)*s)/6) + (((zc^3-3*zc)*k)/24) + (((2*zc^3)-(5*zc)*s^2)/36)
        VaR = mean(r) - (Zcf * sqrt(var(r)))
    } else {
        # should probably add risk-free-rate skew here?
        VaR = mean(r) - (zc * sqrt(var(r)))
    }

    result = VaR

    # Return Value:
    result
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
