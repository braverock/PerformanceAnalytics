\name{VaR.Marginal}
\alias{VaR.Marginal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
VaR.Marginal(R, p = 0.99, modified = TRUE, weightingvector)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ ~~Describe \code{R} here~~ }
  \item{p}{ ~~Describe \code{p} here~~ }
  \item{modified}{ ~~Describe \code{modified} here~~ }
  \item{weightingvector}{ ~~Describe \code{weightingvector} here~~ }
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
function(R, p=0.99, modified = TRUE, weightingvector)
{   # @author Brian G. Peterson

    # Description:

    # Function to implement Marginal VaR
    #
    # R                 Returns of your components
    # p                 probability to calculate VaR over
    # modified          use modified VaR or traditional VaR
    # weightingvector   to calculate portfolio returns with
    #
    # @returns data frame with total VaR of the portfolio plus Marginal VaR for each component

    # Setup
    # data type conditionals
    if (class(R) == "numeric") {
        R = as.matrix(R)
    }
    if (class(R) == "matrix") {
        R = R
    }
    if (class(R) == "data.frame") {
        R = R
    }
    if (class(R) == "timeSeries") {
        R = R@Data
    }
    if (class(R) == "vector") {
        R = array(R=R)
    }
    class(R)

    if (ncol(weightingvector) != ncol(R)) stop ("The Weighting Vector and Return Collection do not have the same number of Columns.")

    columns = ncol(R)
    columnnames=c("PortfolioVaR",colnames(R))

    # Function

    # first, get the numbers for the whole portfolio
    portfolioR   = pfolioReturn(R,weightingvector)
    portfolioVaR = VaR.CornishFisher(portfolioR,p,modified)
    pVaR = array (portfolioVaR)
    result=data.frame(pVaR=pVaR)

    for(column in 1:columns) {
        # calculate a multiplication factor, because the results don't seem to make sense
        # unless the weighting vector always equals the same sum
        weightfactor = sum(weightingvector)/sum(weightingvector[,-column])  # if we do need it
        # weightfactor = 1  # if we don't need it

        subportfolioR   = pfolioReturn(R[ ,-column],weightingvector[ ,-column]*weightfactor)
        subportfolioVaR = VaR.CornishFisher(subportfolioR,p,modified)

        marginalVaR = subportfolioVaR - portfolioVaR

        mVaR = array(marginalVaR)
        mVaR = data.frame(mVaR=mVaR)

        result=cbind(result,mVaR)
    } #end columns loop

    # check our result
    # this check would be used for Incremental/Component VaR, not Marginal VaR
    # if (portfolioVaR != sum(result[,-1])) warning (paste("The VaR of the portfolio ",portfolioVaR," does not match the sum of VaR.Marginal's ",sum(result[,-1])))

    colnames(result)<-columnnames

    # Return Value:
    result
  } # end function VaR.Marginal
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
