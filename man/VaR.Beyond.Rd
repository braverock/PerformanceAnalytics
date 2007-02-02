\name{VaR.Beyond}
\alias{VaR.Beyond}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
VaR.Beyond(R, periods = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ ~~Describe \code{R} here~~ }
  \item{periods}{ ~~Describe \code{periods} here~~ }
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
function (R, periods = 1)
{# @author Peter Carl
 # @author Brian G. Peterson (R-algorithm/testing)

    # Description:

    # Beyond VaR purports to estimate average loss beyond VaR.  Please note
    # that your milage will vary; expect that values obtained from the normal
    # distribution differs radically from the real situation.

    # BeyondVaR is described in theoretical detail in the paper:
    # as defined in:
    # Gaussel, N., Legras, J., Longin, F., and Rabemananjara, R.
    # "Beyond the VaR Horizon"
    # 2001, Quants Review No. 37

    # Assumes an input of regular period returns.


    # FUNCTION:
    if (class(R) == "numeric") {  #single column data

        BVaR = VaR(R) * periods^(1/(mean(R)-1))

        result = BVaR

    } else { #multi-column data

        if (class(R) == "timeSeries") {
            R = R@Data
        }
        if (class(R) == "vector") {
            R = array(R=R)
        }
        class(R)

        columns = ncol(R)
        columnnames=colnames(R)

        for(column in 1:columns) {
            r = as.vector(R[,column])
            if (!is.numeric(r)) stop("The selected column is not numeric")

            BVaR = VaR(r) * periods^(1/(mean(r)-1))

            BVaR=array(BVaR)
            if (column==1) {
                #create data.frame
                result=data.frame(BVaR=BVaR)
            } else {
                BVaR=data.frame(BVaR=BVaR)
                result=cbind(result,BVaR)
            }
        } #end columns loop

        colnames(result)<-columnnames

    } # end data type conditionals

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
