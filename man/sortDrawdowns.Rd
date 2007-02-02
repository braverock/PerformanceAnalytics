\name{sortDrawdowns}
\alias{sortDrawdowns}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sortDrawdowns(runs)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{runs}{ ~~Describe \code{runs} here~~ }
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
function (runs) {
    # sortDrawdowns(findDrawdowns(caduceus.ts@Data[,1]))
    # gives the drawdowns in order of worst to best

    for (i in 1:length(runs$return)) {
        for (j in 1:(length(runs$return)-1)) {
            if (runs$return[j] > runs$return[j+1]) {
                tempRet = runs$return[j]
                tempFor = runs$from[j]
                tempTo = runs$to[j]
                tempLen = runs$length[j]
                runs$return[j] = runs$return[j+1]
                runs$from[j] = runs$from[j+1]
                runs$to[j] = runs$to[j+1]
                runs$length[j] = runs$length[j+1]
                runs$return[j+1] = tempRet
                runs$from[j+1] = tempFor
                runs$to[j+1] = tempTo
                runs$length[j+1] = tempLen
            }
        }
    }
    runs
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
