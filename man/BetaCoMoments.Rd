\name{BetaCo-Moments}
\alias{BetaCoVariance}
\alias{BetaCoSkewness}
\alias{BetaCoKurtosis}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Functions to calculate systematic or beta co-moments of return series }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
BetaCoVariance(Ra, Ri, na.rm = FALSE)
BetaCoSkewness(Ra, Ri, na.rm = FALSE)
BetaCoKurtosis(Ra, Ri, na.rm = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Ra}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{Ri}{ a vector, matrix, data frame, timeSeries or zoo object of index, benchmark, or secondary asset returns to compare against }
  \item{na.rm}{ TRUE/FALSE Remove NA's from the returns? }
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


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }