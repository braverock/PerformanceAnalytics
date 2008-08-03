\name{MultivariateMoments}
\alias{MultivariateMoments}
\alias{CoSkewnessMatrix}
\alias{CoKurtosisMatrix}
\alias{M3.MM}
\alias{M4.MM}
\alias{multivariate_mean}
\alias{StdDev.MM}
\alias{skewness.MM}
\alias{kurtosis.MM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ functions to compute multivariate moments of a joint distribution }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
M3.MM(R)
M4.MM(R)
StdDev.MM(w, sigma)
skewness.MM(w, sigma, M3)
kurtosis.MM(w, sigma, M4)
CoSkewnessMatrix(R, ...)
CoKurtosisMatrix(R, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ ~~Describe \code{R} here~~ }
  \item{w}{ ~~Describe \code{w} here~~ }
%  \item{mu}{ ~~Describe \code{mu} here~~ }
  \item{sigma}{ ~~Describe \code{sigma} here~~ }
  \item{M3}{ ~~Describe \code{M3} here~~ }
  \item{M4}{ ~~Describe \code{M4} here~~ }
  \item{\dots}{ ~~Describe \code{\dots} here~~ }
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
function (R, ...)
{
    return(M3.MM(R))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
