\name{CenteredMoments}
\alias{centeredcomoment}
\alias{centeredmoment}
\alias{Return.centered}
\alias{Ipower}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
centeredcomoment (Ra, Rb, p1, p2, normalize = FALSE)
centeredmoment (R, power)
Return.centered (R, ...)
Ipower (power, h)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ ~~Describe \code{R} here~~ }
  \item{Ra}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{Rb}{ a vector, matrix, data frame, timeSeries or zoo object of index, benchmark, portfolio, or secondary asset returns to compare against }
  \item{power}{ ~~Describe \code{power} here~~ }
  \item{p1}{ ~~Describe \code{p1} here~~ }
  \item{p2}{ ~~Describe \code{p2} here~~ }
  \item{h}{ ~~Describe \code{h} here~~ }
  \item{normalize}{ ~~Describe \code{normalize} here~~ }
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
function (Ra, Rb, p1, p2, normalize = FALSE)
{
    Ra = checkData(Ra)
    Rb = checkData(Rb)
    out = mean(na.omit(Return.centered(Ra)^p1 * Return.centered(Rb)^p2))
    if (normalize) {
        out = out/as.numeric(centeredmoment(Rb, power = (p1 +
            p2)))
    }
    return(out)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
