\name{chart.ACFplus}
\alias{chart.ACFplus}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
chart.ACFplus(R, maxlag = NULL, elementcolor = "gray", main = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ ~~Describe \code{R} here~~ }
  \item{maxlag}{ ~~Describe \code{maxlag} here~~ }
  \item{elementcolor}{ ~~Describe \code{elementcolor} here~~ }
  \item{main}{ ~~Describe \code{main} here~~ }
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
function (R, maxlag = NULL, elementcolor = "gray", main = NULL, 
    ...) 
{
    R = checkData(R)
    data = checkData(na.omit(R[, 1]), method = "vector")
    columns = ncol(R)
    rows = nrow(R)
    columnnames = colnames(R)
    if (is.null(main)) 
        main = columnnames[1]
    num = length(data)
    if (is.null(maxlag)) 
        maxlag = ceiling(10 + sqrt(num))
    ACF = acf(data, maxlag, plot = FALSE)$acf[-1]
    PACF = pacf(data, maxlag, plot = FALSE)$acf
    Lag = 1:length(ACF)/frequency(data)
    minA = min(ACF)
    minP = min(PACF)
    U = 2/sqrt(num)
    L = -U
    minu = min(minA, minP, L) - 0.01
    layout(rbind(1, 2))
    par(mar = c(0.5, 4, 4, 2) + 0.1)
    plot(Lag, ACF, type = "h", ylim = c(minu, 1), main = main, 
        axes = FALSE, ...)
    box(col = elementcolor)
    axis(2, col = elementcolor, cex.axis = 0.8)
    abline(h = c(0, L, U), lty = c(1, 2, 2), col = c(1, 4, 4))
    par(mar = c(4, 4, 0.5, 2) + 0.1)
    plot(Lag, PACF, type = "h", ylim = c(minu, 1), axes = FALSE, 
        ...)
    box(col = elementcolor)
    axis(1, col = elementcolor, cex.axis = 0.8)
    axis(2, col = elementcolor, cex.axis = 0.8)
    abline(h = c(0, L, U), lty = c(1, 2, 2), col = c(1, 4, 4))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
