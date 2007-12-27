\name{chart.ECDF}
\alias{chart.ECDF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
chart.ECDF(R, main = "Empirical CDF", xlab = "x", ylab = "F(x)", colorset = c("black", "#005AFF"), lwd = 1, xlim = NULL, ylim = NULL, darken = FALSE, lty = c(1, 1), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ ~~Describe \code{R} here~~ }
  \item{main}{ ~~Describe \code{main} here~~ }
  \item{xlab}{ ~~Describe \code{xlab} here~~ }
  \item{ylab}{ ~~Describe \code{ylab} here~~ }
  \item{colorset}{ ~~Describe \code{colorset} here~~ }
  \item{lwd}{ ~~Describe \code{lwd} here~~ }
  \item{xlim}{ ~~Describe \code{xlim} here~~ }
  \item{ylim}{ ~~Describe \code{ylim} here~~ }
  \item{darken}{ ~~Describe \code{darken} here~~ }
  \item{lty}{ ~~Describe \code{lty} here~~ }
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
function (R, main = "Empirical CDF", xlab = "x", ylab = "F(x)", 
    colorset = c("black", "#005AFF"), lwd = 1, xlim = NULL, ylim = NULL, 
    darken = FALSE, lty = c(1, 1), ...) 
{
    R = checkData(R, method = "vector", na.rm = TRUE)
    if (darken) 
        elementcolor = "darkgray"
    else elementcolor = "lightgray"
    t = seq(-2.5, 2.5, length.out = 1000)
    p1 = pnorm(t, mean(R), sd(R))
    rx = sort(R)
    stepy = ((0:(length(R) - 1)) + 0.5)/length(R)
    plot(rx, stepy, type = "s", col = colorset[1], xlab = xlab, 
        ylab = ylab, main = main, axes = FALSE, lty = lty[1], 
        lwd = lwd, ...)
    lines(t, p1, lty = lty[2], col = colorset[2], lwd = lwd, 
        ...)
    axis(1, cex.axis = 0.8, col = elementcolor)
    axis(2, cex.axis = 0.8, col = elementcolor)
    box(col = elementcolor)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
