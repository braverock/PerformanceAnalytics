\name{chart.Correlation.color}
\alias{chart.Correlation.color}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
chart.Correlation.color(R, new = FALSE, nrgcols = 50, labels = TRUE, labcols = 1, title = "", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ ~~Describe \code{R} here~~ }
  \item{new}{ ~~Describe \code{new} here~~ }
  \item{nrgcols}{ ~~Describe \code{nrgcols} here~~ }
  \item{labels}{ ~~Describe \code{labels} here~~ }
  \item{labcols}{ ~~Describe \code{labcols} here~~ }
  \item{title}{ ~~Describe \code{title} here~~ }
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
function (R, new = FALSE, nrgcols = 50, labels = TRUE, labcols = 1, title = "", ...)
{ # @author Sandrine Dudoit, sandrine@stat.berkeley.edu, from "SMA" library
  # @author modified by Peter Carl

    # Description:

    # Another, simpler, visualization of a Correlation Matrix, probably better
    # for larger sets of data.

    # Inputs:

    # R: data to be evaluated against its own members
    # new:
    # nrgcols:
    # labels:
    # labcols:
    # title:

    # Outputs:

    # A color graphic of a correlation matrix, with relative colors to indicate the
    # strength of the pairwise correlation.

    # @todo: accomodate larger labels for the columns

    rgcolors.func = function (n = 50)
    {
        k <- round(n/2)
        r <- c(rep(0, k), seq(0, 1, length = k))
        g <- c(rev(seq(0, 1, length = k)), rep(0, k))
        res <- rgb(r, g, rep(0, 2 * k))
        res
    }

    x = cor(R)

    n <- ncol(x)
    corr <- x
    if (new)
        corr <- cor.na(x)
    image(1:n, 1:n, corr[, n:1], col = rgcolors.func(nrgcols),
        axes = FALSE, xlab = "", ylab = "", ...)
    if (length(labcols) == 1) {
        axis(2, at = n:1, labels = labels, las = 2, cex.axis = 0.6,
            col.axis = labcols)
        axis(3, at = 1:n, labels = labels, las = 2, cex.axis = 0.6,
            col.axis = labcols)
    }
    if (length(labcols) == n) {
        cols <- unique(labcols)
        for (i in 1:length(cols)) {
            which <- (1:n)[labcols == cols[i]]
            axis(2, at = (n:1)[which], labels = labels[which],
                las = 2, cex.axis = 0.6, col.axis = cols[i])
            axis(3, at = which, labels = labels[which], las = 2,
                cex.axis = 0.6, col.axis = cols[i])
        }
    }
    mtext(title, side = 3, line = 3)
    box()
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
\keyword{ hplot }
