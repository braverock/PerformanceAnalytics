\name{chart.Boxplot}
\alias{chart.Boxplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ box whiskers plot wrapper, with sensible defaults }
\description{
A wrapper to create box and whiskers plot, but with some sensible defaults useful for comparing distributions.
}
\usage{
chart.Boxplot(R, horizontal = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, or timeSeries of returns }
  \item{horizontal}{ TRUE/FALSE plot horizontal (TRUE) or vertical (FALSE)  }
  \item{\dots}{ any other passthru parameters }
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
\references{ }
\author{ Peter Carl }
\note{ ~~further notes~~

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (R, horizontal = TRUE, ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # A wrapper to create box and whiskers plot, but with some sensible defaults useful for comparing distributions.

    # mar: a numerical vector of the form c(bottom, left, top, right) which
    # gives the number of lines of margin to be specified on the four sides
    # of the plot. The default is c(5, 4, 4, 2) + 0.1

    # This adjusts for longer labels on the left
    par(mar=c(5,10,4,2) + 0.1)

    boxplot(as.data.frame(R), horizontal = horizontal, las = 1, boxwex = .5, axis.cex = .8, text.cex = .5, ...)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
\keyword{ hplot }
