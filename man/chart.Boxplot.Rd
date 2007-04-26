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
  \item{R}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{horizontal}{ TRUE/FALSE plot horizontal (TRUE) or vertical (FALSE)  }
  \item{\dots}{ any other passthru parameters }
}
\value{
Creates a box plot of returns with some defaults.
}
\references{ }
\author{ Peter Carl }
\seealso{
    \code{\link[graphics]{boxplot}}
}
\examples{
data(edhec)
chart.Boxplot(edhec)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
\keyword{ hplot }
