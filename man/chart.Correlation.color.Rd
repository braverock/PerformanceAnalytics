\name{chart.Correlation.color}
\alias{chart.Correlation.color}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ correlation matrix chart, in color }
\description{
Another, simpler, visualization of a Correlation Matrix, probably better for larger sets of data.
}
\usage{
chart.Correlation.color(R, new = FALSE, nrgcols = 50, labels = TRUE, labcols = 1, title = "", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{new}{ ~~Describe \code{new} here~~ }
  \item{nrgcols}{ number or regularly spaced columns }
  \item{labels}{ TRUE/FALSE use labels or not }
  \item{labcols}{ labels to use }
  \item{title}{ title for the plot }
  \item{\dots}{ any other passthru parameters }
}
\value{
correlation chart
}
\references{ }
\author{ Peter Carl }
\note{
based on chart by Sandrine Dudoit, sandrine@stat.berkeley.edu, from "SMA" library
}
\seealso{
    \code{\link{table.Correlation}} \cr
    \code{\link{chart.Correlation}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
\keyword{ hplot }
