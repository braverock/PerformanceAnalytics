\name{rollingRegression}
\alias{rollingRegression}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Rolling Regression on Returns }
\description{
calculates rolling regression statistics on a return stream.
}
\usage{
rollingRegression(formula, data, width, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{ regression formula for \code{\link{lm}} }
  \item{data}{ data set for \code{\link{lm}} }
  \item{width}{ size of the window over which to roll }
  \item{\dots}{ any other passthru parameters }
}
\value{
}
\references{ \url{http://www.mail-archive.com/r-help@stat.math.ethz.ch/msg19544.html} }
\author{ Douglas Bates }
\note{
This code was posted by Douglas Bates to the R-help mailing list in response to a question posted by Ajay Shah.
For the full discussion, see:
\url{http://www.mail-archive.com/r-help@stat.math.ethz.ch/msg19544.html}

}
\seealso{
    \code{\link{lm}} \cr
    \code{\link{apply}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }