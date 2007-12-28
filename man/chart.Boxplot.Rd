\name{chart.Boxplot}
\alias{chart.Boxplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ box whiskers plot wrapper, with sensible defaults }
\description{
A wrapper to create box and whiskers plot, but with some sensible defaults useful for comparing distributions.
}
\usage{
chart.Boxplot(R, horizontal = TRUE, names = TRUE, as.Tufte = FALSE, sort.by = c(NULL, "mean", "median", "variance"), colorset = "black", symbol.color = "red", mean.symbol = 1, median.symbol = "|", outlier.symbol = 1, show.data = FALSE, darken = FALSE, add.mean = TRUE, sort.ascending = FALSE, xlab="Return", main = "Return Distribution Comparison",...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{horizontal}{ TRUE/FALSE plot horizontal (TRUE) or vertical (FALSE)  }
  \item{names}{ ~~Describe \\code{names} here~~ }
  \item{as.Tufte}{ ~~Describe \\code{as.Tufte} here~~ }
  \item{sort.by}{ ~~Describe \\code{sort.by} here~~ }
  \item{colorset}{ ~~Describe \\code{colorset} here~~ }
  \item{symbol.color}{ ~~Describe \\code{symbol.color} here~~ }
  \item{mean.symbol}{ ~~Describe \\code{mean.symbol} here~~ }
  \item{median.symbol}{ ~~Describe \\code{median.symbol} here~~ }
  \item{outlier.symbol}{ ~~Describe \\code{outlier.symbol} here~~ }
  \item{show.data}{ ~~Describe \\code{show.data} here~~ }
  \item{darken}{ ~~Describe \\code{darken} here~~ }
  \item{add.mean}{ ~~Describe \\code{add.mean} here~~ }
  \item{sort.ascending}{ ~~Describe \\code{sort.ascending} here~~ }
  \item{xlab}{ ~~Describe \\code{xlab} here~~ }
  \item{main}{ ~~Describe \\code{main} here~~ }
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
