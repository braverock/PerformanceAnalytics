\name{chart.RegressionDiagnostics}
\alias{chart.RegressionDiagnostics}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
chart.RegressionDiagnostics(R, Rb)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, or timeSeries of returns }
  \item{Rb}{ return vector of the benchmark asset }
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
\note{}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (R, Rb)
{ # @author Peter Carl

x = checkDataMatrix(R)
y = checkDataMatrix(Rb)

par(mfrow = c(3, 2), cex = 0.7)

chart.Histogram(x)
chart.QQPlot(x)
chart.Histogram(y)
chart.QQPlot(y)

chart.Boxplot(as.data.frame(cbind(x,y)))
plot(x~y)

  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
\keyword{ hplot }
