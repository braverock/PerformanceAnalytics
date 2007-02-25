\name{table.MonthlyReturns}
\alias{table.MonthlyReturns}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Monthly Returns Summary: Statistics and Stylized Facts }
\description{
Returns a basic set of statistics that match the period of the data passed in (e.g., monthly returns will get monthly statistics)}
\usage{
table.MonthlyReturns(R, ci = 0.95, firstcolumn = 1, digits = 4)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, or timeSeries of returns }
  \item{ci}{ confidence interval, defaults to 95% }
  \item{firstcolumn}{ first column to use from multicolumn input data R }
  \item{digits}{ number of digits to round results to }
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
