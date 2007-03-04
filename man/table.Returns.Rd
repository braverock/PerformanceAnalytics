\name{table.Returns}
\alias{table.Returns}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Monthly and Calendar year Return table }
\description{
Returns a table of returns formatted with years in rows, months in columns, and a total column in the last column.  For additional columns in \code{R}, annual returns will be appended as columns.

\preformatted{
      Jan   Feb   Mar   Apr  May   Jun   Jul   Aug   Sep   Oct   Nov   Dec    Year
2003 -1.05 -3.16 -1.07  7.99 8.97  1.49 -2.03 -0.79  4.32 -0.89 -1.21  3.05  15.83
2004  1.83 -0.70  2.45  0.36 0.13 -1.92  0.66 -1.74 -1.37 -0.08  3.90 -0.52   2.87
2005  2.00  1.28 -1.04 -0.18 1.64  1.91  4.24  1.92 -1.63  1.45 -0.35  0.00  11.68
2006  2.10    NA    NA    NA   NA    NA    NA    NA    NA    NA    NA    NA   2.10}

}
\usage{
table.Returns(R, column = 1, digits = 1, as.perc = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, or timeSeries of returns }
  \item{column}{ defaults to first column in a multi-column matrix, but you can pass a specific column to format }
  \item{digits}{ number of digits to round results to for presentation }
  \item{as.perc}{ TRUE/FALSE  if TRUE, multiply simple returns by 100 to get \% }
}
\value{
table of returns
}
\references{ }
\author{ Peter Carl }
\note{
This function assumes monthly returns, and does not currently have handling for other scales.

This function defaults to the first column as the returns to be formatted, but the function will format the column specified.
}
\seealso{ }
\examples{
data(edhec)
table.Returns(edhec)
t(table.Returns(edhec))
table.Returns(edhec[,c(13,2,4)])

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
