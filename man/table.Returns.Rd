\name{table.Returns}
\alias{table.Returns}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Monthly and Calendar year Return table }
\description{
Returns a table of returns formatted with years in rows, months in columns, and a total column in the last column.  For additional columns in \code{R}, annual returns will be appended as columns.

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
