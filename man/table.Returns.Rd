\name{table.Returns}
\alias{table.Returns}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Monthly and Calendar year Return table }
\description{
Returns a table of returns formatted with years in rows, months in columns, and a total column in the last column, e.g., \cr

\tab   Jan   Feb   Mar   Apr  May   Jun   Jul   Aug   Sep   Oct   Nov   Dec \cr
2003 -1.05 -3.16 -1.07  7.99 8.97  1.49 -2.03 -0.79  4.32 -0.89 -1.21  3.05
}
\usage{
table.Returns(R, column = 1, digits = 1, as.perc = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, or timeSeries of returns }
  \item{column}{ defaults to first column in a multi-column matrix, but you can pass a specific column to format }
  \item{digits}{ number of digits to round results to }
  \item{as.perc}{ TRUE/FALSE  if TRUE, multiply simple returns by 100 to get \% }
}
\value{
table of returns
}
\references{ }
\author{ Peter Carl }
\note{
This function assumes monthly returns, and does not currently have handling for other scales.
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
    # assumes an input of ***monthly*** returns, such as:
    # > head(monthlyReturns.ts)
    #             Actual S&P500TR
    # 2002-12-31  0.0395  -0.0588
    # 2003-01-31 -0.0105  -0.0262
    # 2003-02-28 -0.0316  -0.0150
    # 2003-03-31 -0.0107   0.0097
    # 2003-04-30  0.0799   0.0824
    # 2003-05-30  0.0897   0.0527

    # This function defaults to the first column as the returns to be
    # formatted, but the function will format the column specified.

    # Outputs:
    # Returns a table of returns formatted with years in rows, months in
    # columns, and a total column in the last column, e.g.,

    #        Jan   Feb   Mar   Apr  May   Jun   Jul   Aug   Sep   Oct   Nov   Dec
    # 2002    NA    NA    NA    NA   NA    NA    NA    NA    NA    NA    NA  3.95
    # 2003 -1.05 -3.16 -1.07  7.99 8.97  1.49 -2.03 -0.79  4.32 -0.89 -1.21  3.05
    # 2004  1.83 -0.70  2.45  0.36 0.13 -1.92  0.66 -1.74 -1.37 -0.08  3.90 -0.52
    # 2005  2.00  1.28 -1.04 -0.18 1.64  1.91  4.24  1.92 -1.63  1.45 -0.35  0.00
    # 2006  2.10    NA    NA    NA   NA    NA    NA    NA    NA    NA    NA    NA
    #       Year
    # 2002  3.95
    # 2003 15.83
    # 2004  2.87
    # 2005 11.68
    # 2006  2.10


    # To plot the result to a graphics object:
    # require(gplot)
    # textplot(target.df, halign = "center", valign="center",cex=.8)
    # strangely, textplot won't fill in trailing zeros

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
