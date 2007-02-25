\name{table.MonthlyReturns}
\alias{table.MonthlyReturns}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
table.MonthlyReturns(R, ci = 0.95, firstcolumn = 1, digits = 4)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, or timeSeries of returns }
  \item{ci}{ ~~Describe \code{ci} here~~ }
  \item{firstcolumn}{ ~~Describe \code{firstcolumn} here~~ }
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (R, ci = 0.95, firstcolumn = 1, digits = 4)
{# @author Peter Carl

    # DESCRIPTION
    # Monthly Returns Summary: Statistics and Stylized Facts

    # Inputs:
    # R: Assumes returns rather than prices

    # Output:
    # Returns a basic set of statistics that match the period of the data passed
    # in (e.g., monthly returns will get monthly statistics)

    # FUNCTION:

    y = checkDataMatrix(R)

    # Set up dimensions and labels
    columns = ncol(y)
    rows = nrow(y)
    columnnames = colnames(y)
    rownames = rownames(y)

    cl.vals = function(x, ci) {
            x = x[!is.na(x)]
            n = length(x)
            if (n <= 1)
            return(c(NA, NA))
            se.mean = sqrt(var(x)/n)
            t.val = qt((1 - ci)/2, n - 1)
            mn = mean(x)
            lcl = mn + se.mean * t.val
            ucl = mn - se.mean * t.val
            c(lcl, ucl)
    }
# for each column, do the following:
    for(column in firstcolumn:columns) {
    x = as.vector(y[,column])
    x.length = length(x)
    x = x[!is.na(x)]
    x.na = x.length - length(x)
    z = c(length(x), x.na, min(x), as.numeric(quantile(x,
       prob = 0.25, na.rm = TRUE)),  median(x), mean(x), exp(mean(log(1+x)))-1,
       as.numeric(quantile(x, prob = 0.75, na.rm = TRUE)), max(x), sqrt(var(x)/length(x)),
       cl.vals(x, ci)[1], cl.vals(x, ci)[2], var(x), sqrt(var(x)),
       skewness(x), kurtosis(x))
    z = base::round(as.numeric(z),digits = digits)
    znames = c("Observations", "NAs", "Minimum", "Quartile 1", "Median", "Arithmetic Mean",
       "Geometric Mean", "Quartile 3", "Maximum", "SE Mean", paste("LCL Mean (",ci,")",sep=""),
       paste("UCL Mean (",ci,")",sep=""), "Variance", "Stdev", "Skewness", "Kurtosis")
        if(column == 1) {
            resultingtable = data.frame(Value = z, row.names = znames)
        }
        else {
            nextcolumn = data.frame(Value = z, row.names = znames)
            resultingtable = cbind(resultingtable, nextcolumn)
        }
    }
    colnames(resultingtable) = columnnames
    ans = resultingtable
    ans
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
