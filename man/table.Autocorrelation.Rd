\name{table.Autocorrelation}
\alias{table.Autocorrelation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
table.Autocorrelation(R, digits = 4)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ ~~Describe \code{R} here~~ }
  \item{digits}{ ~~Describe \code{digits} here~~ }
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
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (R, digits = 4)
{
    R = checkData(R, method = "zoo")
    columns = ncol(R)
    columnnames = colnames(R)
    for (column in 1:columns) {
        y = checkData(R[, column], method = "vector", na.rm = TRUE)
        acflag6 = acf(y, plot = FALSE, lag.max = 6)[[1]][2:7]
        LjungBox = Box.test(y, type = "Ljung-Box", lag = 6)
        values = c(acflag6, LjungBox$p.value)
        values = base::round(as.numeric(values), digits)
        if (column == 1) {
            result.df = data.frame(Value = values)
            colnames(result.df) = columnnames[column]
        }
        else {
            nextcol = data.frame(Value = values)
            colnames(nextcol) = columnnames[column]
            result.df = cbind(result.df, nextcol)
        }
    }
    rownames(result.df) = c("rho1", "rho2", "rho3", "rho4", "rho5",
        "rho6", "Q(6) p-value")
    result.df
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
