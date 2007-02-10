\name{chart.RollingRegression}
\alias{chart.RollingRegression}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
chart.RollingRegression(R, Rb, n = 12, rf = 0, attribute = "Beta", main = paste("Rolling ", n, "-Month ", attribute, sep = ""), xaxis = TRUE, colorset = (1:12), legend.loc = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, or timeSeries of returns }
  \item{Rb}{ return vector of the benchmark asset }
  \item{n}{ ~~Describe \code{n} here~~ }
  \item{attribute}{ ~~Describe \code{attribute} here~~ }
  \item{main}{ ~~Describe \code{main} here~~ }
  \item{xaxis}{ ~~Describe \code{xaxis} here~~ }
  \item{colorset}{ color palette to use, set by default to rational choices }
  \item{legend.loc}{ ~~Describe \code{legend.loc} here~~ }
  \item{\dots}{ any other passthru parameters }
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
function (R, Rb, n, attribute = "Beta", main = paste("Rolling ",n,"-Month ",attribute,sep=""), xaxis = TRUE, colorset = (1:12), legend.loc = NULL, ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # A wrapper to create a chart of relative returns through time

    # Inputs:
    # R: a matrix, data frame, or timeSeries of returns
    # Rb: a matrix, data frame, or timeSeries of returns for a benchmark

    # Outputs:
    # A timeseries line chart of the calculated series

    # FUNCTION:

    # Transform input data to a data frame

    # @todo: These should be excess returns
    x = checkDataMatrix(R)
    y = checkDataMatrix(Rb)

    xcolname = colnames(x)
    colnames = colnames(y)

    columns = ncol(y)
    rows = nrow(x)
    rownames = rownames(x)

    data = as.data.frame(cbind(x[,1],y),row.names = rownames)

    if(attribute == "Beta")
        slot = 2
    if(attribute == "Alpha")
        slot = 1
    if(attribute == "R-Squared")
        slot = 2

    for(column in 2:(columns+1)) {
        result = rollingRegression(data[,1]~data[,column], data = data, width = n)
        if(column == 2) {
            if(attribute == "Beta")
                betas.df = data.frame(result$coefficients[2,],row.names = rownames[n:rows])
            if(attribute == "Alpha")
                betas.df = data.frame(result$coefficients[1,],row.names = rownames[n:rows])
            if(attribute == "R-Squared")
                betas.df = data.frame(result$r.squared,row.names = rownames[n:rows])
        }
        else {
            if(attribute == "Beta")
                nextcolumn = as.data.frame(result$coefficients[2,])
            if(attribute == "Alpha")
                nextcolumn = as.data.frame(result$coefficients[1,])
            if(attribute == "R-Squared")
                nextcolumn = as.data.frame(result$r.squared)
            betas.df = cbind(betas.df, nextcolumn)
        }

    }

    colnames(betas.df) = colnames

    chart.TimeSeries(betas.df, xaxis = xaxis, main = main, legend.loc = legend.loc, col = colorset, ...)

  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
\keyword{ hplot }
