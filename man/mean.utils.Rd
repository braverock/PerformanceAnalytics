\name{mean.utils}
\alias{mean.utils}
\alias{mean.geometric}
\alias{mean.UCL}
\alias{mean.LCL}
\alias{mean.stderr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ calculate attributes relative to the mean of the observation series given, including geometric, stderr, LCL and UCL }
\description{
    \tabular{ll}{
        \code{\link{mean.geometric}} \tab geometric mean \cr
        \code{\link{mean.stderr}} \tab standard error of the mean (S.E. mean) \cr
        \code{\link{mean.LCL}} \tab lower confidence level (LCL) of the mean \cr
        \code{\link{mean.UCL}} \tab upper confidence level (UCL) of the mean \cr
    }
}

\usage{
mean.geometric(x, ...)
mean.stderr(x, ...)
mean.UCL(x,  ci = 0.95, ...)
mean.LCL(x,  ci = 0.95, ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a vector, matrix, data frame, or time series to calculate the modified mean statistic over }
  \item{ci}{ the confidence interval to use }
  \item{\dots}{ any other passthru parameters }
}

\value{
numeric value for the mean function called
}
\author{ Peter Carl }
\seealso{
   \code{\link[stats]{sd}} \cr
   \code{\link[base]{mean}}
}

\examples{
data(edhec)
mean.geometric(edhec[,"Funds.of.Funds"])
mean.stderr(edhec[,"Funds.of.Funds"])
mean.UCL(edhec[,"Funds.of.Funds"])
mean.LCL(edhec[,"Funds.of.Funds"])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
