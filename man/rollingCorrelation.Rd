\name{rollingCorrelation}
\alias{rollingCorrelation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ rolling training period covariance/correlation }
\description{
This is a wrapper for providing n-period trailing correlations for the data provided.
}
\usage{
rollingCorrelation(Ra, Rj, width, trim = TRUE, na.rm = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Ra}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{Rj}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{width}{ the number of periods over which a function is to be calculated. Use the value zero (0) to roll the statistic from inception }
  \item{trim}{ TRUE/FALSE, whether to keep alignment caused by NA's }
  \item{na.rm}{ TRUE/FALSE Remove NA's from the returns? }
  \item{\dots}{ any other passthru parameters }
}
\details{

Example:
\code{head(rollingCorrelation(manager.ts@Data[,1],edhec.ts@Data,n=12))}
\preformatted{
           Convertible Arbitrage    CTA Global    Distressed
2003-11-28             0.2591101     0.2762218     0.7516556
2003-12-31             0.2162078     0.2477113     0.7452179
2004-01-30             0.3918575     0.3489062     0.7562063
2004-02-27             0.5331404     0.3905645     0.7088004
2004-03-31             0.5730389     0.3010877     0.5694478
2004-04-30             0.5146946     0.3762283     0.4374524}

}
\value{
A data.table of n-period trailing correlations for each column in y.
}
\references{ }
\author{ Peter Carl }
\note{
Inspired by fMultivar \code{\link[fMultivar]{rollFun}} written by Diethelm Wurtz.

Assumes that Ra and Rj are sequenced exactly the same and are regular.

Currently assumes that BOTH Ra and Rj are provided

@todo: Allow \emph{either} a matrix or data frame for 'Ra' \emph{or} give both 'Ra' and 'Rj' like \code{\link{cov}}
}
\seealso{
    \code{\link{cov}}
}
\examples{
#head(rollingCorrelation(manager.ts@Data[,1],edhec.ts@Data,n=12))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }