\name{VaR.Beyond}
\alias{VaR.Beyond}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ calculate BVaR or loss Beyond traditional mean-VaR }
\description{
Beyond VaR purports to estimate average loss beyond VaR.  Please note that your milage will vary; expect that values obtained from the normal distribution differs radically from the real situation.

BeyondVaR is described in theoretical detail in the paper:
Gaussel, N., Legras, J., Longin, F., and Rabemananjara, R. "Beyond the VaR Horizon" 2001, Quants Review No. 37
}
\usage{
VaR.Beyond(R, periods = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, or timeSeries of returns }
  \item{periods}{ ~~Describe \code{periods} here~~ }
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
\references{
Gaussel, N., Legras, J., Longin, F., and Rabemananjara, R. "Beyond the VaR Horizon" 2001, Quants Review No. 37
}
\author{
Peter Carl
Brian G. Peterson
}
\seealso{
    \code{\link{VaR.CornishFisher}}
    \code{\link{VaR.traditional}}
    \code{\link{CVaR}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }