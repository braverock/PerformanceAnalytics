\name{moment.fourth}
\alias{moment.fourth}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ calculate the fourth mathematical moment of the return function }
\description{
The fourth mathematical moment of the return function.
Favre and Renaldo use this as separate from kurtosis in developing a four-moment CAPM model.
}
\usage{
moment.fourth(R, na.rm = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ return vector of the portfolio }
  \item{na.rm}{ TRUE/FALSE Remove NA's from the returns? }
}
\value{
fourth mathematical moment of the distribution
}
\references{
Favre, L. and Renaldo, A. How to Price Hedge Funds: From Two- to Four-Moment CAPM. October 2003. UBS and Edhec Business School.
}
\author{ Brian G. Peterson }
\seealso{
    \code{\link{moment.third}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }