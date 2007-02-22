\name{moment.third}
\alias{moment.third}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ calculate the third mathematical moment of the return function }
\description{
The third mathematical moment of the return function.
Favre and Renaldo use this as separate from skewness in developing a four-moment CAPM model.
}
\usage{
moment.third(R, na.rm = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ return vector of the portfolio }
  \item{na.rm}{ TRUE/FALSE Remove NA's from the returns? }
}
\value{
third moment of the return series
}
\references{
Favre, L. and Renaldo, A., October 2003. How to Price Hedge Funds: From Two- to Four-Moment CAPM. UBS and Edhec Business School.
}
\author{ Brian G. Peterson }
\seealso{
    \code{\link{moment.fourth}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }