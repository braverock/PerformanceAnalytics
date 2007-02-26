\name{Return.annualized}
\alias{Return.annualized}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ calculate an annualized return for comparing instruments with different length history }
\description{
 An average annualized return is convenient for comparing returns.

 @todo: This function could be used for calculating geometric or simple returns
}
\usage{
Return.annualized(R, geometric = TRUE, scale = 12)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, or timeSeries of returns }
  \item{geometric}{ generate geometric (TRUE) or simple (FALSE) returns, default TRUE }
  \item{scale}{ number of periods in a year (daily scale = 252, monthly scale = 12, quarterly scale = 4) }
}
\value{
annualized return
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
