\name{Return.annualized}
\alias{Return.annualized}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ calculate an annualized return for comparing instruments with different length history }
\description{
 An average annualized return is convenient for comparing returns.
}
\usage{
Return.annualized(R, scale = 12, geometric = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, or timeSeries of returns }
  \item{scale}{ number of periods in a year (daily scale = 252, monthly scale = 12, quarterly scale = 4) }
  \item{geometric}{ generate geometric (TRUE) or simple (FALSE) returns, default TRUE }
}
\value{
annualized return
}
\details{
Annualized returns are useful for comparing two assets.  To do so, you must scale your observations to an annual scale by raising the compound return to the number of periods in a year, and taking the root to the number of total observations:
    \deqn{prod(1+R)^{\frac{scale}{n}}-1=\sqrt[n]{prod(1+R)^{scale}}-1}{prod(1 + R)^(scale/n) - 1}

where scale is the number of periods in a year, and n is the total number of periods for which you have observations.

 @todo: add ability to calculate simple annualized returns to current geometric-only calculation

}
\references{
Bacon, Carl. \emph{Practical Portfolio Performance Measurement and Attribution}. Wiley. 2004. p. 6
}
\author{ Peter Carl }
\note{}
\seealso{
    \code{\link{Return.cumulative}},
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }