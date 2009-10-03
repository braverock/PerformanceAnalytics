\name{Return.annualized}
\alias{Return.annualized}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ calculate an annualized return for comparing instruments with different length history }
\description{
 An average annualized return is convenient for comparing returns.
}
\usage{
Return.annualized(R, scale = NA, geometric = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{scale}{ number of periods in a year (daily scale = 252, monthly scale = 12, quarterly scale = 4) }
  \item{geometric}{ generate geometric (TRUE) or simple (FALSE) returns, default TRUE }
}
\value{
annualized return
}
\details{
Annualized returns are useful for comparing two assets.  To do so, you must scale your observations to an annual scale by raising the compound return to the number of periods in a year, and taking the root to the number of total observations:
    \deqn{prod(1+R_{a})^{\frac{scale}{n}}-1=\sqrt[n]{prod(1+R_{a})^{scale}}-1}{prod(1 + Ra)^(scale/n) - 1}

where scale is the number of periods in a year, and n is the total number of periods for which you have observations.

For simple returns (geometric=FALSE), the formula is:

    \deqn{\overline{R_{a}} \cdot scale}{mean(R)*scale}

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
data(edhec)
Return.annualized(edhec[,13])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }