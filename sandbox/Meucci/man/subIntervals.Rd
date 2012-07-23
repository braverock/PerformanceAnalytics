\name{subIntervals}
\alias{subIntervals}
\title{Generate the intervals containing jth point of the grid.}
\usage{
  subIntervals(x)
}
\arguments{
  \item{x}{a vector containing the scenarios}
}
\value{
  a list containing

  xLB a vector containing the lower bound of each interval

  xUB a vector containing the upper bound of each interval
  \deqn{ I_{j} \equiv \big[ x_{j} - \frac{x_{j} - x_{j-1}
  }{2}, x_{j} + \frac{x_{j+1} - x_{j}}{2} \big) }
}
\description{
  Generate the intervals containing jth point of the grid.
}
\author{
  Ram Ahluwalia \email{ram@wingedfootcapital.com}
}
\references{
  A. Meucci - "Fully Flexible Extreme Views" - See formula
  (17) \url{http://ssrn.com/abstract=1542083}
}

