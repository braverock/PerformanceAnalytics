\name{hermitePolynomial}
\alias{hermitePolynomial}
\title{Generate a Hermite Polynomial of order n}
\usage{
  hermitePolynomial(n)
}
\arguments{
  \item{n}{A numeric defining the order of the Hermite
  Polynomial}
}
\value{
  p A vector containing the Hermite Polynomial of order n
  \deqn{ H_{j} (x) \equiv (-1)^{J} e^{ \frac{ -x^{2} }{2} }
  \frac{ d^{J} e^{ \frac{ -x^{2} }{2} }}{ dx^{J} } }
}
\description{
  Generate a Hermite Polynomial of order n
}
\author{
  Ram Ahluwalia \email{ram@wingedfootcapital.com}
}
\references{
  A. Meucci - "Fully Flexible Extreme Views" - See formula
  (20) \url{http://ssrn.com/abstract=1542083}
}

