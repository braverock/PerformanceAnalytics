\name{PanicCopula}
\alias{PanicCopula}
\title{Copula-Marginal Algorithm (CMA)}
\usage{
  PanicCopula(N = 20, J = 50000, r_c = 0.3, r = 0.99,
    b = 0.02, sig = 0.2, sigma)
}
\arguments{
  \item{N}{number of assets}

  \item{J}{number of samples from joint distribution of
  asset returns}

  \item{r_c}{average correlation in a calm market}

  \item{r}{average correlation in a panic market}

  \item{b}{probability of a panic market occurring}

  \item{sigma}{covariance matrix of asset returns}

  \item{sig}{TBD}
}
\value{
  a list with: copula a couplua hist a object of type
  histogram p_ the revised probabilities (invisible)
  meanReturn the mean return for the portfolio given the
  views varianceReturn the variance of the portfolio
  returns
}
\description{
  Copula-Marginal Algorithm (CMA) to generate and
  manipulate flexible copulas, as described in Meucci A.,
  "New Breed of Copulas for Risk and Portfolio Management",
  Risk, September 2011 Most recent version of article and
  code available at http://www.symmys.com/node/335
}
\examples{
PanicCopula( N = 20 , J = 50000 , r_c = .3 , r = .99 , b = .02 , sig = .2 , sigma )
}
\author{
  Ram Ahluwalia \email{rahluwalia@gmail.com}
}
\references{
  \url{http://www.symmys.com}
}

