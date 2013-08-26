\name{table.Penance}
\alias{table.Penance}
\title{Table for displaying the Mximum Drawdown and the Time under Water}
\usage{
  table.Penance(R, confidence)
}
\arguments{
  \item{R}{Returns}

  \item{confidence}{the confidence interval}
}
\description{
  \code{table.Penance} Displays the table showing
  mean,Standard Deviation , phi, sigma , MaxDD,time at
  which MaxDD occurs, MaxTuW and the penance.For more
  details about MaxDD , Time under Water see code
  \code{MaxDD} and \code{TuW} respoectively.
}
\author{
  Pulkit Mehrotra
}
\references{
  Bailey, David H. and Lopez de Prado, Marcos,
  Drawdown-Based Stop-Outs and the "Triple Penance"
  Rule(January 1, 2013).
}
\seealso{
  \code{\link{chart.Penance}} \code{\link{MaxDD}}
  \code{\link{TuW}}
}

