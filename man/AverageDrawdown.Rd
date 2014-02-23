\name{AverageDrawdown}
\alias{AverageDrawdown}
\alias{AverageRecovery}
\title{Calculates the average of the observed drawdowns.}
\usage{
AverageDrawdown(R, ...)

AverageRecovery(R, ...)
}
\arguments{
  \item{R}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of asset returns}

  \item{\dots}{any other passthru parameters}
}
\description{
ADD = abs(sum[j=1,2,...,d](D_j/d)) where D'_j = jth
drawdown over entire period d = total number of drawdowns
in the entire period
}

