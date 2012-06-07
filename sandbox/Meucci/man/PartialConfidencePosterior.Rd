\name{PartialConfidencePosterior}
\alias{PartialConfidencePosterior}
\title{Constructs the partial confidence posterior based on a prior, sample mu/covariance, and relative confidence in the prior}
\usage{
  PartialConfidencePosterior(mean_sample, cov_sample,
    mean_prior, cov_prior, relativeConfidenceInMeanPrior,
    relativeConfidenceInCovPrior, sampleSize)
}
\arguments{
  \item{mean}{the mean of the sample returns}

  \item{cov}{the sample covariance matrix}

  \item{mean_prior}{the prior for the mean returns}

  \item{cov_prior}{the covariance matrix prior}

  \item{confidenceInMeanPrior}{a numeric with the relative
  confidence in the mean prior vs. the sample mean. A value
  of 2 indicates twice as much weight to assign to the
  prior vs. the sample data. Must be greater than or equal
  to zero}

  \item{confidenceInCovPrior}{a numeric with the relative
  confidence in the covariance prior vs. the sample
  covariance. A value of 2 indicates twice as much weight
  to assign to the prior vs. the sample data. Must be
  greater than or equal to zero}

  \item{sampleSize}{a numeric with the number of rows in
  the sample data used to estimate mean_sample and
  cov_sample}
}
\value{
  mean_post a vector with the confidence weighted posterior
  mean vector of asset returns blended from the prior and
  sample mean vector

  cov_post a covariance matrix the confidence weighted
  posterior covariance matrix of asset returns blended from
  the prior and sample covariance matrix

  time_post a numeric with ... TODO:

  nu_pst a numeric with ... TODO:
}
\description{
  Constructs the partial confidence posterior based on
  prior (mean vector and covariance matrix) and a posterior
  with a relative confidence in the prior vs. the sample
  data
}
\author{
  Ram Ahluwalia \email{ram@wingedfootcapital.com}
}

