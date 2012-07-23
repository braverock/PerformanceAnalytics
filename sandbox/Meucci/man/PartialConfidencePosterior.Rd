\name{PartialConfidencePosterior}
\alias{PartialConfidencePosterior}
\title{Constructs the partial confidence posterior based on a prior, sample mu/covariance, and relative confidence in the prior}
\usage{
  PartialConfidencePosterior(mean_sample, cov_sample,
    mean_prior, cov_prior, relativeConfidenceInMeanPrior,
    relativeConfidenceInCovPrior, sampleSize)
}
\arguments{
  \item{mean_sample}{the mean of the sample returns}

  \item{cov_sample}{the sample covariance matrix}

  \item{mean_prior}{the prior for the mean returns}

  \item{cov_prior}{the covariance matrix prior}

  \item{relativeConfidenceInMeanPrior}{a numeric with the
  relative confidence in the mean prior vs. the sample
  mean. A value of 2 indicates twice as much weight to
  assign to the prior vs. the sample data. Must be greater
  than or equal to zero}

  \item{relativeConfidenceInCovPrior}{a numeric with the
  relative confidence in the covariance prior vs. the
  sample covariance. A value of 2 indicates twice as much
  weight to assign to the prior vs. the sample data. Must
  be greater than or equal to zero}

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

  time_post a numeric

  nu_pst a numeric
}
\description{
  Constructs the partial confidence posterior based on
  prior (mean vector and covariance matrix) and a posterior
  with a relative confidence in the prior vs. the sample
  data
}
\details{
  \deqn{ T_{1} \equiv T_{0} + T \\ \mu_{1} \equiv \frac{1}{
  T_{1} } \big( T_{0} \mu_{0} + T \hat{ \mu } \big) \\
  \nu_{1} \equiv \nu_{0} + T \\ \Sigma_{1} \equiv \big(
  \nu_{0} \Sigma_{0} + T \hat{ \Sigma } + \frac{
  \big(\mu_{0} - \hat{\mu} \big) \big(\mu_{0} - \hat{\mu}
  \big)' }{ \big( \frac{1}{T} + \frac{1}{T_{0} } \big) } }
}
\author{
  Ram Ahluwalia \email{ram@wingedfootcapital.com}
}
\references{
  A. Meucci - Robust Bayesian Allocation - See formula (11)
  - (14) \url{
  http://papers.ssrn.com/sol3/papers.cfm?abstract_id=681553
  }
}

