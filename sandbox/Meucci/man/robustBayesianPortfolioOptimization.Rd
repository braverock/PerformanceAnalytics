\name{robustBayesianPortfolioOptimization}
\alias{robustBayesianPortfolioOptimization}
\title{Construct a Bayesian mean-variance efficient frontier and identifies the most robust portfolio}
\usage{
  robustBayesianPortfolioOptimization(mean_post, cov_post,
    nu_post, riskAversionMu = 0.1, riskAversionSigma = 0.1,
    discretizations = 10, longonly = FALSE, volatility)
}
\arguments{
  \item{mean_post}{the posterior vector of means (after
  blending prior and sample data)}

  \item{cov_post}{the posterior covariance matrix (after
  blending prior and sample data)}

  \item{confidenceInPrior}{a numeric with the relative
  confidence in the prior vs. the sample data. A value of 2
  indicates twice as much weight to assign to the prior vs.
  the sample data. Must be greater than or equal to zero}

  \item{riskAversionMu}{risk aversion coefficient for
  estimation of means.}

  \item{riskAversionSigma}{risk aversion coefficient for
  estimation of Sigma.}

  \item{discretizations}{an integer with the number of
  portfolios to generate along efficient frontier (equally
  distanced in return space). Parameter must be an integer
  greater or equal to 1.}

  \item{volatility}{a numeric with the volatility used to
  calculate gamma-m. gamma-m acts as a constraint on the
  maximum volatility of the robust portfolio. A higher
  volatility means a higher volatile robust portfolio may
  be identified.}
}
\value{
  a list of portfolios along the frontier from least risky
  to most risky bayesianFrontier a list with portfolio
  along the Bayesian efficient frontier. Specifically:
  returns: the expected returns of each portfolo along the
  Bayesian efficient frontier volatility: the expected
  volatility of each portfolo along the Bayesian efficient
  frontier weights: the weights of each portfolo along the
  Bayesian efficient frontier robustPortfolio the most
  robust portfolio along the Bayesian efficient frontier.
  Specifically: returns: the expected returns of each
  portfolo along the Bayesian efficient frontier
  volatility: the expected volatility of each portfolo
  along the Bayesian efficient frontier weights: the
  weights of each portfolo along the Bayesian efficient
  frontier
}
\description{
  Construct a collection of portfolios along the Bayesian
  mean-variance efficient frontier where each portfolio is
  equally distanced in return space. The function also
  returns the most robust portfolio along the Bayesian
  efficient frontier
}
\author{
  Ram Ahluwalia \email{ram@wingedfootcapital.com}
}

