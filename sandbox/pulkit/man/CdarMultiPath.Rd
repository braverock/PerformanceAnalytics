\name{CdarMultiPath}
\alias{CdarMultiPath}
\title{Conditional Drawdown at Risk for Multiple Sample Path}
\usage{
  CdarMultiPath(R, ps, sample, geometric = TRUE, p = 0.95,
    ...)
}
\arguments{
  \item{R}{an xts, vector, matrix,data frame, timeSeries or
  zoo object of multiple sample path returns}

  \item{ps}{the probability for each sample path}

  \item{scen}{the number of scenarios in the Return series}

  \item{instr}{the number of instruments in the Return
  series}

  \item{geometric}{utilize geometric chaining (TRUE) or
  simple/arithmetic chaining (FALSE) to aggregate returns,
  default TRUE}

  \item{p}{confidence level for calculation
  ,default(p=0.95)}

  \item{\dots}{any other passthru parameters}
}
\description{
  For a given \eqn{\alpha \epsilon [0,1]} in the multiple
  sample-paths setting,CDaR, denoted by
  \eqn{D_{\alpha}(w)}, is the average of
  \eqn{(1-\alpha).100\%} drawdowns of the set
  {d_st|t=1,....T,s = 1,....S}, and is defined by

  \deqn{D_\alpha(w) =
  \max_{{q_st}{\epsilon}Q}{\sum_{s=1}^S}{\sum_{t=1}^T}{p_s}{q_st}{d_st}},

  where

  \deqn{Q = \left\{ \left\{ q_st\right\}_{s,t=1}^{S,T} |
  \sum_{s = 1}^S \sum_{t = 1}^T{p_s}{q_st} = 1,
  0{\leq}q_st{\leq}\frac{1}{(1-\alpha)T}, s = 1....S, t =
  1.....T \right\}}

  For \eqn{\alpha = 1} , \eqn{D_\alpha(w)} is defined by
  (3) with the constraint
  \eqn{0{\leq}q_st{\leq}\frac{1}{(1-\alpha)T}}, in Q
  replaced by \eqn{q_st{\geq}0}

  As in the case of a single sample-path, the CDaR
  definition includes two special cases : (i) for
  \eqn{\alpha = 1},\eqn{D_1(w)} is the maximum drawdown,
  also called drawdown from peak-to-valley, and (ii) for
  \eqn{\alpha} = 0, \eqn{D_\alpha(w)} is the average
  drawdown
}
\references{
  Zabarankin, M., Pavlikov, K., and S. Uryasev. Capital
  Asset Pricing Model (CAPM) with Drawdown Measure.Research
  Report 2012-9, ISE Dept., University of Florida,
  September 2012
}

