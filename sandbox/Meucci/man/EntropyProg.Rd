\name{EntropyProg}
\alias{EntropyProg}
\title{Entropy pooling program for blending views on scenarios with a prior scenario-probability distribution}
\usage{
  EntropyProg(p, A, b, Aeq, beq)
}
\arguments{
  \item{p}{a vector of initial probabilities based on prior
  (reference model, empirical distribution, etc.). Sum of
  'p' must be 1}

  \item{Aeq}{matrix consisting of equality constraints
  (paired with argument 'beq'). Denoted as 'H' in the
  Meucci paper. (denoted as 'H' in the "Meucci - Flexible
  Views Theory & Practice" paper formlua 86 on page 22)}

  \item{beq}{vector corresponding to the matrix of equality
  constraints (paired with argument 'Aeq'). Denoted as 'h'
  in the Meucci paper}

  \item{A}{matrix consisting of inequality constraints
  (paired with argument 'b'). Denoted as 'F' in the Meucci
  paper}

  \item{b}{vector consisting of inequality constraints
  (paired with matrix A). Denoted as 'f' in the Meucci
  paper}
}
\value{
  p_ revised probabilities based on entropy pooling
}
\description{
  Entropy program will change the initial predictive
  distribution 'p' to a new set 'p_' that satisfies
  specified moment conditions but changes other propoerties
  of the new distribution the least by minimizing the
  relative entropy between the two distributions.
  Theoretical note: Relative Entropy (Kullback-Leibler
  information criterion KLIC) is an asymmetric measure.
}
\details{
  We retrieve a new set of probabilities for the
  joint-scenarios using the Entropy pooling method Of the
  many choices of 'p' that satisfy the views, we choose 'p'
  that minimize the entropy or distance of the new
  probability distribution to the prior joint-scenario
  probabilities We use Kullback-Leibler divergence or
  relative entropy dist(p,q): Sum across all scenarios [
  p-t * ln( p-t / q-t ) ] Therefore we define solution as
  p* = argmin (choice of p ) [ sum across all scenarios:
  p-t * ln( p-t / q-t) ], such that 'p' satisfies views.
  The views modify the prior in a cohrent manner
  (minimizing distortion) We forumulate the stress tests of
  the baseline scenarios as linear constraints on yet-to-be
  defined probabilities Note that the numerical
  optimization acts on a very limited number of variables
  equal to the number of views. It does not act directly on
  the very large number of variables of interest, namely
  the probabilities of the Monte Carlo scenarios. This
  feature guarantees the numerical feasability of entropy
  optimization Note that new probabilities are generated in
  much the same way that the state-price density modifies
  objective probabilities of pay-offs to risk-neutral
  probabilities in contingent-claims asset pricing

  Compute posterior (=change of measure) with Entropy
  Pooling, as described in
}
\author{
  Ram Ahluwalia \email{ram@wingedfootcapital.com}
}
\references{
  A. Meucci - "Fully Flexible Views: Theory and Practice".
  See page 22 for illustration of numerical implementation
  Symmys site containing original MATLAB source code
  \url{http://www.symmys.com} NLOPT open-source
  optimization site containing background on algorithms
  \url{http://ab-initio.mit.edu/wiki/index.php/NLopt} We
  use the information-theoretic estimator of Kitamur and
  Stutzer (1997). Reversing 'p' and 'p_' leads to the
  empirical likelihood" estimator of Qin and Lawless
  (1994). See Robertson et al, "Forecasting Using Relative
  Entropy" (2002) for more theory
}

