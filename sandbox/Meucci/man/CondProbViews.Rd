\name{CondProbViews}
\alias{CondProbViews}
\title{Input conditional views}
\usage{
  CondProbViews(View, X)
}
\arguments{
  \item{View}{TBD}

  \item{X}{TBD}
}
\value{
  A TBD

  b TBD

  g TBD
}
\description{
  statement: View(k).Who (e.g. [1 3])= View(k).Equal (e.g.
  {[2 3] [1 3 5]}) optional conditional statement:
  View(k).Cond_Who (e.g. [2])= View(k).Cond_Equal (e.g.
  {[1]}) amount of stress is quantified as Prob(statement)
  <= View(k).v if View(k).sgn = 1; Prob(statement) >=
  View(k).v if View(k).sgn = -1;
}
\details{
  confidence in stress is quantified in View(k).c in (0,1)
}
\author{
  Ram Ahluwalia \email{ram@wingedfootcapital.com}
}

