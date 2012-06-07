\name{Cumul2Raw}
\alias{Cumul2Raw}
\title{Transforms cumulants of Y-t into raw moments}
\usage{
  Cumul2Raw(ka)
}
\arguments{
  \item{ka}{cumulants of Y}
}
\value{
  mu_ the raw non-central moments of Y
}
\description{
  step 5 of the projection process: From the cumulants of Y
  we compute the raw non-central moments of Y We do so
  recursively by the identity in formula (24) which follows
  from applying (21) and re-arranging terms
}
\author{
  Ram Ahluwalia \email{rahluwalia@gmail.com}
}

