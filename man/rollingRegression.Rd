\name{rollingRegression}
\alias{rollingRegression}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
This code was posted by Douglas Bates to the R-help mailing list in response to a question posted by Ajay Shah.
For the full discussion, see:
http://www.mail-archive.com/r-help@stat.math.ethz.ch/msg19544.html
}
\usage{
rollingRegression(formula, data, width, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{ ~~Describe \code{formula} here~~ }
  \item{data}{ ~~Describe \code{data} here~~ }
  \item{width}{ ~~Describe \code{width} here~~ }
  \item{\dots}{ any other passthru parameters }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ http://www.mail-archive.com/r-help@stat.math.ethz.ch/msg19544.html }
\author{ Douglas Bates }
\note{ ~~further notes~~

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
    rollingRegression <-
    function(formula, data, width, ...)
    {# @author Douglas Bates

        # DESCRIPTION:
        # This code was posted to the R-help mailing list in response to a question
        # posted by Ajay Shah.  For the full discussion, see:
        # http://www.mail-archive.com/r-help@stat.math.ethz.ch/msg19544.html

        # @todo: make inputs consistant with other functions

        mCall = match.call()
        mCall$width = NULL
        mCall[[1]] = as.name("lm")
        mCall$x = mCall$y = TRUE # now mCall contains lm(y = TRUE, x = TRUE)
        bigfit = eval(mCall, parent.frame())
        ncoef = length(coef(bigfit))
        nr = nrow(data)
        width = as.integer(width)[1]
        stopifnot(width >= ncoef, width <= nr)
        y = bigfit$y
        x = bigfit$x
        terms = bigfit$terms
        inds = embed(seq(nr), width)[, rev(seq(width))]
        sumrys <- lapply(seq(nrow(inds)),
            function(st) {
                ind = inds[st,]
                fit = lm.fit(x[ind, , drop = FALSE], y[ind])
                fit$terms = terms
                class(fit) = "lm"
                summary(fit)
            })
        list(coefficients = sapply(sumrys, function(sm) coef(sm)[,"Estimate"]),
            Std.Error = sapply(sumrys, function(sm) coef(sm)[,"Std. Error"]),
            sigma = sapply(sumrys, "[[", "sigma"),
            r.squared = sapply(sumrys, "[[", "r.squared"))
    }

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
