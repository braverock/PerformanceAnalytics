\name{chart.Regression}
\alias{chart.Regression}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
chart.Regression(Ra, Rb, Rf, excess.returns = FALSE, reference.grid = TRUE, main = "Title", ylab = NULL, xlab = NULL, xlim = NA, colorset = 1:12, symbolset = 1:12, darken = FALSE, legend.loc = NULL, ylog = FALSE, fit = c("loess", "linear", "spline"), span = 2/3, degree = 1, family = c("symmetric", "gaussian"), ylim = NA, evaluation = 50, cex = 0.8, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Ra}{ a vector of returns to test, e.g., the asset to be examined }
  \item{Rb}{ a matrix, data.frame, or timeSeries of benchmark(s) to test the asset against. }
  \item{Rf}{ risk free rate, in same period as your returns }
  \item{excess.returns}{ ~~Describe \code{excess.returns} here~~ }
  \item{reference.grid}{ if true, draws a grid aligned with the points on the x and y axes }
  \item{main}{ set the chart title, same as in \code{\link{plot}} }
  \item{ylab}{ set the y-axis label, same as in \code{\link{plot}} }
  \item{xlab}{ set the x-axis label, same as in \code{\link{plot}} }
  \item{xlim}{ set the x-axis limit, same as in \code{\link{plot}} }
  \item{ylim}{ set the y-axis limit, same as in \code{\link{plot}} }
  \item{colorset}{ color palette to use, set by default to rational choices }
  \item{symbolset}{ symbol set to use, set by default to rational choices }
  \item{darken}{ if true, draws the chart elements in "darkgray" rather than "gray".  Makes it easier to print for some printers. }
  \item{legend.loc}{ places a legend into one of nine locations on the chart: bottomright, bottom, bottomleft, left, topleft, top, topright, right, or center. }
  \item{ylog}{ TRUE/FALSE set the y-axis to logarithmic scale, similar to \code{\link{plot}}, default FALSE }
  \item{fit}{ ~~Describe \code{fit} here~~ }
  \item{span}{ ~~Describe \code{span} here~~ }
  \item{degree}{ ~~Describe \code{degree} here~~ }
  \item{family}{ ~~Describe \code{family} here~~ }
  \item{evaluation}{ ~~Describe \code{evaluation} here~~ }
  \item{cex}{ ~~Describe \code{cex} here~~ }
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
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (Ra, Rb, Rf, excess.returns = FALSE, reference.grid = TRUE,
    main = "Title", ylab = NULL, xlab = NULL, xlim = NA, colorset = 1:12,
    symbolset = 1:12, darken = FALSE, legend.loc = NULL, ylog = FALSE,
    fit = c("loess", "linear", "spline"), span = 2/3, degree = 1,
    family = c("symmetric", "gaussian"), ylim = NA, evaluation = 50,
    cex = 0.8, ...)
{
    Ra = checkData(Ra, method = "zoo")
    Rb = checkData(Rb, method = "zoo")
    if (!is.null(dim(Rf))) {
        Rf = checkData(Rf, method = "zoo")
    }
    if (excess.returns) {
        Ra = Return.excess(Ra, Rf)
        Rb = Return.excess(Rb, Rf)
    }
    columns.a = ncol(Ra)
    columns.b = ncol(Rb)
    columnnames.a = colnames(Ra)
    columnnames.b = colnames(Rb)
    legendnames = NULL
    if (is.na(xlim[1]))
        xlim = range(as.vector(na.omit(Rb)))
    if (is.na(ylim[1]))
        ylim = range(as.vector(na.omit(Ra)))
    if (is.null(xlab)) {
        if (excess.returns)
            xlab = "Excess Return of Benchmarks"
        else xlab = "Return of Benchmarks"
    }
    if (is.null(ylab)) {
        if (excess.returns)
            ylab = "Excess Return of Assets"
        else ylab = "Return of Assets"
    }
    if (darken)
        elementcolor = "darkgray"
    else elementcolor = "lightgray"
    color.tic = 0
    for (column.a in 1:columns.a) {
        color.tic = color.tic + 1
        for (column.b in 1:columns.b) {
            merged.assets = merge(Ra[, column.a, drop = FALSE],
                Rb[, column.b, drop = FALSE])
            merged.assets.df = as.data.frame(na.omit(merged.assets))
            if (column.a == 1 & column.b == 1) {
                plot(merged.assets.df[, 2], merged.assets.df[,
                  1], col = colorset[color.tic], pch = symbolset[color.tic],
                  xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab,
                  main = main, axes = FALSE, cex = cex, ...)
            }
            else {
                plot.xy(merged.assets.df[, c(2, 1)], type = "p",
                  col = colorset[color.tic], pch = symbolset[color.tic],
                  cex = cex, ...)
            }
            for (s_fit in fit) {
                switch(s_fit, linear = {
                  abline(lm(merged.assets.df[, 1] ~ merged.assets.df[,
                    2]), col = colorset[color.tic])
                }, loess = {
                  r.loess = loess.smooth(merged.assets.df[, 2],
                    merged.assets.df[, 1], span, degree, family,
                    evaluation)
                  lines(r.loess, col = colorset[color.tic], lty = 2)
                }, )
            }
            legendnames = c(legendnames, paste(columnnames.a[column.a],
                columnnames.b[column.b], sep = " to "))
            if (column.b != columns.b)
                color.tic = color.tic + 1
        }
        if (reference.grid) {
            grid(col = elementcolor)
            abline(h = 0, col = elementcolor)
            abline(v = 0, col = elementcolor)
        }
        axis(1, col = elementcolor, ...)
        axis(2, col = elementcolor, ...)
        box(col = elementcolor)
        if (!is.null(legend.loc)) {
            legend(legend.loc, inset = 0.02, text.col = colorset,
                col = colorset, pch = symbolset, cex = cex, border.col = elementcolor,
                lwd = 1, bg = "white", legend = legendnames)
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
\keyword{ hplot }
