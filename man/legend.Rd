\name{legend}
\alias{PerformanceAnalytics.internal}
\alias{allsymbols}
\alias{bluefocus}
\alias{bluemono}
\alias{bond.dates}
\alias{bond.labels}
\alias{closedsymbols}
\alias{cycles.dates}
\alias{dark6equal}
\alias{dark8equal}
\alias{equity.dates}
\alias{equity.labels}
\alias{fillsymbols}
\alias{greenfocus}
\alias{greenmono}
\alias{grey6mono}
\alias{grey8mono}
\alias{legend}
\alias{linesymbols}
\alias{macro.dates}
\alias{macro.labels}
\alias{opensymbols}
\alias{rainbow10equal}
\alias{rainbow12equal}
\alias{rainbow6equal}
\alias{rainbow8equal}
\alias{redfocus}
\alias{redmono}
\alias{rich10equal}
\alias{rich12equal}
\alias{rich6equal}
\alias{rich8equal}
\alias{risk.dates}
\alias{risk.labels}
\alias{set6equal}
\alias{set8equal}
\alias{tim10equal}
\alias{tim12equal}
\alias{tim6equal}
\alias{tim8equal}
\title{internal functions for setting useful defaults for graphs}
\usage{
  legend(x, y = NULL, legend, fill = NULL,
    col = par("col"), lty, lwd, pch, angle = 45,
    density = NULL, bty = "o", bg = par("bg"), pt.bg = NA,
    cex = 1, pt.cex = cex, pt.lwd = lwd, xjust = 0,
    yjust = 1, x.intersp = 1, y.intersp = 1,
    adj = c(0, 0.5), text.width = NULL,
    text.col = par("col"), merge = do.lines && has.pch,
    trace = FALSE, plot = TRUE, ncol = 1, horiz = FALSE,
    title = NULL, inset = 0, border.col = NULL,
    border.lwd = 1, border.lty = "solid", box.col = NULL,
    box.lwd = 1, box.lty = "solid")
}
\arguments{
  \item{x,y}{the x and y co-ordinates to be used to
  position the legend.  They can be specified by keyword or
  in any way which is accepted by \code{\link{xy.coords}}:
  See Details.}

  \item{legend}{a character or \link{expression} vector.
  of length \eqn{\ge 1}{>= 1} to appear in the legend.}

  \item{fill}{if specified, this argument will cause boxes
  filled with the specified colors (or shaded in the
  specified colors) to appear beside the legend text.}

  \item{col}{the color of points or lines appearing in the
  legend.}

  \item{lty,lwd}{the line types and widths for lines
  appearing in the legend. One of these two \emph{must} be
  specified for line drawing.}

  \item{pch}{the plotting symbols appearing in the legend,
  either as vector of 1-character strings, or one (multi
  character) string.  \emph{Must} be specified for symbol
  drawing.}

  \item{angle}{angle of shading lines.}

  \item{density}{the density of shading lines, if numeric
  and positive. If \code{NULL} or negative or \code{NA}
  color filling is assumed.}

  \item{bty}{the type of box to be drawn around the legend.
  The allowed values are \code{"o"} (the default) and
  \code{"n"}.}

  \item{bg}{the background color for the legend box.  (Note
  that this is only used if \code{bty != "n"}.)}

  \item{box.lty,box.lwd}{the line type and width for the
  legend box.}

  \item{border.lty,border.lwd}{the line type and width for
  the legend border.}

  \item{pt.bg}{the background color for the
  \code{\link{points}}, corresponding to its argument
  \code{bg}.}

  \item{cex}{character expansion factor \bold{relative} to
  current \code{par("cex")}.}

  \item{pt.cex}{expansion factor(s) for the points.}

  \item{pt.lwd}{line width for the points, defaults to the
  one for lines, or if that is not set, to
  \code{par("lwd")}.}

  \item{xjust}{how the legend is to be justified relative
  to the legend x location.  A value of 0 means left
  justified, 0.5 means centered and 1 means right
  justified.}

  \item{yjust}{the same as \code{xjust} for the legend y
  location.}

  \item{x.intersp}{character interspacing factor for
  horizontal (x) spacing.}

  \item{y.intersp}{the same for vertical (y) line
  distances.}

  \item{adj}{numeric of length 1 or 2; the string
  adjustment for legend text. Useful for y-adjustment when
  \code{labels} are \link{plotmath} expressions.}

  \item{text.width}{the width of the legend text in x
  (\code{"user"}) coordinates.  (Should be positive even
  for a reversed x axis.)  Defaults to the proper value
  computed by \code{\link{strwidth}(legend)}.}

  \item{text.col}{the color used for the legend text.}

  \item{merge}{logical; if \code{TRUE}, \dQuote{merge}
  points and lines but not filled boxes.  Defaults to
  \code{TRUE} if there are points and lines.}

  \item{trace}{logical; if \code{TRUE}, shows how
  \code{legend} does all its magical computations.}

  \item{plot}{logical.  If \code{FALSE}, nothing is plotted
  but the sizes are returned.}

  \item{ncol}{the number of columns in which to set the
  legend items (default is 1, a vertical legend).}

  \item{horiz}{logical; if \code{TRUE}, set the legend
  horizontally rather than vertically (specifying
  \code{horiz} overrides the \code{ncol} specification).}

  \item{title}{a character string or length-one expression
  giving a title to be placed at the top of the legend.}

  \item{inset}{inset distance(s) from the margins as a
  fraction of the plot region when legend is placed by
  keyword.}
}
\description{
  Internal functions and data objects to make graphs easier
  to read, and better for print and presentation.
}
\details{
  Also contains common economic cycle dates and dates of
  serious market events per asset class.

  All items ending in .labels or .dates contain labels or
  dates that would be appropriate for specific asset
  classes or economic cycles.

  \code{legend} is a wrapper function for
  \code{\link[graphics]{legend}} to better handle placement
  and formatting of a legend for the charts

  all objects ending in symbol are symbol sets for line
  charts.
}
\seealso{
  \code{\link[graphics]{legend}}
}
\keyword{internal}

