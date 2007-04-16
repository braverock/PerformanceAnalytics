\name{PerformanceAnalytics-internal}
\alias{legend}
\alias{bluefocus}
\alias{bluemono}
\alias{dark6equal}
\alias{dark8equal}
\alias{greenfocus}
\alias{greenmono}
\alias{grey6mono}
\alias{grey8mono}
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
\alias{set6equal}
\alias{set8equal}
\alias{tim10equal}
\alias{tim12equal}
\alias{tim6equal}
\alias{tim8equal}

%- Dates and labels for specific asset classes and events
\alias{bond.dates}
\alias{bond.labels}
\alias{cycles.dates}
\alias{equity.dates}
\alias{equity.labels}
\alias{macro.dates}
\alias{macro.labels}
\alias{risk.dates}
\alias{risk.labels}

%- Symbols for intersection points on line graphs
\alias{allsymbols}
\alias{closedsymbols}
\alias{fillsymbols}
\alias{linesymbols}
\alias{opensymbols}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{ internal functions for setting useful defaults for graphs }
\description{
Internal functions and data objects to make graphs easier to read, and better for print and presentation.

Also contains common economic cycle dates and dates of serious market events per asset class.
}
\usage{
legend(x, y = NULL, legend, fill = NULL, col = par("col"), lty, lwd, pch, angle = 45, density = NULL, bty = "o", bg = par("bg"), pt.bg = NA, cex = 1, pt.cex = cex, pt.lwd = lwd, xjust = 0, yjust = 1, x.intersp = 1, y.intersp = 1, adj = c(0, 0.5), text.width = NULL, text.col = par("col"), merge = do.lines && has.pch, trace = FALSE, plot = TRUE, ncol = 1, horiz = FALSE, title = NULL, inset = 0, border.col = NULL, border.lwd = 1, border.lty = "solid")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ ~~Describe \code{x} here~~ }
  \item{y}{ ~~Describe \code{y} here~~ }
  \item{legend}{ ~~Describe \code{legend} here~~ }
  \item{fill}{ ~~Describe \code{fill} here~~ }
  \item{col}{ ~~Describe \code{col} here~~ }
  \item{lty}{ ~~Describe \code{lty} here~~ }
  \item{lwd}{ ~~Describe \code{lwd} here~~ }
  \item{pch}{ ~~Describe \code{pch} here~~ }
  \item{angle}{ ~~Describe \code{angle} here~~ }
  \item{density}{ ~~Describe \code{density} here~~ }
  \item{bty}{ ~~Describe \code{bty} here~~ }
  \item{bg}{ ~~Describe \code{bg} here~~ }
  \item{pt.bg}{ ~~Describe \code{pt.bg} here~~ }
  \item{cex}{ ~~Describe \code{cex} here~~ }
  \item{pt.cex}{ ~~Describe \code{pt.cex} here~~ }
  \item{pt.lwd}{ ~~Describe \code{pt.lwd} here~~ }
  \item{xjust}{ ~~Describe \code{xjust} here~~ }
  \item{yjust}{ ~~Describe \code{yjust} here~~ }
  \item{x.intersp}{ ~~Describe \code{x.intersp} here~~ }
  \item{y.intersp}{ ~~Describe \code{y.intersp} here~~ }
  \item{adj}{ ~~Describe \code{adj} here~~ }
  \item{text.width}{ ~~Describe \code{text.width} here~~ }
  \item{text.col}{ ~~Describe \code{text.col} here~~ }
  \item{merge}{ ~~Describe \code{merge} here~~ }
  \item{trace}{ ~~Describe \code{trace} here~~ }
  \item{plot}{ ~~Describe \code{plot} here~~ }
  \item{ncol}{ ~~Describe \code{ncol} here~~ }
  \item{horiz}{ ~~Describe \code{horiz} here~~ }
  \item{title}{ ~~Describe \code{title} here~~ }
  \item{inset}{ ~~Describe \code{inset} here~~ }
  \item{border.col}{ ~~Describe \code{border.col} here~~ }
  \item{border.lwd}{ ~~Describe \code{border.lwd} here~~ }
  \item{border.lty}{ ~~Describe \code{border.lty} here~~ }
}
\details{
All items ending in .labels or .dates contain labels or dates that would be appropriate for specific asset classes or economic cycles.

\code{legend} is a wrapper function for \code{\link[graphics]{legend}} to better handle placement and formatting of a legend for the charts

all objects ending in symbol are symbol sets for line charts.

}
\seealso{
\code{\link[graphics]{legend}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ internal }
