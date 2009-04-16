\name{textplot}
\alias{textplot}
\alias{textplot.default}
\alias{textplot.character}
\alias{textplot.matrix}
\alias{textplot.data.frame}

\title{Display text information in a graphics plot.}

\description{
  This function displays text output in a graphics window.  It is the
  equivalent of 'print' except that the output is displayed as a plot.
}
\usage{
textplot(object, halign="center", valign="center", cex, ...)
\method{textplot}{default}(object, halign=c("center","left","right"),
         valign=c("center", "top", "bottom"), cex, ... )
\method{textplot}{character}(object, halign = c("center", "left", "right"),
         valign = c("center", "top", "bottom"), cex, fixed.width=TRUE,
         cspace=1, lspace=1, mar=c(0, 0, 3, 0) + 0.1,
         tab.width = 8, ...)
\method{textplot}{data.frame}(object, halign = c("center", "left", "right"),
         valign = c("center", "top", "bottom"), cex, ...)
\method{textplot}{matrix}(object, halign = c("center", "left", "right"),
         valign = c("center", "top", "bottom"), cex, cmar = 2,
         rmar = 0.5, show.rownames = TRUE, show.colnames = TRUE,
         hadj = 1, vadj = 1, mar = c(1, 1, 4, 1) + 0.1,
         col.data = par("col"), col.rownames = par("col"), 
         col.colnames = par("col"), ...) 
}
\arguments{
  \item{object}{Object to be displayed.}
  \item{halign}{Alignment in the x direction, one of "center", "left",
    or "right". }
  \item{valign}{Alignment in the y direction, one of "center", "top" ,
    or "bottom"}
  \item{cex}{Character size, see \code{\link{par}} for details. If
    unset, the code will attempt to use the largest value which allows
    the entire object to be displayed.}
  \item{fixed.width}{Logical value indicating whether to emulate a
    fixed-width font by aligning characters in each row of text. This is
    usually necessary for text-formatted tables display properly.
    Defaults to 'TRUE'.}
  \item{cspace}{Space between characters as a
    multiple of the width of the letter 'W'. This only applies when 
    \code{fixed.width==TRUE}. }
  \item{lspace}{Line spacing. This only applies when
    \code{fixed.width==TRUE}.}
  \item{mar}{Figure margins, see the documentation for \code{par}.}
  \item{rmar, cmar}{Space between rows or columns, in
    fractions of the size of the letter 'M'.}
  \item{show.rownames, show.colnames}{Logical value indicating whether row or
    column names will be displayed.}
  \item{hadj,vadj}{Vertical and horizontal location of elements within
    matrix cells.  These have the same meaning as the \code{adj} graphics
    paramter (see \code{\link{par}}).}
  \item{col.data}{Colors for data elements.  If a single value is
    provided, all data elements will be the same color.  If a matrix
    matching the dimensions of the data is provided, each data element
    will receive the specified color.}
  \item{col.rownames, col.colnames}{Colors for row names and column
    names, respectively.  Either may be specified as a scalar or a
    vector of appropriate length.}
  \item{tab.width}{Width of a single tab stop, in characters}
  \item{\dots}{ Optional arguments passed to the text plotting command
    or specialied object methods}
}
\details{
  A new plot is created and the object is displayed
  using the largest font that will fit on in the plotting region.  The
  \code{halign} and \code{valign} parameters can be used to control the
  location of the string within the plotting region.

  For matrixes and vectors a specialized textplot function is available,
  which plots each of the cells individually, with column widths set
  according to the sizes of the column elements.  If present, row and
  column labels will be displayed in a bold font.  
}
\value{
  The character scaling factor (\code{cex}) used.
}
\author{Originally written by Gregory R. Warnes \email{warnes@bst.rochester.edu} for the package 'gplots', modified by Peter Carl}
\seealso{ \code{\link{plot}}, \code{\link{text}}, 
          \code{\link[utils]{capture.output}, \code{\link[gplots]{textplot}} 
        }

\examples{
\dontrun{
### simple examples
# show R version information
textplot(version)

# show the alphabet as a single string
textplot( paste(letters[1:26], collapse=" ") )

# show the alphabet as a matrix
textplot( matrix(letters[1:26], ncol=2))

### Make a nice 4 way display with two plots and two text summaries

data(iris)
par(mfrow=c(2,2))
plot( Sepal.Length ~ Species, data=iris, border="blue", col="cyan",
      main="Boxplot of Sepal Length by Species" )
plotmeans( Sepal.Length ~ Species, data=iris, barwidth=2, connect=FALSE,
           main="Means and 95\% Confidence Intervals\nof Sepal Length by Species")

info <- sapply( split(iris$Sepal.Length, iris$Species),
                function(x) round(c(Mean=mean(x), SD=sd(x), N=gdata::nobs(x)),2) )

textplot( info, valign="top"  )
title("Sepal Length by Species")

reg <- lm( Sepal.Length ~ Species, data=iris )
textplot( capture.output(summary(reg)), valign="top")
title("Regression of Sepal Length by Species")

par(mfrow=c(1,1))

### Show how to control text color
cols <- c("red", "green", "magenta", "forestgreen")
mat <- cbind(name=cols, t(col2rgb(cols)), hex=col2hex(cols))

textplot(mat,
         col.data=matrix(cols, nrow=length(cols), byrow=FALSE, ncol=5),
         )

### Show how to manually tune the character size
data(iris)
reg <- lm( Sepal.Length ~ Species, data=iris )
text <- capture.output(summary(reg))

# do the plot and capture the character size used
textplot(text, valign="top")

# see what size was used
cex

# now redo the plot at 80\% size
textplot( text, valign="top", cex=cex*0.80)



}
}
\keyword{hplot}
