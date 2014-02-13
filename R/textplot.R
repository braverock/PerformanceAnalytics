# # Original function from gplots package written by warnes
# $Id$

# Example using format.df as a pre-processor
# > textplot(format.df(t(y), na.blanks=F,cdec=c(3,3,1)), row.valign="center", wrap.rownames=20, wrap.colnames=10, cex=1)





#' Display text information in a graphics plot.
#' 
#' This function displays text output in a graphics window.  It is the
#' equivalent of 'print' except that the output is displayed as a plot.
#' 
#' A new plot is created and the object is displayed using the largest font
#' that will fit on in the plotting region.  The \code{halign} and
#' \code{valign} parameters can be used to control the location of the string
#' within the plotting region.
#' 
#' For matrixes and vectors a specialized textplot function is available, which
#' plots each of the cells individually, with column widths set according to
#' the sizes of the column elements.  If present, row and column labels will be
#' displayed in a bold font.
#' 
#' textplot also uses replaceTabs, a function to replace all tabs in a string
#' with an appropriate number of spaces.  That function was also written by
#' Gregory R. Warnes and included in the 'gplots' package.
#' 
#' @aliases textplot textplot.default textplot.character textplot.matrix
#' textplot.data.frame replaceTabs
#' @param object Object to be displayed.
#' @param halign Alignment in the x direction, one of "center", "left", or
#' "right".
#' @param valign Alignment in the y direction, one of "center", "top" , or
#' "bottom"
#' @param cex Character size, see \code{\link{par}} for details. If unset, the
#' code will attempt to use the largest value which allows the entire object to
#' be displayed.
#' @param mar Figure margins, see the documentation for \code{par}.
#' @param rmar,cmar Space between rows or columns, in fractions of the size of
#' the letter 'M'.
#' @param show.rownames,show.colnames Logical value indicating whether row or
#' column names will be displayed.
#' @param hadj,vadj Vertical and horizontal location of elements within matrix
#' cells.  These have the same meaning as the \code{adj} graphics paramter (see
#' \code{\link{par}}).
#' @param col.data Colors for data elements.  If a single value is provided,
#' all data elements will be the same color.  If a matrix matching the
#' dimensions of the data is provided, each data element will receive the
#' specified color.
#' @param col.rownames,col.colnames Colors for row names and column names,
#' respectively.  Either may be specified as a scalar or a vector of
#' appropriate length.
#' @param max.cex Sets the largest text size as a ceiling
#' @param row.valign Sets the vertical alignment of the row as "top", "bottom",
#' or (default) "center".
#' @param heading.valign Sets the vertical alignment of the heading as "top",
#' (default) "bottom", or "center".
#' @param wrap If TRUE (default), will wrap column names and rownames
#' @param wrap.colnames The number of characters after which column labels will
#' be wrapped.  Default is 10.
#' @param wrap.rownames The number of characters after which row headings will
#' be wrapped.  Default is 10.
#' @param text in the function 'replaceTabs', the text string to be processed
#' @param width in the function 'replaceTabs', the number of spaces to replace
#' tabs with
#' @param \dots Optional arguments passed to the text plotting command or
#' specialized object methods
#' @param fixed.width default is TRUE
#' @param cspace default is 1
#' @param lspace default is 1
#' @param tab.width default is 8
#' @author Originally written by Gregory R. Warnes
#' \email{warnes@@bst.rochester.edu} for the package 'gplots', modified by
#' Peter Carl
#' @seealso \code{\link{plot}}, \cr \code{\link{text}}, \cr
#' \code{\link[utils]{capture.output}}, \cr \code{\link[gplots]{textplot}}
#' @keywords hplot
#' @examples
#' 
#' # Also see the examples in the original gplots textplot function
#' data(managers)
#' textplot(table.AnnualizedReturns(managers[,1:6]))
#' 
#' # This was really nice before Hmisc messed up 'format' from R-base
#' # prettify with format.df in hmisc package
#' # require("Hmisc")
#'   result = t(table.CalendarReturns(managers[,1:8]))[-1:-12,]
#' 
#' #  textplot(Hmisc::format.df(result, na.blank=TRUE, numeric.dollar=FALSE, cdec=rep(1,dim(result)[2])), rmar = 0.8, cmar = 1,  max.cex=.9, halign = "center", valign = "top", row.valign="center", wrap.rownames=20, wrap.colnames=10, col.rownames=c("red", rep("darkgray",5), rep("orange",2)), mar = c(0,0,4,0)+0.1)
#' 
#' # title(main="Calendar Returns")
#'
#' @S3method textplot default 
#' @S3method textplot character
#' @S3method textplot data.frame
#' @S3method textplot matrix
#' 
#' @export
textplot <- function(object, halign="center", valign="center", cex, 
                            max.cex = 1, cmar=2, rmar=0.5,
                            show.rownames=TRUE, show.colnames=TRUE,
                            hadj=1, vadj=NULL,
                            row.valign="center",
                            heading.valign = "bottom",
                            mar= c(0,0,0,0)+0.1,
                            col.data=par("col"),
                            col.rownames=par("col"),
                            col.colnames=par("col"),
                            wrap = TRUE, 
                            wrap.colnames = 10, 
                            wrap.rownames = 10, ... )
  UseMethod('textplot')


#' @rdname textplot
#' @method textplot default
#' @export
textplot.default <- function(object,
                             halign=c("center","left","right"),
                             valign=c("center","top","bottom"),
                             cex,
                            max.cex, cmar, rmar,
                            show.rownames, show.colnames,
                            hadj, vadj,
                            row.valign,
                            heading.valign,
                            mar,
                            col.data,
                            col.rownames,
                            col.colnames,
                            wrap, 
                            wrap.colnames, 
                            wrap.rownames,... )
{

  if (is.matrix(object) || (is.vector(object) && length(object)>1) )
    return(textplot.matrix(object, halign, valign, cex, ... ))

  halign <- match.arg(halign)
  valign <- match.arg(valign)

  textplot.character(object, halign,  valign, cex, ...)
}


#' @rdname textplot
#' @method textplot data.frame
#' @export
textplot.data.frame <- function(object,
                             halign=c("center","left","right"),
                             valign=c("center","top","bottom"),
                             cex, 
                            max.cex = 1, cmar=2, rmar=0.5,
                            show.rownames=TRUE, show.colnames=TRUE,
                            hadj=1, vadj=NULL,
                            row.valign="center",
                            heading.valign = "bottom",
                            mar= c(0,0,0,0)+0.1,
                            col.data=par("col"),
                            col.rownames=par("col"),
                            col.colnames=par("col"),
                            wrap = TRUE, 
                            wrap.colnames = 10, 
                            wrap.rownames = 10, ... ){
    textplot.matrix(object, halign, valign, cex, 
                            max.cex, cmar, rmar,
                            show.rownames, show.colnames,
                            hadj, vadj,
                            row.valign,
                            heading.valign,
                            mar,
                            col.data,
                            col.rownames,
                            col.colnames,
                            wrap, 
                            wrap.colnames, 
                            wrap.rownames, ... )
}

#' @rdname textplot
#' @method textplot matrix
#' @export
textplot.matrix <- function(object,
                            halign=c("center","left","right"),
                            valign=c("center","top","bottom"),
                            cex, max.cex = 1, cmar=2, rmar=0.5,
                            show.rownames=TRUE, show.colnames=TRUE,
                            hadj=1, vadj=NULL,
                            row.valign="center",
                            heading.valign = "bottom",
                            mar= c(0,0,0,0)+0.1, # original settings: c(1,1,4,1)+0.1,
            # mar: a numerical vector of the form c(bottom, left, top, right) which
            # gives the number of lines of margin to be specified on the four sides
            # of the plot. The default is c(5, 4, 4, 2) + 0.1
                            col.data=par("col"),
                            col.rownames=par("col"),
                            col.colnames=par("col"),
                            wrap = TRUE, # fix this to add other methods
                            wrap.colnames = 10, # wrap after how many characters?
                            wrap.rownames = 10, # wrap after how many characters?
                            ... )
{
# @todo: add methods c("wrap", "abbreviate", "both") for handling long column and row names
# @todo: look at postrix package 'addtable2plot' for ideas about setting borders, text, and other formats by cell
  if(is.vector(object))
    object <- t(as.matrix(object))
  else
    object <- as.matrix(object)

  # check dimensions of col.data, col.rownames, col.colnames
  if(length(col.data)==1)
    col.data <- matrix(col.data, nrow=nrow(object), ncol=ncol(object))
  else
    if( nrow(col.data)!=nrow(object) || ncol(col.data)!=ncol(object) )
      stop("Dimensions of 'col.data' do not match dimensions of 'object'.")

  if(length(col.rownames)==1)
      col.rownames <- rep(col.rownames, nrow(object))      

  if(length(col.colnames)==1)
    if(show.rownames)
      col.colnames <- rep(col.colnames, ncol(object)+1)
    else
      col.colnames <- rep(col.colnames, ncol(object))
  
  halign=match.arg(halign)
  valign=match.arg(valign)

  opar <- par()[c("mar","xpd","cex")]
  on.exit( par(opar) )
  par(mar=mar, xpd=FALSE )

  # setup plot area
  plot.new()
  plot.window(xlim=c(0,1),ylim=c(0,1), log = "", asp=NA)



  # add 'r-style' row and column labels if not present
  if( is.null(colnames(object) ) )
    colnames(object) <- paste( "[,", 1:ncol(object), "]", sep="" )
  if( is.null(rownames(object)) )
    rownames(object) <- paste( "[", 1:nrow(object), ",]", sep="")


  # extend the matrix to include row and column labels
  if( show.rownames )
    {
      if(wrap) # wrap row labels
        row.names = sapply(rownames(object), function(x) paste(strwrap(x,wrap.rownames), collapse = "\n"), USE.NAMES=FALSE)
      else
        row.names = rownames(object)
      object <- cbind( row.names, object )
      col.data <- cbind( col.rownames, col.data )

    }
  if( show.colnames )
    {
      if(wrap) # wrap column labels
        column.names = sapply(colnames(object), function(x) paste(strwrap(x,wrap.colnames), collapse = "\n"), USE.NAMES=FALSE)
      else
        column.names = colnames(object)
      object <- rbind( column.names, object )
      col.data <- rbind( col.colnames, col.data )

    }

  # set the character size
  if( missing(cex) )
    {
      cex <- max.cex
      lastloop <- FALSE
    }
  else
    {
      lastloop <- TRUE
    }

  for (i in 1:20) # try to find the right cex to print at
    {
      oldcex <- cex

      colwidth = apply( object, 2, function(XX) max(strwidth(XX, cex=cex)) ) + strwidth("W",cex=cex) * cmar #(1 + cmar ) 
      width = sum(colwidth)

      rowheight = apply(object,1, function(X) max(strheight(X,cex=cex)) ) + strheight("(",cex=cex) * (1 + rmar )
      height=sum(rowheight)
      if(lastloop) break

      cex <- cex / max(width,height)

      if (abs(oldcex - cex) < 0.001)
        {
          lastloop <- TRUE
        }
    }
    # reset to maximum size if "discovered" size is too large
    if(cex>max.cex) {
        cex = max.cex

        colwidth = apply( object, 2, function(XX) max(strwidth(XX, cex=cex)) ) + strwidth("W",cex=cex) * cmar #(1 + cmar ) 
        width = sum(colwidth)

        rowheight = apply(object,1, function(X) max(strheight(X,cex=cex)) ) + strheight("(",cex=cex) * (1 + rmar )
        height=sum(rowheight)
    }
  # setup x alignment for the table
  if(halign=="left")
    xpos <- 0
  else if(halign=="center")
    xpos <- 0 + (1-width)/2
  else #if(halign=="right")
    xpos <- 0 + (1-width)

  # setup y alignment for the table
  if(valign=="top")
    ypos <- 1
  else if (valign=="center")
    ypos <- 1 - (1-height)/2
  else #if (valign=="bottom")
    ypos <- 0 + height

  x <- xpos
  y <- ypos

# @todo: apply hadj and vadj differently to headers, body; cell-by-cell control of alignment

# Uncomment these lines to see the dimensions of the box for the table, where rect(xleft, ybottom, xright, ytop)
# points(xpos,ypos)
# rect(xpos,ypos-height,xpos+width,ypos)

   xpos<-x
  for(i in 1:ncol(object)) {

    xpos <- xpos + colwidth[i]

# format the header separately here

    for(j in 1:nrow(object)) {
    # set new vertical alignment cases here.  This doesn't give cell-by-cell control.
    # will have to do col headings and rownames separately

      if( show.colnames && j==1 ){
        if (i==1 && j==1){} #brute force first cell to blank, better sol'n?
        else { # create the header
          if(heading.valign=="top") { # This works for valign "top" but not for "centered" or "bottom"
            ypos = y 
            vadj = 1
          } 
          if(heading.valign=="bottom") {
            ypos = y - rowheight[1] + strheight("(",cex=cex) * (1 + rmar)
            vadj = 0
          }
          if(heading.valign=="center") {
            ypos = y - rowheight[1]/2 + strheight("(",cex=cex) * (1 + rmar)/2
            vadj = .5
          }
          text(xpos, ypos, object[j,i], adj=c(hadj,vadj), cex=cex, font=2, col=col.data[j,i], ... )
        }
      }
      else {
        if(row.valign=="top") { 
            ypos = y - sum(rowheight[0:(j-1)]) 
            vadj = 1
        } 
        if(row.valign=="bottom") {
            ypos = y - sum(rowheight[1:(j)]) + strheight("(",cex=cex) * (1 + rmar)
            vadj = 0
        }
        if(row.valign=="center") {
            ypos = y - (sum(rowheight[1:(j)]) + sum(rowheight[0:(j-1)]))/2 + strheight("(",cex=cex) * (1 + rmar)/2
            vadj = .5
        }
        if(show.rownames && i==1)
            font = 2
        else
            font = 1
        text(xpos, ypos, object[j,i], adj=c(hadj,vadj), cex=cex, font=font, col=col.data[j,i], ... )
      }
    }

  }

  par(opar)
}

#' @rdname textplot
#' @method textplot character
#' @export
textplot.character <- function (object,
                                halign = c("center", "left", "right"),
                                valign = c("center", "top", "bottom"),
                                cex,
                            max.cex = 1, cmar=2, rmar=0.5,
                            show.rownames=TRUE, show.colnames=TRUE,
                            hadj=1, vadj=NULL,
                            row.valign="center",
                            heading.valign = "bottom",
                            mar= c(0,0,3,0)+0.1,
                            col.data=par("col"),
                            col.rownames=par("col"),
                            col.colnames=par("col"),
                            wrap = TRUE,
                            wrap.colnames = 10,
                            wrap.rownames = 10,
                                fixed.width=TRUE,
                                cspace=1,
                                lspace=1,
                                tab.width=8,
                                ...)
  {
    object <- paste(object,collapse="\n",sep="")
    object <- replaceTabs(object, width=tab.width)

    halign = match.arg(halign)
    valign = match.arg(valign)
    plot.new()

    opar <- par()[c("mar","xpd","cex","family")]
    on.exit( par(opar) )

    par(mar=mar,xpd=FALSE )
    if(fixed.width)
        par(family="mono")

    plot.window(xlim = c(0, 1), ylim = c(0, 1), log = "", asp = NA)

    slist   <- unlist(lapply(object, function(x) strsplit(x,'\n')))
    slist   <- lapply(slist, function(x) unlist(strsplit(x,'')))

    slen    <- sapply(slist, length)
    slines  <- length(slist)

    if (missing(cex))
      {
        lastloop <- FALSE
        cex <- 1
      }
    else
      lastloop <- TRUE


    for (i in 1:20)
      {
        oldcex <- cex
        #cat("cex=",cex,"\n")
        #cat("i=",i,"\n")
        #cat("calculating width...")
        cwidth  <- max(sapply(unlist(slist), strwidth,  cex=cex)) * cspace
        #cat("done.\n")
        #cat("calculating height...")
        cheight <- max(sapply(unlist(slist), strheight, cex=cex)) * ( lspace + 0.5 )
        #cat("done.\n")

        width <- strwidth(object, cex=cex)
        height <- strheight(object, cex=cex)

        if(lastloop) break

        cex <- cex  / max(width, height)

        if (abs(oldcex - cex) < 0.001)
          {
            lastloop <- TRUE
          }

      }

    if (halign == "left")
        xpos <- 0
    else if (halign == "center")
        xpos <- 0 + (1 - width)/2
    else xpos <- 0 + (1 - width)

    if (valign == "top")
        ypos <- 1
    else if (valign == "center")
        ypos <- 1 - (1 - height)/2
    else ypos <- 1 - (1 - height)

    text(x=xpos, y=ypos, labels=object, adj=c(0,1),
             cex=cex, ...)

    par(opar)
    invisible(cex)
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2014 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
