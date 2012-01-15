# # Original function from gplots package written by warnes
# $Id$

# Example using format.df as a pre-processor
# > textplot(format.df(t(y), na.blanks=F,cdec=c(3,3,1)), row.valign="center", wrap.rownames=20, wrap.colnames=10, cex=1)

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
# Copyright (c) 2004-2012 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################