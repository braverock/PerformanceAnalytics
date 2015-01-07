## Function to replace all tabs in a string with an appropriate number of spaces.

#' @rdname textplot
replaceTabs.inner <-  function( text, width=8 )
{
    # handle a single character string
    spaces <- "        "

  if(nchar(text)<1) return(text)
  
  text.split <- strsplit(text,"\t")[[1]]
  if(length(text.split)==1)
    return(text)
  else
    {
      nSpaceAdd <- 8 - nchar(text.split) %% 8
      nSpaceAdd[length(nSpaceAdd)] <- 0
      nSpaceAdd[nSpaceAdd==8] <- 0
      
      retval <- ""
      for(i in 1:length(text.split))
        {
          tmp.text <- chartr("\t"," ", text.split[i]) # one space here
          retval <- paste(retval, tmp.text, substr(spaces,0,nSpaceAdd[i]-1 ), sep='' ) # rest here
        }
      return(retval)
    }
}

#' @rdname textplot
replaceTabs <- function(text, width=8)
{
  text <- as.character(text)
  retval <- sapply(text, replaceTabs.inner) 
  names(retval) <- names(text)
  retval
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2015 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################