sortDrawdowns <-
function (runs) {
    # sortDrawdowns(findDrawdowns(caduceus.ts@Data[,1]))
    # gives the drawdowns in order of worst to best

    # modified with permission from function by Sankalp Upadhyay
    # <sankalp.upadhyay [at] gmail [dot] com>

    for (i in 1:length(runs$return)) {
        for (j in 1:(length(runs$return)-1)) {
            if (runs$return[j] > runs$return[j+1]) {
                tempRet = runs$return[j]
                tempFor = runs$from[j]
                tempTo = runs$to[j]
                tempLen = runs$length[j]
                tempPtt = runs$peaktotrough[j]
                tempTro = runs$trough[j]
                tempRec = runs$recovery[j]

                runs$return[j] = runs$return[j+1]
                runs$from[j] = runs$from[j+1]
                runs$to[j] = runs$to[j+1]
                runs$length[j] = runs$length[j+1]
                runs$peaktotrough[j] = runs$peaktotrough[j+1]
                runs$trough[j] = runs$trough[j+1]
                runs$recovery[j] = runs$recovery[j+1]

                runs$return[j+1] = tempRet
                runs$from[j+1] = tempFor
                runs$to[j+1] = tempTo
                runs$length[j+1] = tempLen
                runs$peaktotrough[j+1] = tempPtt
                runs$trough[j+1] = tempTro
                runs$recovery[j+1] = tempRec
            }
        }
    }
    runs
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2010 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################