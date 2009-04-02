`table.CaptureRatios` <-
function (Ra, Rb, digits = 4,...)
{# @author Peter Carl

    # FUNCTION:

    Ra = checkData(Ra)
    Rb = checkData(Rb)

    # Get dimensions and labels
    columns.a = ncol(Ra)
    columns.b = ncol(Rb)
    columnnames.a = colnames(Ra)
    columnnames.b = colnames(Rb)

    result.df = data.frame(NULL)

    # Calculate
    for(column.a in 1:columns.a) { # for each asset passed in as R
        # for(column.b in 1:columns.b) { # against each asset passed in as Rb
            row.df = data.frame(NULL)
            merged.assets = merge(Ra[,column.a,drop=FALSE], Rb[,1,drop=FALSE])
            merged.assets = na.omit(merged.assets) 

            UpCapture = UpDownRatios(Ra=merged.assets[,1], Rb=merged.assets[,2], method="capture", side="up")
            DnCapture = UpDownRatios(Ra=merged.assets[,1], Rb=merged.assets[,2], method="capture", side="down")

            row.df = cbind(UpCapture, DnCapture)
            rownames(row.df) = columnnames.a[column.a]
            result.df = rbind(result.df, row.df)
        # }
    }

    colnames(result.df) = c("Up Capture", "Down Capture")

    result.df = base::round(result.df, digits)
    result.df
}
