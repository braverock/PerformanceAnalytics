#' internal functions for setting useful defaults for graphs
#' 
#' Internal functions and data objects to make graphs easier to read, and
#' better for print and presentation.
#' 
#' Also contains common economic cycle dates and dates of serious market events
#' per asset class.
#' 
#' All items ending in .labels or .dates contain labels or dates that would be
#' appropriate for specific asset classes or economic cycles.
#' 
#' \code{legend} is a wrapper function for \code{\link[graphics]{legend}} to
#' better handle placement and formatting of a legend for the charts
#' 
#' all objects ending in symbol are symbol sets for line charts.
#' 
#' @aliases PerformanceAnalytics.internal legend bluefocus bluemono dark6equal
#' dark8equal greenfocus greenmono grey6mono grey8mono rainbow10equal
#' rainbow12equal rainbow6equal rainbow8equal redfocus redmono rich10equal
#' rich12equal rich6equal rich8equal set6equal set8equal tim10equal tim12equal
#' tim6equal tim8equal bond.dates bond.labels cycles.dates equity.dates
#' equity.labels macro.dates macro.labels risk.dates risk.labels allsymbols
#' closedsymbols fillsymbols linesymbols opensymbols tol1qualitative tol2qualitative tol3qualitative
#' tol4qualitative tol5qualitative tol6qualitative tol7qualitative tol8qualitative tol9qualitative
#' tol10qualitative tol11qualitative tol12qualitative
#' tol14rainbow tol15rainbow tol18rainbow tol21rainbow
#' @param x,y the x and y co-ordinates to be used to position the legend.  They
#' can be specified by keyword or in any way which is accepted by
#' \code{\link{xy.coords}}: See Details.
#' @param legend a character or \link{expression} vector.  of length \eqn{\ge
#' 1}{>= 1} to appear in the legend.
#' @param fill if specified, this argument will cause boxes filled with the
#' specified colors (or shaded in the specified colors) to appear beside the
#' legend text.
#' @param col the color of points or lines appearing in the legend.
#' @param lty,lwd the line types and widths for lines appearing in the legend.
#' One of these two \emph{must} be specified for line drawing.
#' @param pch the plotting symbols appearing in the legend, either as vector of
#' 1-character strings, or one (multi character) string.  \emph{Must} be
#' specified for symbol drawing.
#' @param angle angle of shading lines.
#' @param density the density of shading lines, if numeric and positive. If
#' \code{NULL} or negative or \code{NA} color filling is assumed.
#' @param bty the type of box to be drawn around the legend.  The allowed
#' values are \code{"o"} (the default) and \code{"n"}.
#' @param bg the background color for the legend box.  (Note that this is only
#' used if \code{bty != "n"}.)
#' @param box.lty,box.lwd the line type and width for the legend box.
#' @param border.lty,border.lwd the line type and width for the legend border.
#' @param pt.bg the background color for the \code{\link{points}},
#' corresponding to its argument \code{bg}.
#' @param cex character expansion factor \bold{relative} to current
#' \code{par("cex")}.
#' @param pt.cex expansion factor(s) for the points.
#' @param pt.lwd line width for the points, defaults to the one for lines, or
#' if that is not set, to \code{par("lwd")}.
#' @param xjust how the legend is to be justified relative to the legend x
#' location.  A value of 0 means left justified, 0.5 means centered and 1 means
#' right justified.
#' @param yjust the same as \code{xjust} for the legend y location.
#' @param x.intersp character interspacing factor for horizontal (x) spacing.
#' @param y.intersp the same for vertical (y) line distances.
#' @param adj numeric of length 1 or 2; the string adjustment for legend text.
#' Useful for y-adjustment when \code{labels} are \link{plotmath} expressions.
#' @param text.width the width of the legend text in x (\code{"user"})
#' coordinates.  (Should be positive even for a reversed x axis.)  Defaults to
#' the proper value computed by \code{\link{strwidth}(legend)}.
#' @param text.col the color used for the legend text.
#' @param merge logical; if \code{TRUE}, \dQuote{merge} points and lines but
#' not filled boxes.  Defaults to \code{TRUE} if there are points and lines.
#' @param trace logical; if \code{TRUE}, shows how \code{legend} does all its
#' magical computations.
#' @param plot logical.  If \code{FALSE}, nothing is plotted but the sizes are
#' returned.
#' @param ncol the number of columns in which to set the legend items (default
#' is 1, a vertical legend).
#' @param horiz logical; if \code{TRUE}, set the legend horizontally rather
#' than vertically (specifying \code{horiz} overrides the \code{ncol}
#' specification).
#' @param title a character string or length-one expression giving a title to
#' be placed at the top of the legend.
#' @param inset inset distance(s) from the margins as a fraction of the plot
#' region when legend is placed by keyword.
#' @seealso \code{\link[graphics]{legend}}
#' @keywords internal
#' @export 
#' @export allsymbols bluefocus bluemono bond.dates bond.labels closedsymbols cycles.dates 
#' @export dark6equal dark8equal equity.dates equity.labels fillsymbols greenfocus greenmono grey6mono
#' @export grey8mono legend linesymbols macro.dates macro.labels opensymbols rainbow10equal 
#' @export rainbow12equal rainbow6equal rainbow8equal redfocus redmono rich10equal rich12equal rich6equal
#' @export rich8equal risk.dates risk.labels set6equal set8equal tim10equal tim12equal tim6equal tim8equal
#' @export tol1qualitative tol2qualitative tol3qualitative tol4qualitative tol5qualitative tol6qualitative tol7qualitative tol8qualitative tol9qualitative tol10qualitative tol11qualitative tol12qualitative
#' @export tol14rainbow tol15rainbow tol18rainbow tol21rainbow
legend <-
function (x, y = NULL, legend, fill = NULL, col = par("col"),
    lty, lwd, pch, angle = 45, density = NULL, bty = "o", bg = par("bg"),
    pt.bg = NA, cex = 1, pt.cex = cex, pt.lwd = lwd, xjust = 0,
    yjust = 1, x.intersp = 1, y.intersp = 1, adj = c(0, 0.5),
    text.width = NULL, text.col = par("col"), merge = do.lines &&
        has.pch, trace = FALSE, plot = TRUE, ncol = 1, horiz = FALSE,
    title = NULL, inset = 0, border.col = NULL, border.lwd = 1, border.lty = "solid", box.col = NULL, box.lwd = 1, box.lty = "solid")
{
    # Modifications to core graphics legend() function
    # @author R Core Dev Team
    # @author modifications Peter Carl

    # Minor modifications to the function include:
    # - added border.col so that the legend border could be colored
    # - added border.lwd to change the line width of the border
    # - added border.lty to change the line type for the border
    # - changed line segment end to a more squared type

    # > plot.new()
    # > par(mar = c(0, 0, 0, 0))
    # > legend("center",text.col=rainbow6equal, cex = .8, ncol=3, border.col = "grey",legend = colnames(data))

    if (missing(legend) && !missing(y) && (is.character(y) ||
        is.expression(y))) {
        legend <- y
        y <- NULL
    }
    mfill <- !missing(fill) || !missing(density)
    if (length(title) > 1)
        stop("invalid title")
    n.leg <- if (is.call(legend))
        1
    else length(legend)
    if (n.leg == 0)
        stop("'legend' is of length 0")
    auto <- if (is.character(x))
        match.arg(x, c("bottomright", "bottom", "bottomleft",
            "left", "topleft", "top", "topright", "right", "center"))
    else NA
    if (is.na(auto)) {
        xy <- xy.coords(x, y)
        x <- xy$x
        y <- xy$y
        nx <- length(x)
        if (nx < 1 || nx > 2)
            stop("invalid coordinate lengths")
    }
    else nx <- 0
    xlog <- par("xlog")
    ylog <- par("ylog")
    rect2 <- function(left, top, dx, dy, density = NULL, angle, border = border.col, lty = border.lty, lwd = border.lwd, ...) {
        r <- left + dx
        if (xlog) {
            left <- 10^left
            r <- 10^r
        }
        b <- top - dy
        if (ylog) {
            top <- 10^top
            b <- 10^b
        }
        rect(left, top, r, b, angle = angle, density = density, border = border, lty = lty, lwd = lwd, ...)
    }
    segments2 <- function(x1, y1, dx, dy, ...) {
        x2 <- x1 + dx
        if (xlog) {
            x1 <- 10^x1
            x2 <- 10^x2
        }
        y2 <- y1 + dy
        if (ylog) {
            y1 <- 10^y1
            y2 <- 10^y2
        }
        segments(x1, y1, x2, y2, lend="butt", ...) # added squared end to line seg
    }
    points2 <- function(x, y, ...) {
        if (xlog)
            x <- 10^x
        if (ylog)
            y <- 10^y
        points(x, y, ...)
    }
    text2 <- function(x, y, ...) {
        if (xlog)
            x <- 10^x
        if (ylog)
            y <- 10^y
        text(x, y, ...)
    }
    if (trace)
        catn <- function(...) do.call("cat", c(lapply(list(...),
            formatC), list("\n")))
    cin <- par("cin")
    Cex <- cex * par("cex")
    if (is.null(text.width))
        text.width <- max(strwidth(legend, units = "user", cex = cex))
    else if (!is.numeric(text.width) || text.width < 0)
        stop("'text.width' must be numeric, >= 0")
    xc <- Cex * xinch(cin[1], warn.log = FALSE)
    yc <- Cex * yinch(cin[2], warn.log = FALSE)
    xchar <- xc
    xextra <- 0
    yextra <- yc * (y.intersp - 1)
    ymax <- max(yc, strheight(legend, units = "user", cex = cex))
    ychar <- yextra + ymax
    if (trace)
        catn("  xchar=", xchar, "; (yextra,ychar)=", c(yextra,
            ychar))
    if (mfill) {
        xbox <- xc * 0.8
        ybox <- yc * 0.5
        dx.fill <- xbox
    }
    do.lines <- (!missing(lty) && (is.character(lty) || any(lty >
        0))) || !missing(lwd)
    n.legpercol <- if (horiz) {
        if (ncol != 1)
            warning("horizontal specification overrides: Number of columns := ",
                n.leg)
        ncol <- n.leg
        1
    }
    else ceiling(n.leg/ncol)
    if (has.pch <- !missing(pch) && length(pch) > 0) {
        if (is.character(pch) && !is.na(pch[1]) && nchar(pch[1],
            type = "c") > 1) {
            if (length(pch) > 1)
                warning("not using pch[2..] since pch[1] has multiple chars")
            np <- nchar(pch[1], type = "c")
            pch <- substr(rep.int(pch[1], np), 1:np, 1:np)
        }
        if (!merge)
            dx.pch <- x.intersp/2 * xchar
    }
    x.off <- if (merge)
        -0.7
    else 0
    if (is.na(auto)) {
        if (xlog)
            x <- log10(x)
        if (ylog)
            y <- log10(y)
    }
    if (nx == 2) {
        x <- sort(x)
        y <- sort(y)
        left <- x[1]
        top <- y[2]
        w <- diff(x)
        h <- diff(y)
        w0 <- w/ncol
        x <- mean(x)
        y <- mean(y)
        if (missing(xjust))
            xjust <- 0.5
        if (missing(yjust))
            yjust <- 0.5
    }
    else {
        h <- (n.legpercol + (!is.null(title))) * ychar + yc
        w0 <- text.width + (x.intersp + 1) * xchar
        if (mfill)
            w0 <- w0 + dx.fill
        if (has.pch && !merge)
            w0 <- w0 + dx.pch
        if (do.lines)
            w0 <- w0 + (2 + x.off) * xchar
        w <- ncol * w0 + 0.5 * xchar
        if (!is.null(title) && (tw <- strwidth(title, units = "user",
            cex = cex) + 0.5 * xchar) > w) {
            xextra <- (tw - w)/2
            w <- tw
        }
        if (is.na(auto)) {
            left <- x - xjust * w
            top <- y + (1 - yjust) * h
        }
        else {
            usr <- par("usr")
            inset <- rep(inset, length.out = 2)
            insetx <- inset[1] * (usr[2] - usr[1])
            left <- switch(auto, bottomright = , topright = ,
                right = usr[2] - w - insetx, bottomleft = , left = ,
                topleft = usr[1] + insetx, bottom = , top = ,
                center = (usr[1] + usr[2] - w)/2)
            insety <- inset[2] * (usr[4] - usr[3])
            top <- switch(auto, bottomright = , bottom = , bottomleft = usr[3] +
                h + insety, topleft = , top = , topright = usr[4] -
                insety, left = , right = , center = (usr[3] +
                usr[4] + h)/2)
        }
    }
    if (plot && bty != "n") {
        if (trace)
            catn("  rect2(", left, ",", top, ", w=", w, ", h=",
                h, ", ...)", sep = "")
        rect2(left, top, dx = w, dy = h, col = bg, density = NULL, border = border.col)#added border = border.col
    }
    xt <- left + xchar + xextra + (w0 * rep.int(0:(ncol - 1),
        rep.int(n.legpercol, ncol)))[1:n.leg]
    yt <- top - 0.5 * yextra - ymax - (rep.int(1:n.legpercol,
        ncol)[1:n.leg] - 1 + (!is.null(title))) * ychar
    if (mfill) {
        if (plot) {
            fill <- rep(fill, length.out = n.leg)
            rect2(left = xt, top = yt + ybox/2, dx = xbox, dy = ybox,
                col = fill, density = density, angle = angle,
                border = box.col) #removed internal border
        }
        xt <- xt + dx.fill
    }
    if (plot && (has.pch || do.lines))
        col <- rep(col, length.out = n.leg)
    if (missing(lwd))
        lwd <- par("lwd")
    if (do.lines) {
        seg.len <- 2
        if (missing(lty))
            lty <- 1
        lty <- rep(lty, length.out = n.leg)
        lwd <- rep(lwd, length.out = n.leg)
        ok.l <- !is.na(lty) & (is.character(lty) | lty > 0)
        if (trace)
            catn("  segments2(", xt[ok.l] + x.off * xchar, ",",
                yt[ok.l], ", dx=", seg.len * xchar, ", dy=0, ...)")
        if (plot)
            segments2(xt[ok.l] + x.off * xchar, yt[ok.l], dx = seg.len *
                xchar, dy = 0, lty = lty[ok.l], lwd = lwd[ok.l],
                col = col[ok.l])
        xt <- xt + (seg.len + x.off) * xchar
    }
    if (has.pch) {
        pch <- rep(pch, length.out = n.leg)
        pt.bg <- rep(pt.bg, length.out = n.leg)
        pt.cex <- rep(pt.cex, length.out = n.leg)
        pt.lwd <- rep(pt.lwd, length.out = n.leg)
        ok <- !is.na(pch) & (is.character(pch) | pch >= 0)
        x1 <- (if (merge)
            xt - (seg.len/2) * xchar
        else xt)[ok]
        y1 <- yt[ok]
        if (trace)
            catn("  points2(", x1, ",", y1, ", pch=", pch[ok],
                ", ...)")
        if (plot)
            points2(x1, y1, pch = pch[ok], col = col[ok], cex = pt.cex[ok],
                bg = pt.bg[ok], lwd = pt.lwd[ok])
        if (!merge)
            xt <- xt + dx.pch
    }
    xt <- xt + x.intersp * xchar
    if (plot) {
        if (!is.null(title))
            text2(left + w/2, top - ymax, labels = title, adj = c(0.5,
                0), cex = cex, col = text.col)
        text2(xt, yt, labels = legend, adj = adj, cex = cex,
            col = text.col)
    }
    invisible(list(rect = list(w = w, h = h, left = left, top = top),
        text = list(x = xt, y = yt)))
}

# ------------------------------------------------------------------------------

# This is not a function, per se, but a way to set up specific color pallets
# for use in the charts we use.  These pallets have been designed to create
# readable, comparable line and bar graphs with specific objectives outlined
# before each category below.

# We use this approach rather than generating them on the fly for two reasons:
# 1) fewer dependencies on libraries that don't need to be called dynamically;
# and 2) to guarantee the color used for the n-th column of data.

    # FOCUS PALETTE
    # Colorsets designed to provide focus to the data graphed as the first element.
    # This palette is best used when there is clearly an important data set for the
    # viewer to focus on, with the remaining data being secondary, tertiary, etc.
    # Later elements graphed in diminishing values of gray.  These were generated
    # with RColorBrewer, using the 8 level "grays" palette and replacing the darkest
    # with the focus color.

    # For best results, replace the highlight color with the first color of the
    # equal weighted palette from below.  This will coordinate charts with different
    # purposes.

    #Red as highlight
    redfocus = c("#CB181D", "#252525", "#525252", "#737373", "#969696", "#BDBDBD", "#D9D9D9", "#F0F0F0")

    #Green as highlight
    greenfocus = c("#41AB5D", "#252525", "#525252", "#737373", "#969696", "#BDBDBD", "#D9D9D9", "#F0F0F0")

    #Blue as highlight
    bluefocus = c("#0033FF", "#252525", "#525252", "#737373", "#969696", "#BDBDBD", "#D9D9D9", "#F0F0F0")

    # EQUAL WEIGHT PALETTES
    # These colorsets are fine for when all of the data should be observed and
    # distinguishable on a line graph. The different numbers in the name
    # indicate the number of colors generated (six colors is probably the maximum
    # for a readable linegraph).

    # Generated with rainbow(12, s = 0.6, v = 0.75)
    rainbow12equal = c("#BF4D4D", "#BF864D", "#BFBF4D", "#86BF4D", "#4DBF4D", "#4DBF86", "#4DBFBF", "#4D86BF", "#4D4DBF", "#864DBF", "#BF4DBF", "#BF4D86")

    rainbow10equal = c("#BF4D4D", "#BF914D", "#A8BF4D", "#63BF4D", "#4DBF7A", "#4DBFBF", "#4D7ABF", "#634DBF", "#A84DBF", "#BF4D91")

    rainbow8equal = c("#BF4D4D", "#BFA34D", "#86BF4D", "#4DBF69", "#4DBFBF", "#4D69BF", "#864DBF", "#BF4DA3")

    rainbow6equal = c("#BF4D4D", "#BFBF4D", "#4DBF4D", "#4DBFBF", "#4D4DBF", "#BF4DBF")

    # Generated with package "gplots" function rich.colors(12)
    rich12equal = c("#000040", "#000093", "#0020E9", "#0076FF", "#00B8C2", "#04E466", "#49FB25", "#E7FD09", "#FEEA02", "#FFC200", "#FF8500", "#FF3300")

    rich10equal = c("#000041", "#0000A9", "#0049FF", "#00A4DE", "#03E070", "#5DFC21", "#F6F905", "#FFD701", "#FF9500", "#FF3300")

    rich8equal = c("#000041", "#0000CB", "#0081FF", "#02DA81", "#80FE1A", "#FDEE02", "#FFAB00", "#FF3300")

    rich6equal = c("#000043", "#0033FF", "#01CCA4", "#BAFF12", "#FFCC00", "#FF3300")

    # Generated with package "fields" function tim.colors(12), which is said to
    # emulate the default matlab colorset
    tim12equal = c("#00008F", "#0000EA", "#0047FF", "#00A2FF", "#00FEFF", "#5AFFA5", "#B5FF4A", "#FFED00", "#FF9200", "#FF3700", "#DB0000", "#800000")

    tim10equal = c("#00008F", "#0000FF", "#0070FF", "#00DFFF", "#50FFAF", "#BFFF40", "#FFCF00", "#FF6000", "#EF0000", "#800000")

    tim8equal = c("#00008F", "#0020FF", "#00AFFF", "#40FFBF", "#CFFF30", "#FF9F00", "#FF1000", "#800000")

    tim6equal = c("#00008F", "#005AFF", "#23FFDC", "#ECFF13", "#FF4A00", "#800000")

    # Generated with sort(brewer.pal(8,"Dark2")) #Dark2, Set2

    dark8equal = c("#1B9E77", "#666666", "#66A61E", "#7570B3", "#A6761D", "#D95F02", "#E6AB02", "#E7298A")

    dark6equal = c("#1B9E77", "#66A61E", "#7570B3", "#D95F02", "#E6AB02", "#E7298A")

    set8equal = c("#66C2A5", "#8DA0CB", "#A6D854", "#B3B3B3", "#E5C494", "#E78AC3", "#FC8D62", "#FFD92F")

    set6equal = c("#66C2A5", "#8DA0CB", "#A6D854", "#E78AC3", "#FC8D62", "#FFD92F")

    # MONOCHROME PALETTES
    # sort(brewer.pal(8,"Greens"))
    redmono = c("#99000D", "#CB181D", "#EF3B2C", "#FB6A4A", "#FC9272", "#FCBBA1", "#FEE0D2", "#FFF5F0")

    greenmono = c("#005A32", "#238B45", "#41AB5D", "#74C476", "#A1D99B", "#C7E9C0", "#E5F5E0", "#F7FCF5")

    bluemono = c("#084594", "#2171B5", "#4292C6", "#6BAED6", "#9ECAE1", "#C6DBEF", "#DEEBF7", "#F7FBFF")

    grey8mono = c("#000000","#252525", "#525252", "#737373", "#969696", "#BDBDBD", "#D9D9D9", "#F0F0F0")

    grey6mono = c("#242424", "#494949", "#6D6D6D", "#929292", "#B6B6B6", "#DBDBDB")

    # QUALITATIVE
    # by Paul Tol, http://www.sron.nl/~pault/colourschemes.pdf
    tol1qualitative=c("#4477AA")
    tol2qualitative=c("#4477AA", "#CC6677")
    tol3qualitative=c("#4477AA", "#DDCC77", "#CC6677")
    tol4qualitative=c("#4477AA", "#117733", "#DDCC77", "#CC6677")
    tol5qualitative=c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677")
    tol6qualitative=c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677","#AA4499")
    tol7qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#DDCC77", "#CC6677","#AA4499")
    tol8qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677","#AA4499")
    tol9qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677", "#882255", "#AA4499")
    tol10qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#882255", "#AA4499")
    tol11qualitative=c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#882255", "#AA4499")
    tol12qualitative=c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#AA4466", "#882255", "#AA4499")

    # A recent update to his technical paper adds a new banded rainbow scheme, several with a large number of steps.  He notes that when you're using them, you're better off picking a scheme that matches the number of colors needed, rather than using only a few colors from a larger scheme.

  tol14rainbow=c("#882E72", "#B178A6", "#D6C1DE", "#1965B0", "#5289C7", "#7BAFDE", "#4EB265", "#90C987", "#CAE0AB", "#F7EE55", "#F6C141", "#F1932D", "#E8601C", "#DC050C")

  tol15rainbow=c("#114477", "#4477AA", "#77AADD", "#117755", "#44AA88", "#99CCBB", "#777711", "#AAAA44", "#DDDD77", "#771111", "#AA4444", "#DD7777", "#771144", "#AA4477", "#DD77AA")

  tol18rainbow=c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788") 

  tol21rainbow= c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", "#117744", "#44AA77", "#88CCAA", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788")

# ------------------------------------------------------------------------------

# These are sets of data symbols for use as either pch or symbolset
#  e.g., chart.RiskReturnScatter(data.ts, colorset = rich8equal, symbolset =
#        c(opensymbols, closedsymbols))

    opensymbols = c(0,1,2,5,6)

    closedsymbols = c(15,16,17,18)

    fillsymbols = c(21,22,23,24,25)

    linesymbols = c(7,8,9,10,11,12,13,14)

    allsymbols = c(0:25)

# ------------------------------------------------------------------------------

# Similarly, these are event lists that we might want to annotate charts with.

    # Event lists - FOR BEST RESULTS, KEEP THESE IN ORDER
    risk.dates = c(
        "10/87",
        "02/94",
        "07/97",
        "08/98",
        "10/98",
        "07/00",
        "09/01")
    risk.labels = c(
        "Black Monday",
        "Bond Crash",
        "Asian Crisis",
        "Russian Crisis",
        "LTCM",
        "Tech Bubble",
        "Sept 11")

    equity.dates = c("01/05")
    equity.labels = c("Test Date")

    bond.dates = c()
    bond.labels = c()

    macro.dates = c()
    macro.labels = c()

    # Period beginning and endings, e.g., these are peak and trough dates from
    # http://www.nber.org/cycles.html/
    cycles.dates = list(
        c("10/26","11/27"),
        c("08/29","03/33"),
        c("05/37","06/38"),
        c("02/45","10/45"),
        c("11/48","10/49"),
        c("07/53","05/54"),
        c("08/57","04/58"),
        c("04/60","02/61"),
        c("12/69","11/70"),
        c("11/73","03/75"),
        c("01/80","07/80"),
        c("07/81","11/82"),
        c("07/90","03/91"),
        c("03/01","11/01"))

# ------------------------------------------------------------------------------

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
