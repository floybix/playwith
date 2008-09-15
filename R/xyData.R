# playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

getDataArg <- function(playState, eval = TRUE)
{
    if (!is.null(playState$data.points)) {
        ## data.points were supplied
        tmp.data <- if (eval) playState$data.points
        else quote(playDevCur()$data.points)
    } else {
        ## data.points missing; guess from call
        mainCall <- mainCall(playState)
        if (length(mainCall > 1)) {
            ## check for named "data" argument
            tmp.data <- callArg(playState, "data", eval = eval)
            if (is.null(tmp.data)) {
                ## look at first argument
                tmp.x <- callArg(playState, 1, eval=TRUE)
                if (inherits(tmp.x, "formula")) {
                    ## if 1st arg is formula, 2nd is data if un-named (by convention)
                    if (is.null(tmp.data) &&
                        (length(mainCall) >= 3) &&
                        (is.null(names(mainCall)) ||
                         identical(names(mainCall)[[3]], "")))
                    {
                        tmp.data <- callArg(playState, 2, eval = eval)
                    }
                }
            }
        }
        if (is.null(tmp.data)) {
            ## objects may also come from a with() block
            if (identical(playState$call[[1]], as.symbol("with"))) {
                tmp.data <- playState$call[[2]]
                if (eval) tmp.data <- eval(tmp.data, playState$env)
            }
        }
    }
    tmp.data
}

xyCoords <- function(playState = playDevCur(), space="plot")
{
    foo <- xyData(playState, space=space)
    foo$x <- as.numeric(foo$x)
    foo$y <- as.numeric(foo$y)
    foo
}

xyData <- function(playState = playDevCur(), space="plot")
{
    if (!is.null(playState$data.points)) {
        return(xy.coords_with_class(playState$data.points))
    }
    ## pass call name with a self-titled class to allow user-defined
    ## plotCoords definitions
    callName <- playState$callName
    class(callName) <- callName

    ## lattice -- get panel data from stored trellis object
    if (playState$is.lattice) {
        if (space == "page") {
            ## TODO: this only works with standard coords (xyplot etc)
            ## data from all packets
            tmp <- try(do.call(rbind,
                               lapply(playState$trellis$panel.args, as.data.frame)
                               ), silent=TRUE)
            if (inherits(tmp, "try-error"))
                return(NULL)
            return(tmp)
        }
        if (space == "plot") {
            space <- packet.number()
            if (length(space) == 0) {
                packets <- trellis.currentLayout(which="packet")
                if (sum(packets > 0) > 1) stop("space not well specified")
                space <- packets[packets > 0][1]
            }
            space <- paste("packet", space)
        }
        packet <- as.numeric(sub("packet ", "", space))
        if (is.na(packet) ||
            (packet > length(playState$trellis$panel.args)))
            return(NULL)
        foo <- trellis.panelArgs(playState$trellis, packet.number=packet)
        ## look for plotCoords method and pass panel.args
        return(plotCoords(callName, call = mainCall(playState),
                          envir = playState$env,
                          panel.args = foo))
    }
    ## non-lattice plot:
    ## look for plotCoords method and pass data arg
    plotCoords(callName, call = mainCall(playState),
               envir = playState$env,
               data = getDataArg(playState))
}

## TODO: allow plotCoords to return multiple coords for each data point (a list?) eg splom
## TODO: allow plotCoords to return a line structure for each data point? eg parallel

plotCoords <- function(name, call, envir, ...)
    UseMethod("plotCoords")

plotCoords.default <- function(name, call, envir, panel.args, data, ...)
{
    if (!missing(panel.args)) {
        ## lattice plot
        foo <- panel.args
        if (is.null(foo$y) && !is.null(foo$distribution)) {
            ## probably `qqmath`
            return(plotCoords.qqmath(name, call, envir,
                                    panel.args = panel.args))
        }
        if (!is.null(foo$scales.3d)) {
            ## probably `cloud` (but could be wireframe...)
            return(plotCoords.cloud(name, call, envir,
                                    panel.args = panel.args))
        }
        nx <- length(foo$x)
        ny <- length(foo$y)
        if ((nx == 0) || (ny == 0))
            return(NULL)
        if (nx != ny) {
            if (nx < ny)
                foo$x <- rep(foo$x, length.out = ny)
            else foo$y <- rep(foo$y, length.out = nx)
        }
        return(foo)

    } else {
        ## non-lattice plot
        if (length(call) <= 1) return(NULL)
        tmp.x <- eval(call[[2]], data, envir)
        tmp.y <- eval(call[["y"]], data, envir)
        xy.coords_with_class(tmp.x, tmp.y, data = data, envir = envir)
    }
}

plotCoords.qqnorm <-
plotCoords.qqplot <-
    function(name, call, envir, ...)
{
    ## these return plotted coordinates in a list
    call$plot <- FALSE
    eval(call, envir)
}

plotCoords.qqmath <- function(name, call, envir, panel.args, ...)
{
    ## based on panel.identify.qqmath
    x <- panel.args$x
    distribution <- panel.args$distribution
    groups <- panel.args$groups
    subscripts <- panel.args$subscripts
    x <- as.numeric(x)
    if (is.null(subscripts)) subscripts <- seq_along(x)
    if (!is.null(panel.args$f.value))
        return(NULL)
    distribution <-
        if (is.function(distribution)) distribution
        else if (is.character(distribution)) get(distribution)
        else eval(distribution)
    ## compute quantiles corresponding to given vector, possibly
    ## containing NA's.  The return value must correspond to the
    ## original order
    getq <- function(x)
    {
        ans <- x
        id <- !is.na(x)
        ord <- order(x[id])
        if (any(id)) ans[id] <- distribution(ppoints(sum(id)))[order(ord)]
        ans
    }
    if (is.null(groups))
    {
        return(list(x = getq(x), y = x, subscripts = subscripts))
    }
    else
    {
        allq <- rep(NA_real_, length(x))
        subg <- groups[subscripts]
        vals <- if (is.factor(groups)) levels(groups) else sort(unique(groups))
        for (i in seq_along(vals))
        {
            ok <- !is.na(subg) & (subg == vals[i])
            allq[ok] <- getq(x[ok])
        }
        return(list(x = allq, y = x, subscripts = subscripts))
    }
}

plotCoords.cloud <- function(name, call, envir, panel.args, ...)
{
    idcall <- call("panel.identify.cloud", panel.args = panel.args)
    for (x in c("screen", "R.mat", "perspective", "distance", "aspect")) {
        val <- call[[x]]
        if (!is.null(val))
            idcall[x] <- list(val)
    }
    idcall$panel.3d.identify <-
        function(x, y, z, rot.mat = diag(4), distance, xlim.scaled,
                 ylim.scaled, zlim.scaled, subscripts, ...)
        {
            id <- ((x >= xlim.scaled[1]) & (x <= xlim.scaled[2]) &
                   (y >= ylim.scaled[1]) & (y <= ylim.scaled[2]) &
                   (z >= zlim.scaled[1]) & (z <= zlim.scaled[2]) &
                   !is.na(x) & !is.na(y) & !is.na(z))
            m <- ltransform3dto3d(rbind(x, y, z), rot.mat, distance)
            xpos <- m[1, ]
            ypos <- m[2, ]
            xpos[!id] <- NA
            ypos[!id] <- NA
            list(x = xpos, y = ypos, subscripts = subscripts)
        }
    eval(idcall, envir)
}

## adapted from grDevices::xy.coords
xy.coords_with_class <-
    function(x, y = NULL, recycle = TRUE, data = NULL, envir = NULL)
{
    if (is.null(y)) {
        if (is.language(x)) {
            if (inherits(x, "formula") && length(x) == 3) {
                if (!is.null(data)) {
                    y <- eval(x[[2]], data, envir)
                    x <- eval(x[[3]], data, envir)
                } else {
                    y <- eval(x[[2]], environment(x), envir)
                    x <- eval(x[[3]], environment(x), envir)
                }
            }
            else return(NULL) #stop("invalid first argument")
        }
        else if (inherits(x, "zoo")) {
            y <- if (is.matrix(x)) x[, 1] else x
            x <- stats::time(x)
        }
        else if (inherits(x, "ts")) {
            y <- if (is.matrix(x)) x[, 1] else x
            x <- stats::time(x)
        }
        else if (is.complex(x)) {
            y <- Im(x)
            x <- Re(x)
        }
        else if (is.matrix(x) || is.data.frame(x)) {
            if (ncol(x) == 1) {
                y <- x[, 1]
                x <- seq_along(y)
            }
            else {
                y <- x[, 2]
                x <- x[, 1]
            }
        }
        else if (is.list(x)) {
            y <- x[["y"]]
            x <- x[["x"]]
        }
        else {
            y <- x
            x <- seq_along(x)
        }
    }
    if (inherits(x, "zoo")) {
        ## and y is not null
        x <- as.numeric(x)
        y <- as.numeric(y)
    }
    if (inherits(x, "POSIXt")) x <- as.POSIXct(x)
    if (length(x) != length(y)) {
        if (recycle) {
            if ((nx <- length(x)) < (ny <- length(y)))
                x <- rep(x, length.out = ny)
            else y <- rep(y, length.out = nx)
        } else stop("'x' and 'y' lengths differ")
    }
    list(x = x, y = y)
}

labels.zoo <-
labels.ts <-
    function(object, ...)
{
    rep(format(stats::time(object)), length = length(object))
}

## aka playUnLog
spaceCoordsToDataCoords <- function(playState, xy)
{
    if (!is.null(xy$x)) xy$x <-
        spaceCoordsToDataCoordsXY(playState, xy$x, x.or.y="x")
    if (!is.null(xy$y)) xy$y <-
        spaceCoordsToDataCoordsXY(playState, xy$y, x.or.y="y")
    xy
}

spaceCoordsToDataCoordsXY <- function(playState, x, x.or.y=c("x", "y"))
{
    x.or.y <- match.arg(x.or.y)
    logBase <- playLogBase(playState, x.or.y)
    if (is.na(logBase)) return(x)
    logBase ^ x
}

## aka playReLog
dataCoordsToSpaceCoords <- function(playState, xy)
{
    if (!is.null(xy$x)) xy$x <-
        dataCoordsToSpaceCoordsXY(playState, xy$x, x.or.y="x")
    if (!is.null(xy$y)) xy$y <-
        dataCoordsToSpaceCoordsXY(playState, xy$y, x.or.y="y")
    xy
}

dataCoordsToSpaceCoordsXY <- function(playState, x, x.or.y=c("x", "y"))
{
    x.or.y <- match.arg(x.or.y)
    logBase <- playLogBase(playState, x.or.y)
    if (is.na(logBase)) return(x)
    log(x, base=logBase)
}

playLogBase <- function(playState, x.or.y=c("x", "y"))
{
    x.or.y <- match.arg(x.or.y)
    if (playState$is.lattice) {
        scalesObj <- playState$trellis[[paste(x.or.y, "scales", sep=".")]]
        x <- scalesObj$log
        if (identical(x, FALSE)) return(NA)
        if (isTRUE(x)) return(10)
        if (identical(x, "e")) return(exp(1))
        return(x)
    } else if (playState$is.ggplot) {
        logArg <- callArg(playState, "log")
        if (!is.null(logArg) &&
            (x.or.y %in% strsplit(logArg, split="")[[1]]))
            return(10)
    } else {
        ## traditional graphics plot
        if (par(paste(x.or.y, "log", sep=""))) return(10)
    }
    return(NA)
}
