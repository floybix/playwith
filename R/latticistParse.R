## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2008 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

latticistParse <- function(call, trellis = NULL)
{
    if (missing(call) && !is.null(trellis))
        call <- trellis$call
    stopifnot(is.call(call))
    ## check that it is a sensible call
    if (length(call) <= 1) return(NULL)
    ## defaults
    xvar <- yvar <- zvar <- groups <- c1 <- c2 <- NULL
    subset <- aspect <- aspect3D <- x.relation <- y.relation <- NULL
    xdisc <- ydisc <- xprop <- yprop <- gprop <- FALSE
    doTile <- doSegments <- doAsError <- FALSE
    nLevels <- NULL
    defaultPlot <- "marginal.plot"
    ## get name of plot function
    callName <- toString(deparse(call[[1]]))
    isHypervar <-
        (callName %in% c("splom", "parallel", "marginal.plot"))
    if (isHypervar)
        defaultPlot <- callName
    is3D <- (callName %in% c("cloud", "wireframe"))
    if (!is.null(trellis)) ## more reliable:
        is3D <- !is.null(trellis$panel.args.common$scales.3d)
    isTrivar <- (callName %in%
                 c("levelplot", "contourplot", "segplot", "tileplot"))
    isTrivar <- (isTrivar || is3D)
    doTile <- (callName == "tileplot")

    isInteraction <- function(x)
        is.call.to(x, "*") || is.call.to(x, "+")
    isDiscretized <- function(x) {
        is.call.to(x, "equal.count") ||
        is.call.to(x, "cut") ||
        is.call.to(x, "cutEq")
    }

    ## parse variables from plot call arguments
    arg1 <- call[[2]]
    groups <- call[["groups"]]
    subset <- call[["subset"]]
    ## if is formula
    if (is.call.to(arg1, "~")) {
        if (length(arg1) == 2) {
            xvar <- arg1[[2]]
        } else {
            yvar <- arg1[[2]]
            xvar <- arg1[[3]]
        }
        if (is.call.to(xvar, "|")) {
            c1 <- xvar[[3]]
            xvar <- xvar[[2]]
        }
        if (isTrivar && isInteraction(xvar)) {
            if (callName == "segplot") {
                doSegments <- TRUE
                ## y ~ x + z
                zvar <- xvar[[3]]
                xvar <- xvar[[2]]
                ## as.error form: y ~ I(x-z) + I(x+z)
                if (is.call.to(xvar, "I") && is.call.to(zvar, "I")) {
                    if (length(xvar[[2]]) == 3) {
                        zvar <- xvar[[2]][[3]]
                        xvar <- xvar[[2]][[2]]
                        doAsError <- TRUE
                    }
                }
                groups <- call[["level"]]
            } else {
                ## z ~ x * y
                zvar <- yvar
                yvar <- xvar[[3]]
                xvar <- xvar[[2]]
                if (callName %in% c("levelplot", "tileplot")) {
                    groups <- zvar
                    zvar <- NULL
                }
            }
        }
        ## qqmath by convention has var on y axis
        if (callName == "qqmath") {
            yvar <- xvar
            xvar <- NULL
        }
        ## separate multiple conditioning variables
        if (isInteraction(c1)) {
            c2 <- c1[[3]]
            c1 <- c1[[2]]
        }
    }
    else if (is.call.to(arg1, "xtabs") ||
             is.call.to(arg1, "prop.table")) {
        propMargins <- NULL
        if (is.call.to(arg1, "prop.table")) {
            propMargins <- eval(arg1$margin)
            arg1 <- arg1[[2]]
        }
        xform <- arg1[[2]]
        ## parse xtabs formula
        vars.expr <- attr(terms(eval(xform)), "variables")
        vars <- as.list(vars.expr)[-1]
        if (callName %in% c("cloud", "levelplot")) {
            xvar <- vars[[1]]
            yvar <- vars[[2]]
            vars <- vars[-(1:2)]
            xprop <- (1 %in% propMargins)
            yprop <- (2 %in% propMargins)
        } else {
            if (identical(eval(call[["horizontal"]]), FALSE)) {
                ## variable is on x axis
                xvar <- vars[[1]]
                xprop <- !is.null(propMargins)
            } else {
                ## variable is on y axis
                yvar <- vars[[1]]
                yprop <- !is.null(propMargins)
            }
            vars <- vars[-1]
        }
        if (!identical(groups, FALSE)) {
            if (length(vars) >= 1) {
                groups <- vars[[length(vars)]]
                vars <- vars[-length(vars)]
            }
        }
        if (length(vars) >= 1) c1 <- vars[[1]]
        if (length(vars) >= 2) c2 <- vars[[2]]
        subset <- arg1$subset
    } else {
        ## other object (probably data.frame)
    }

    if (isHypervar) ## by definition:
        xvar <- yvar <- zvar <- NULL

    if (isTRUE(groups) || identical(groups, FALSE))
        groups <- NULL

    ## strip discretization code for display
    nLevels <- INIT.NLEVELS
    nlevset <- FALSE
    stripDisc <- function(x, env = parent.frame()) {
        if (isDiscretized(x) &&
            is.numeric(n <- x[[3]]) &&
            (!nlevset || (n == nLevels)))
        {
            x <- x[[2]]
            env$nLevels <- n
            env$nlevset <- TRUE
        }
        x
    }
    if (!nlevset) nLevels <- NULL
    oxvar <- xvar
    oyvar <- yvar
    xvar <- stripDisc(xvar)
    yvar <- stripDisc(yvar)
    xdisc <- !identical(xvar, oxvar)
    ydisc <- !identical(yvar, oyvar)
    c1 <- stripDisc(c1)
    c2 <- stripDisc(c2)
                                        #groups <- stripDisc(groups) ## TODO - ?
    ## histogram by convention has x discretized
    if (callName == "histogram") {
        xdisc <- TRUE
    }
    ## hexbinplot by convention has x and y discretized
    if (callName == "hexbinplot") {
        xdisc <- TRUE
        ydisc <- TRUE
    }

    ## strip reordering code for display
    stripReorder <- function(x) {
        if (is.call.to(x, "reorder") ||
            is.call.to(x, "reorderByFreq"))
            x <- stripReorder(x[[2]])
        x
    }
    xvar <- stripReorder(xvar)
    yvar <- stripReorder(yvar)
    c1 <- stripReorder(c1)
    c2 <- stripReorder(c2)
    groups <- stripReorder(groups)

    ## aspect
    aspect <- eval(call[["aspect"]])
    if (callName == "cloud") {
        aspect3D <- aspect
        aspect <- eval(call[["panel.aspect"]])
    }
    ## scales
    scales <- call[["scales"]]
    x.relation <- NULL
    y.relation <- NULL
    if (is.character(scales)) {
        x.relation <- y.relation <- scales
    } else if (is.character(scales$relation)) {
        x.relation <- y.relation <- scales$relation
    } else {
        if (is.character(scales$x)) {
            x.relation <- scales$x
        } else if (is.character(scales$x$relation)) {
            x.relation <- scales$x$relation
        }
        if (is.character(scales$y)) {
            y.relation <- scales$y
        } else if (is.character(scales$y$relation)) {
            y.relation <- scales$y$relation
        }
    }

    ## create latticist specification
    list(xvar = xvar
         yvar = yvar,
         zvar = zvar,
         groups = groups,
         cond = c1,
         cond2 = c2,
         subset = subset,
         aspect = aspect,
         aspect3D = aspect3D,
         x.relation = x.relation,
         y.relation = y.relation,
         xdisc = xdisc,
         ydisc = ydisc,
         xprop = xprop,
         yprop = yprop,
         gprop = gprop,
         doTile = doTile,
         doSegments = doSegments,
         doAsError = doAsError,
         nLevels = nLevels,
         defaultPlot = defaultPlot)
}


