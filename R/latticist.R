## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2008 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer


latticist <-
    function(dat,
             reorder.levels = TRUE,
             plot.call = quote(marginal.plot(dat, reorder = FALSE)),
             ...,
             labels = rownames(dat),
             time.mode = FALSE)
{
    isDefaultPlotCall <- missing(plot.call)
    datArg <- quote(unknown)
    if (missing(dat)) {
        if (missing(plot.call))
            stop("Give one of 'dat' or 'plot.call'.")
        ## plot.call was given; try to extract data
        ## also need to replace it in the call with `dat`
        dat <- NULL
        ## assuming the relevant lattice function is the outer call
        ## check for named 'data' argument
        if (!is.null(plot.call$data)) {
            datArg <- plot.call$data
            dat <- eval.parent(datArg)
            plot.call$data <- quote(dat)
        } else {
            if (length(plot.call) <= 1) {
                ## no arguments or NULL call
                stop("Can not extract data from plot.call.")
            } else {
                ## one or more arguments
                datArg <- plot.call[[2]]
                dat <- eval.parent(datArg)
                if (!inherits(dat, "formula")) {
                    ## go with first argument
                    plot.call[[2]] <- quote(dat)
                } else {
                    ## try second argument if exists and un-named
                    if ((length(plot.call) > 2) &&
                        (is.null(names(plot.call)) ||
                         identical(names(plot.call)[3], "")))
                    {
                        datArg <- plot.call[[3]]
                        dat <- eval.parent(datArg)
                        plot.call[[3]] <- quote(dat)
                    } else {
                        stop("Can not extract data from plot.call.")
                    }
                }
            }
        }
    } else {
        ## dat was given
        datArg <- substitute(dat)
    }
    title <- paste("Latticist:", toString(deparse(datArg), width=30))

    if (!is.data.frame(dat))
        dat <- as.data.frame(dat)

    ## convert numerics with discrete values in {-1, 0, 1} to factors
    isnum <- sapply(dat, is.numeric)
    for (nm in names(dat)[isnum]) {
        dd <- dat[[nm]]
        ## test first 50 values first (quick check)
        vals <- unique(head(dd, n=50))
        if (all(vals %in% -1:1)) {
            if (all(range(dd, na.rm=TRUE) %in% -1:1) &&
                all(range(abs(diff(dd)), na.rm=TRUE) %in% 0:2))
            {
                dat[[nm]] <- factor(dd)
            }
        }
    }

    if (reorder.levels) {
        iscat <- sapply(dat, is.categorical)
        for (nm in names(dat)[iscat]) {
            val <- dat[[nm]]
            if (is.character(val))
                dat[[nm]] <- factor(val)
            if (!is.ordered(val) &&
                !is.shingle(val) &&
                nlevels(val) > 1)
            {
                dat[[nm]] <- reorderByFreq(val)
            }
        }
    }

    playwith(plot.call = plot.call,
             title = title, ...,
             labels = labels,
             time.mode = time.mode,
             latticist = list(), ## to replace (reset) any existing one
             init.actions = list(latticist = makeLatticist(dat)))
}

makeLatticist <- function(dat)
{
    isInteraction <- function(x)
        is.call.to(x, "*") || is.call.to(x, "+")
    isDiscretized <- function(x) {
        is.call.to(x, "equal.count") ||
        is.call.to(x, "cut") ||
        is.call.to(x, "cutEq")
    }
    isUnordered <- function(x, val) {
        ## need this because is.ordered(cut()) == FALSE!
        if (is.call.to(x, "cut")) return(FALSE)
        if (is.call.to(x, "cut2")) return(FALSE)
        ## assumes is.categorical(val)
        !is.ordered(val) && !is.shingle(val)
    }

    LOTS <- 1000
    HEAPS <- 8000
    MAXPANELS <- 16
    INIT.NLEVELS <- 4

    function(playState)
    {
        ## create list to store some settings
        if (length(playState$latticist) == 0)
            playState$latticist <- list()
        ## get arguments to current call
        callName <- playState$callName
        isHypervar <- (callName %in% c("splom", "parallel", "marginal.plot"))
        is3D <- !is.null(playState$trellis$panel.args.common$scales.3d)
        isTrivar <- (callName %in%
                     c("levelplot", "contourplot", "segplot", "tileplot"))
        isTrivar <- isTrivar || is3D
        tileVal <- (callName == "tileplot")
        arg1 <- callArg(playState, 1, eval=FALSE)
        groups <- callArg(playState, "groups", eval=FALSE)
        level <- callArg(playState, "level", eval=FALSE)
        subset <- callArg(playState, "subset", eval=FALSE)
        yprop <- xprop <- FALSE
        segmentsVal <- aserrorVal <- FALSE

        ## parse variables from existing plot call
        xvar <- yvar <- zvar <- c1 <- c2 <- NULL
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
                    segmentsVal <- TRUE
                    ## y ~ x + z
                    zvar <- xvar[[3]]
                    xvar <- xvar[[2]]
                    ## as.error form: y ~ I(x-z) + I(x+z)
                    if (is.call.to(xvar, "I") && is.call.to(zvar, "I")) {
                        if (length(xvar[[2]]) == 3) {
                            zvar <- xvar[[2]][[3]]
                            xvar <- xvar[[2]][[2]]
                            aserrorVal <- TRUE
                        }
                    }
                    groups <- level
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
                if (identical(callArg(playState, "horizontal"), FALSE)) {
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

        if (isHypervar)
            xvar <- yvar <- zvar <- NULL

        if (isTRUE(groups) || identical(groups, FALSE))
            groups <- NULL

        ## strip discretization code for display
        nLevels <- INIT.NLEVELS
        nlevset <- FALSE
        stripDisc <- function(x, env=parent.frame()) {
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

        ## evaluate variables to determine data types
        xcat <- is.categorical(eval(xvar, dat))
        ycat <- is.categorical(eval(yvar, dat))
        gcat <- is.categorical(eval(groups, dat))

        ## set up variables and options
        xvarStr <- deparseOneLine(xvar)
        yvarStr <- deparseOneLine(yvar)
        zvarStr <- deparseOneLine(zvar)
        c1Str <- deparseOneLine(c1)
        c2Str <- deparseOneLine(c2)
        groupsStr <- deparseOneLine(groups)

        iscat <- NULL

        NULLNAMES <- c("(none)", "")

        ## variables and expressions
        varexprs <- playState$latticist$varexprs
        if (is.null(varexprs)) {
            iscat <- sapply(dat, is.categorical)
            varexprs <- c("NULL",
                          names(dat)[iscat],
                          if (any(iscat) && any(!iscat))
                          "------------------",
                          names(dat)[!iscat],
                          "-------------------",
                          "1:nrow(dat)")
            ## log() of positive numerics
                                        #logs <- lapply(names(dat)[!iscat], function(nm) {
                                        #    if (all(dat[[nm]] > 0, na.rm=TRUE))
                                        #        paste("log(", nm, ")", sep="")
                                        #    else NULL
                                        #})
                                        #varexprs <- c(varexprs, unlist(logs))

            ## is.na() of variables with missing values
                                        #missings <- lapply(names(dat), function(nm) {
                                        #    if (any(is.na(dat[[nm]])))
                                        #        paste("is.na(", nm, ")", sep="")
                                        #    else NULL
                                        #})
                                        #varexprs <- c(varexprs, unlist(missings))
        }
        varexprs <- unique(c(varexprs,
                             xvarStr, yvarStr,
                             c1Str, c2Str, groupsStr))
        playState$latticist$varexprs <- varexprs
        varexprs[[1]] <- NULLNAMES[[1]]

        ## subset
        subsetopts <- playState$latticist$subsets
        if (is.null(subsetopts)) {
            ## preload some useful subsets
            subsetopts <- NULLNAMES[[1]]
            ## preload factor levels (only most frequent two of each)
            toplev <- lapply(names(dat)[iscat], function(nm) {
                tmp <- names(sort(table(dat[[nm]]), decreasing=TRUE))
                tmp <- tmp[seq_len(min(2, length(tmp)))] ## top 2
                paste(nm, "==", sapply(tmp, deparse))
            })
            subsetopts <- c(subsetopts, unlist(toplev))
            subsetopts <- c(subsetopts, "------------------")
            if (nrow(dat) >= LOTS) {
                ## a regular sample down by one order of magnitude
                subN <- 10 ^ (round(log10(nrow(dat))) - 1)
                subsetopts <- c(subsetopts,
                                sprintf("seq(1, nrow(dat), length = %i)", subN))
                subsetopts <- c(subsetopts, "-----------------")
            }
            ## is.finite() of variables with missing values
            missings <- lapply(names(dat), function(nm) {
                if (any(is.na(dat[[nm]])))
                    paste("!is.na(", nm, ")", sep="")
                else NULL
            })
            missings <- unlist(missings)
            if (length(missings) > 0) {
                subsetopts <- c(subsetopts, "complete.cases(dat)")
            }
            subsetopts <- c(subsetopts, missings)
        }
        subsetopts <-
            unique(c(subsetopts, if (!is.null(subset) && !isTRUE(subset))
                     deparseOneLine(subset)))
        playState$latticist$subsets <- subsetopts

        ## aspect
        aspectVal <- callArg(playState, "aspect")
        aspect3DVal <- NULL
        if (callName == "cloud") {
            aspect3DVal <- aspectVal
            aspectVal <- callArg(playState, "panel.aspect")
        }
        aspectopts <- c('"fill"', '"iso"', '"xy"',
                        "0.5", "1", "2")
        if (!is.null(aspectVal)) {
            aspectVal <- deparseOneLine(aspectVal)
            aspectopts <-
                unique(c(aspectopts, aspectVal))
        }
        ## scales
        scalesopts <- c("x same, y same",
                        "x same, y free",
                        "x free, y same",
                        "x free, y free",
                        "------------------",
                        "x sliced, y sliced",
                        "x sliced, y same",
                        "x sliced, y free",
                        "x same, y sliced",
                        "x free, y sliced")
        scales <- callArg(playState, "scales")
        scalesVal <- NULL
        if (is.character(scales) || is.character(scales$relation) ||
            is.character(scales$x) || is.character(scales$y) ||
            is.character(scales$x$relation) || is.character(scales$y$relation))
        {
            scalesVal <- paste("x ", playState$trellis$x.scales$relation, ", ",
                               "y ", playState$trellis$y.scales$relation, sep="")
        }

        ## lines setting
        linesVal <- playState$latticist$linesSetting
        if (is.null(linesVal)) {
            linesVal <- TRUE
            playState$latticist$linesSetting <- linesVal
        }

        tryParse <- function(x0) {
            x <- x0
            if (x %in% NULLNAMES) x <- "NULL"
            result <- tryCatch(parse(text=x)[[1]], error=function(e)e)
            ## check whether there was a syntax error
            if (inherits(result, "error")) {
                itemName <- deparseOneLine(substitute(x0))
                msg <- paste("Error parsing ", itemName, ": ",
                             conditionMessage(result), sep="")
                gmessage.error(msg)
                stop(result) ## kills R under linux ("stack smashing detected")
            }
            result
        }
        tryEval <- function(x0, ...) {
            x <- x0
            result <- tryCatch(eval(x, ...), error=function(e)e)
            ## check whether there was a syntax error
            if (inherits(result, "error")) {
                itemName <- deparseOneLine(substitute(x0))
                msg <- paste("Error evaluating ", itemName, ": ",
                             conditionMessage(result), sep="")
                gmessage.error(msg)
                stop(result) ## kills R under linux ("stack smashing detected")
            }
            result
        }

        ## generate formula and other args from widget settings
        composePlot <- function(playState, newXY = FALSE) {
            ## this is needed by handler functions to fiddle with GUI:
            if (!isTRUE(playState$tmp$plot.ready)) return()
            ## parse variables / expressions
            xvar <- tryParse(xvarW$getActiveText())
            yvar <- tryParse(yvarW$getActiveText())
            zvar <- tryParse(zvarW$getActiveText())
            if (xonW["active"] == FALSE) xvar <- NULL
            if (yonW["active"] == FALSE) yvar <- NULL
            if (is.null(xvar) || is.null(yvar))
                zvar <- NULL
            c1 <- tryParse(c1W$getActiveText())
            c2 <- tryParse(c2W$getActiveText())
            groups <- tryParse(groupsW$getActiveText())
            ## options settings
            doXDisc <- xdiscW["active"]
            doYDisc <- ydiscW["active"]
            doXProp <- xpropW["active"]
            doYProp <- ypropW["active"]
            nlev <- nLevelsW["value"]
            doLines <- playState$latticist$linesSetting
            doTile <- tileW["active"] #### TODO: does not keep state...
            doSegments <- segmentsW["active"]
            doAsError <- aserrorW["active"]
            do3DTable <- playState$latticist$do3DTable
            if (is.null(do3DTable)) do3DTable <- FALSE
            ## keep for titles etc
            xvarOrigStr <- deparseOneLine(xvar)
            yvarOrigStr <- deparseOneLine(yvar)
            zvarOrigStr <- deparseOneLine(zvar)
            c1OrigStr <- deparseOneLine(c1)
            c2OrigStr <- deparseOneLine(c2)
            groupsOrigStr <- deparseOneLine(groups)
            ## parse aspect and scales except if new xy structure
            aspect <- NULL
            x.relation <- NULL
            y.relation <- NULL
            if (!newXY) {
                aspect <- tryParse(aspectW$getActiveText())
                scalesIdx <- scalesW$getActive() + 1
                if (scalesIdx > 0) {
                    tmp <- strsplit(scalesopts[scalesIdx], ", ")[[1]]
                    x.relation <- substring(tmp[1], first=3)
                    y.relation <- substring(tmp[2], first=3)
                }
            }
            subset <- tryParse(subsetW$getActiveText())
            if (is.null(subset)) subset <- TRUE

            ## evaluate to check types
            xVal <- tryEval(xvar, dat)
            yVal <- tryEval(yvar, dat)
            zVal <- tryEval(zvar, dat)
            c1Val <- tryEval(c1, dat)
            c2Val <- tryEval(c2, dat)
            groupsVal <- tryEval(groups, dat)
            subsetVal <- tryEval(subset, dat)
            ## calculate number of data points
            nPoints <- 0
            if (!is.null(xVal) && !is.null(yVal))
                nPoints <- sum(is.finite(xVal[subsetVal]) &
                               is.finite(yVal[subsetVal]))
            else if (!is.null(xVal))
                nPoints <- sum(is.finite(xVal[subsetVal]))
            else if (!is.null(yVal))
                nPoints <- sum(is.finite(yVal[subsetVal]))

            ## discretize
            do.xdisc <- (!is.null(xvar) && !is.categorical(xVal) && xdiscW["active"])
            do.ydisc <- (!is.null(yvar) && !is.categorical(yVal) && ydiscW["active"])
            do.c1disc <- (!is.null(c1) && !is.categorical(c1Val))
            do.c2disc <- (!is.null(c2) && !is.categorical(c2Val))
            do.gdisc <- (!is.null(groups) && !is.categorical(groupsVal))

            ## if there are any numerical variables on plot
            ## we can use shingles (otherwise plotting a "table" method)
            anyNumerics <- ((!is.null(xvar) && !is.categorical(xVal) && !do.xdisc) ||
                            (!is.null(yvar) && !is.categorical(yVal) && !do.ydisc))
            if (anyNumerics) {
                ## use shingles where appropriate
                if (do.xdisc) xvar <- call("equal.count", xvar, nlev) ## or cut?
                if (do.ydisc) yvar <- call("equal.count", yvar, nlev) ## or cut?
                ## conditioning variables
                if (do.c1disc) c1 <- call("equal.count", c1, nlev)
                if (do.c2disc) c2 <- call("equal.count", c2, nlev)
            } else {
                ## table method, need factors not shingles
                if (do.xdisc) {
                    if (is.null(yvar) || do.ydisc)
                        xvar <- call("cut", xvar, nlev)
                    else xvar <- call("cutEq", xvar, nlev)
                }
                if (do.ydisc) {
                    if (is.null(xvar) || do.xdisc)
                        yvar <- call("cut", yvar, nlev)
                    else yvar <- call("cutEq", yvar, nlev)
                }
                if (do.c1disc) c1 <- call("cutEq", c1, nlev)
                if (do.c2disc) c2 <- call("cutEq", c2, nlev)
            }
            if (do.gdisc) groups <- call("cutEq", groups, nlev)
            ## re-evaluate data if changed
            if (do.xdisc) xVal <- tryEval(xvar, dat)
            if (do.ydisc) yVal <- tryEval(yvar, dat)
            if (do.c1disc) c1Val <- tryEval(c1, dat)
            if (do.c2disc) c2Val <- tryEval(c2, dat)
            if (do.gdisc) groupsVal <- tryEval(groups, dat) ## TODO avoid this?

            ## if only one conditioning term, call it c1
            if (is.null(c1) && !is.null(c2)) {
                c1 <- c2
                c1Val <- c2Val
                c2 <- c2Val <- NULL
            }

            ## reorder factor levels of groups
                                        #            if (!is.null(groupsVal)) {
                                        #                if (isUnordered(groups, groupsVal)) {
                                        #                    groups <- call("reorderByFreq", groups)
                                        #                    groupsVal <- tryEval(groups, dat)
                                        #                }
                                        #            }

            ## reorder conditioning (TODO: index.cond)
            if (!is.null(c1Val)) {
                                        #                    if (!is.null(yVal) || !is.null(xVal)) {
                                        #                        c1 <- call("reorder", c1,
                                        #                                   if (!is.null(yVal)) yvar else xvar)
                                        #                    }
                                        #                    c1Val <- tryEval(c1, dat)
            }

            ## combined conditioning term (may be NULL)
            cond <- c1
            if (!is.null(c2)) cond <- call("*", c1, c2)
            ncond <- 1
            tooManyPanels <- FALSE
            if (!is.null(cond)) {
                ncond <- nlevels(c1Val)
                if (!is.null(c2Val)) ncond <- ncond * nlevels(c2Val)
                if (ncond > MAXPANELS) tooManyPanels <- TRUE
            }

            ## create template plot call
            oldCall <- playState$call
            playState$call <- quote(xyplot(0 ~ 0, data = dat))
            updateMainCall(playState)
            callArg(playState, "subset") <-
                if (!isTRUE(subset)) subset else NULL
            ## useOuterStrips unless we are going to use layout=...
            if (!is.null(c1) && !is.null(c2) && !tooManyPanels) {
                playState$call <-
                    call("useOuterStrips", playState$call)
                updateMainCall(playState)
            }
                                        #if (is.numeric(groupsVal)) {
            ## handled below

                                        #playState$call <- call("+", playState$call,
                                        #                       bquote(layer.col(with(dat, .(groups)))))
                                        #updateMainCall(playState)
                                        #endpoints <- range(groupsVal)
                                        #digits <- max(2 - floor(log10(diff(endpoints))), 0)
                                        #endpoints <- round(endpoints, digits=digits)
                                        #callArg(playState, "legend") <-
                                        #    bquote(list(right =
                                        #                list(fun = draw.colorkey,
                                        #                     args = list(at=do.breaks(.(endpoints), 30))
                                        #                     )))
                                        #} else {
            callArg(playState, "groups") <- groups ## may be NULL
                                        #}
            if (tooManyPanels)
                callArg(playState, "layout") <-
                    c(0, min(MAXPANELS, ceiling(ncond / 2)))

            ## put shingle levels on axis
            if (anyNumerics && do.xdisc)
                callArg(playState, quote(scales$x$limits)) <-
                    as.character(levels(xVal))
            if (anyNumerics && do.ydisc)
                callArg(playState, quote(scales$y$limits)) <-
                    as.character(levels(yVal))
            ## put shingle levels in strip
            if (anyNumerics && do.c1disc && (is.null(c2) || do.c2disc))
                callArg(playState, "strip") <-
                    quote(strip.custom(strip.levels=TRUE, strip.names=FALSE))

            ## construct plot title
            if (!is.null(xvar) || !is.null(yvar)) {
                title <- paste(c(if (!is.null(yvar)) yvarOrigStr,
                                 if (!is.null(xvar)) xvarOrigStr),
                               collapse=" vs ")
                if (!is.null(zvar))
                    title <- paste(zvarOrigStr, "vs", xvarOrigStr,
                                   "and", yvarOrigStr)
                if (is.null(xvar) || is.null(yvar))
                    title <- paste("Distribution of", title)
                byStr <- paste(c(if (!is.null(c1)) c1OrigStr,
                                 if (!is.null(c2)) c2OrigStr,
                                 if (!is.null(groups)) groupsOrigStr),
                               collapse=" and ")
                if (nchar(byStr) > 0)
                    title <- paste(title, byStr, sep=" by ")
                callArg(playState, "main") <- title
            }

            ## axis labels (not for categoricals)
            if (!is.null(xvar) && !is.categorical(xVal))
                callArg(playState, "xlab") <- xvarOrigStr
            if (!is.null(yvar) && !is.categorical(yVal))
                callArg(playState, "ylab") <- yvarOrigStr

            ## choose plot type and formula
            if (is.null(xVal) && is.null(yVal)) {
                ## HYPERVARIATE

                opt <- playState$latticist$defaultPlotType
                if (is.null(opt)) opt <- "marginals"
                varsub <- playState$latticist$varsubset
                dat.expr <- quote(dat)
                if (!is.null(varsub))
                    dat.expr <- call("[", quote(dat), varsub)
                dat.form <- call("~", dat.expr)
                if (!is.null(cond))
                    dat.form <- call("~", call("|", dat.expr, cond))

                if (opt == "marginals") {
                    callArg(playState, 0) <- quote(marginal.plot)
                    callArg(playState, 1) <- dat.expr
                    callArg(playState, "data") <- NULL

                } else if (opt == "splom") {
                    callArg(playState, 0) <- quote(splom)
                    callArg(playState, 1) <- dat.form
                    callArg(playState, "cex") <- 0.5
                    callArg(playState, "pscales") <- 0

                } else if (opt == "parallel") {
                    callArg(playState, 0) <- quote(parallel)
                    callArg(playState, 1) <- dat.form
                }

            } else if (is.null(xVal) || is.null(yVal)) {
                ## UNIVARIATE

                if (is.categorical(xVal) || is.categorical(yVal)) {
                    ## UNIVARIATE CATEGORICAL
                    if (!is.null(yVal)) {
                        ## data on y axis, use dotplot
                        callArg(playState, 0) <- quote(dotplot)
                        callArg(playState, "data") <- NULL
                        callArg(playState, "subset") <- NULL
                        xterms <- paste(c(deparseOneLine(yvar),
                                          if (!is.null(c1)) deparseOneLine(c1),
                                          if (!is.null(c2)) deparseOneLine(c2),
                                          if (!is.null(groups)) deparseOneLine(groups)),
                                        collapse=" + ")
                        ## and set logical `groups` argument
                        callArg(playState, "groups") <- !is.null(groups)
                        xform <- as.formula(paste("~", xterms))
                        tabcall <-
                            call("xtabs", xform, quote(dat), subset=subset)
                        if (doYProp) {
                            tabcall <- call("prop.table", tabcall, margin = 1)
                        }
                        callArg(playState, 1) <- tabcall

                        if (doLines) {
                            if (!is.null(groups))
                                callArg(playState, "type") <- c("p", "l")
                            else
                                callArg(playState, "type") <- c("p", "h")
                            callArg(playState, "origin") <- 0
                        }

                    } else {
                        ## data on x axis, use barchart
                        ## (just for variety? & dotplot.table has no horizontal=FALSE)
                        ## BUT if x is a discretized numeric, use histogram
                        if (do.xdisc && is.null(groups)) {
                            xvar <- xvar[[2]] ## undo disc function
                            callArg(playState, 0) <- quote(histogram)
                            if (!is.null(cond))
                                callArg(playState, 1) <- call("~", call("|", xvar, cond))
                            else
                                callArg(playState, 1) <- call("~", xvar)
                        } else {
                            callArg(playState, 0) <- quote(barchart)
                            callArg(playState, "data") <- NULL
                            callArg(playState, "subset") <- NULL
                            xterms <- paste(c(deparseOneLine(xvar),
                                              if (!is.null(c1)) deparseOneLine(c1),
                                              if (!is.null(c2)) deparseOneLine(c2),
                                              if (!is.null(groups)) deparseOneLine(groups)),
                                            collapse=" + ")
                            ## and set logical `groups` argument
                            callArg(playState, "groups") <- !is.null(groups)
                            xform <- as.formula(paste("~", xterms))
                            tabcall <-
                                call("xtabs", xform, quote(dat), subset=subset)
                            if (doXProp) {
                                tabcall <- call("prop.table", tabcall, margin = 1)
                            }
                            callArg(playState, 1) <- tabcall
                            ## TODO: make stack an option?
                            callArg(playState, "stack") <- TRUE
                            callArg(playState, "horizontal") <- FALSE
                        }
                    }

                } else {
                    ## UNIVARIATE NUMERIC
                    if (!is.null(xVal)) {
                        ## data on x axis, use densityplot
                        callArg(playState, 0) <- quote(densityplot)
                        if (!is.null(cond))
                            callArg(playState, 1) <- call("~", call("|", xvar, cond))
                        else
                            callArg(playState, 1) <- call("~", xvar)
                        ## settings depend on number of points, groups
                        if (nPoints >= HEAPS) {
                            callArg(playState, "plot.points") <- FALSE
                        } else if (nPoints >= LOTS) {
                            callArg(playState, "plot.points") <- "jitter"
                            callArg(playState, "pch") <- "+" ## like jittered rug
                        }
                        callArg(playState, "ref") <- TRUE
                    } else {
                        ## data on y axis, use qqmath
                        callArg(playState, 0) <- quote(qqmath)
                        if (!is.null(cond))
                            callArg(playState, 1) <- call("~", call("|", yvar, cond))
                        else
                            callArg(playState, 1) <- call("~", yvar)
                        ## settings depend on number of points, groups
                        type <- "p"
                        if (nPoints >= HEAPS) {
                            callArg(playState, "f.value") <- quote(ppoints(100))
                            if (doLines) type <- "l"
                        } else {
                            if (doLines) type <- "o"
                        }
                        ## a grid is always useful with qqmath
                        callArg(playState, "type") <- c("g", type)
                        ## decide when to use normal vs uniform etc
                        tailtest <- function(x) {
                            qs <- quantile(x, c(0.01, 0.05, 0.95, 0.99), na.rm=TRUE)
                            diff(qs[c(1,4)]) / diff(qs[2:3])
                        }
                        ## expected with uniform is 1.1
                        ## expected with normal is 1.4
                        ## expected with lognormal is 2.0
                        tst <- tailtest(yVal)
                        if (tst >= 1.9) {
                            ## more skewed than normal
                            ## TODO: this is probably just confusing...
                            callArg(playState, "distribution") <- quote(qlnorm)
                            callArg(playState, "xlab") <- "Log-normal quantiles"
                        } else if (tst <= 1.2) {
                            ## uniform distribution
                            callArg(playState, "distribution") <- quote(qunif)
                            callArg(playState, "xlab") <- expression(Proportion <= y)
                        } else {
                            callArg(playState, "distribution") <- quote(qnorm)
                            callArg(playState, "xlab") <- "Probability (normal distribution)"
                            ## label probabilites on axis
                            ## TODO: axis.components function to do this
                            probs <- c(0.001, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 0.999)
                            callArg(playState, quote(scales$x$at)) <- bquote(qnorm(.(probs)))
                            callArg(playState, quote(scales$x$labels)) <- probs
                        }
                        callArg(playState, "prepanel") <- quote(prepanel.qqmathline)
                    }
                }

            } else if (is.null(zVal)) {
                ## BIVARIATE

                if (is.categorical(xVal) && is.categorical(yVal)) {
                    ## BIVARIATE CATEGORICAL
                    if (do.xdisc && do.ydisc && require("hexbin")) {
                        ## if both are discretized numerics, use 2D binning
                        ## use hexbin package if available
                        callArg(playState, 0) <- quote(hexbinplot)
                        xvar <- xvar[[2]] ## undo disc function
                        yvar <- yvar[[2]] ## undo disc function
                        if (!is.null(cond))
                            callArg(playState, 1) <- call("~", yvar, call("|", xvar, cond))
                        else
                            callArg(playState, 1) <- call("~", yvar, xvar)
                        type <- if (doLines) "r"
                        if (!is.null(cond)) type <- c("g", type)
                        callArg(playState, "type") <- type
                        if (is.null(aspect))
                            aspect <- 1
                        ## ignore groups setting
                        callArg(playState, "groups") <- NULL

                    } else {
                        ## 2D TABLE (3D BARCHART)
                        callArg(playState, 0) <- quote(cloud)
                        callArg(playState, "data") <- NULL
                        callArg(playState, "subset") <- NULL
                        ## reorder factor levels
                        if (isUnordered(yvar, yVal)) {
                            if (nlevels(yVal) > 2) {
                                yvar <- call("reorder", yvar,
                                             call("unclass", xvar), na.rm=T)
                                yVal <- tryEval(yvar, dat)
                            }
                        }
                        xterms <- paste(c(deparseOneLine(xvar),
                                          deparseOneLine(yvar),
                                          if (!is.null(c1)) deparseOneLine(c1),
                                          if (!is.null(c2)) deparseOneLine(c2),
                                          if (!is.null(groups)) deparseOneLine(groups)),
                                        collapse=" + ")
                        xform <- as.formula(paste("~", xterms))
                        tabcall <- call("xtabs", xform, quote(dat), subset=subset)
                        if (doXProp || doYProp) {
                            mgn <- c(if (doXProp) 1, if (doYProp) 2)
                            tabcall <- call("prop.table", tabcall, margin = mgn)
                        }
                        callArg(playState, 1) <- tabcall
                        if (do3DTable) {
                            ## and set logical `groups` argument
                            callArg(playState, "groups") <- !is.null(groups)
                            callArg(playState, "panel.3d.cloud") <- quote(panel.3dbars)
                            callArg(playState, "col.facet") <- "grey"
                            callArg(playState, "xbase") <- 0.4
                            callArg(playState, "ybase") <- 0.4
                            ## rotate view 180 around z axis (better with reordering)
                            callArg(playState, "screen") <-
                                list(z = 180, z = 40, x = -60)
                            ## set aspect so that bars have square bases, like "iso"
                            ## aspect for cloud is in the form c(y/x, z/x)
                            asp.y.x <- length(levelsOK(yVal)) / length(levelsOK(xVal))
                            callArg(playState, "aspect") <- round(c(asp.y.x, min(asp.y.x, 1)), 2)
                            callArg(playState, quote(scales$z$draw)) <- FALSE
                            callArg(playState, "xlab") <- expression(NULL)
                            callArg(playState, "ylab") <- expression(NULL)
                            callArg(playState, "zlab") <- expression(NULL)
                                        # scales = list(rot = 90) # TODO use generic code below
                        } else {
                            ## levelplot: use color rather than depth for frequencies
                            callArg(playState, 0) <- quote(levelplot)
                        }
                    }
                } else

                if (is.categorical(yVal) || is.categorical(xVal)) {
                    ## BIVARIATE CATEGORICAL AND NUMERIC

                    ## TODO: if only one value for each level use dotplot

                    if (is.logical(xVal))
                        callArg(playState, "horizontal") <- FALSE

                    ## reorder factor levels if more than 2
                    if (is.categorical(yVal) && isUnordered(yvar, yVal)) {
                        if (nlevels(yVal) > 2) {
                            yvar <- call("reorder", yvar, xvar, na.rm=T)
                            yVal <- tryEval(yvar, dat)
                        }
                    }
                    if (is.categorical(xVal) && isUnordered(xvar, xVal)) {
                        if (nlevels(xVal) > 2) {
                            xvar <- call("reorder", xvar, yvar, na.rm=T)
                            xVal <- tryEval(xvar, dat)
                        }
                    }
                    ## formula
                    if (!is.null(cond))
                        callArg(playState, 1) <- call("~", yvar, call("|", xvar, cond))
                    else
                        callArg(playState, 1) <- call("~", yvar, xvar)

                    if (!is.null(groups)) {
                        callArg(playState, 0) <- quote(stripplot)
                        callArg(playState, "jitter.data") <- TRUE
                        if (doLines) {
                            callArg(playState, "type") <- c("p", "a")
                            ## TODO: check whether there are any missing values
                                        #callArg(playState, "fun") <- quote(median)
                            callArg(playState, "fun") <- quote(function(x) median(x, na.rm=TRUE))
                        }

                    } else {
                        callArg(playState, 0) <- quote(bwplot)
                                        #callArg(playState, "origin") <- 0
                        callArg(playState, "varwidth") <- FALSE
                    }

                } else {
                    ## BIVARIATE NUMERIC
                    callArg(playState, 0) <- quote(xyplot)
                    if (!is.null(cond))
                        callArg(playState, 1) <- call("~", yvar, call("|", xvar, cond))
                    else
                        callArg(playState, 1) <- call("~", yvar, xvar)

                    if (is.numeric(groupsVal) || do.gdisc) {
                        if (doTile) {
                            callArg(playState, 0) <- quote(tileplot)
                        } else {
                            callArg(playState, 0) <- quote(levelplot)
                            callArg(playState, "panel") <- quote(panel.levelplot.points)
                            callArg(playState, "prepanel") <- quote(prepanel.default.xyplot)
                        }
                        ## un-discretize groups
                        if (do.gdisc) groups <- groups[[2]]
                        form <- call("~", groups, call("*", xvar, yvar))
                        if (!is.null(cond))
                            form[[3]] <- call("|", form[[3]], cond)
                        callArg(playState, 1) <- form
                        callArg(playState, "groups") <- NULL
                    }

                    if (doLines) {
                        ## work out whether x data (in each panel) is spaced out
                        ## enough to join by lines (rather than smoothing, below)
                        guessPanelType <- function(panelx) {
                            ans <- list(lines=TRUE, jitter=FALSE)
                            if (!any(is.finite(panelx)))
                                return(ans)
                            rge <- range(panelx, na.rm=TRUE)
                            diffs <- diff(sort(panelx))
                            diffr <- range(diffs)
                            ans$lines <- FALSE
                            if (min(diffr) == 0) {
                                ans$jitter <- TRUE
                                posdiff <- min(diffs[diffs > 0])
                                ## join averages by lines if few discrete values
                                if (posdiff > diff(rge) / 30) {
                                    ans$lines <- TRUE
                                }
                            } else {
                                if ((max(diffr) - min(diffr) < getOption("ts.eps")) &&
                                    (min(diffr) > 2 * getOption("ts.eps"))) {
                                    ## regular differences -- likely time series
                                    ans$lines <- TRUE
                                } else {
                                    if (min(diffr) > diff(rge) / 100) {
                                        ans$lines <- TRUE
                                    }
                                }
                            }
                            ans
                        }
                        if (is.null(cond) && is.null(groups)) {
                            panelType <- guessPanelType(xVal[subsetVal])
                        } else {
                            ## split into packets by conditioning and/or groups
                            condList <- list()
                            if (!is.null(c1)) {
                                condList$c1 <- c1Val
                                if (!is.factor(c1Val) && !is.shingle(c1Val))
                                    condList$c1 <- as.factorOrShingle(c1Val)
                            }
                            if (!is.null(c2)) {
                                condList$c2 <- c2Val
                                if (!is.factor(c2Val) && !is.shingle(c2Val))
                                    condList$c2 <- as.factorOrShingle(c2Val)
                            }
                            if (!is.null(groups)) {
                                condList$groups <- groupsVal
                                if (!is.factor(groupsVal) && !is.shingle(groupsVal))
                                    condList$groups <- as.factorOrShingle(groupsVal)
                            }
                            if (!isTRUE(subset))
                                condList <- lapply(condList,
                                                   function(v) v[subsetVal, drop=TRUE])
                            nlevsList <- lapply(condList, nlevels)
                            packetDefs <- do.call(expand.grid, lapply(nlevsList, seq_len))
                            ## make a list for each packet (combination of cond levels)
                            packetDefs <- as.data.frame(t(packetDefs))
                            xSubVal <- if (isTRUE(subset)) xVal else xVal[subsetVal]
                            panelType <- sapply(packetDefs, function(packLev) {
                                id <- lattice:::compute.packet(condList, levels=packLev)
                                unlist(guessPanelType(xSubVal[id]))
                            })
                            panelType <- as.data.frame(t(panelType))
                        }

                        if (any(panelType$jitter)) {
                            callArg(playState, "jitter.x") <- TRUE
                            if (all(panelType$lines))
                                callArg(playState, "type") <- c("p", "a")
                        } else {
                            if (all(panelType$lines))
                                callArg(playState, "type") <- "o"
                        }

                        if (any(panelType$lines == FALSE)) {
                            ## use loess smoother
                            callArg(playState, "type") <- c("p", "smooth")
                            callArg(playState, "prepanel") <- quote(try.prepanel.loess)
                            ## do not worry about errors in loess.smooth
                            callArg(playState, quote(plot.args$panel.error)) <- "warning"
                        }
                    }
                }
            } else {
                ## TRIVARIATE

                if (doSegments) {
                    ## SEGMENTS
                    ## use segplot
                    callArg(playState, 0) <- quote(segplot)
                    if (is.categorical(xVal) && !is.categorical(yVal)) {
                        ## switch x and y, so categorical is on y axis
                        oldx <- xvar
                        xvar <- yvar
                        yvar <- oldx
                        oldx <- xVal
                        xVal <- yVal
                        yVal <- oldx
                    }
                    form <- call("~", yvar, call("+", xvar, zvar))
                    if (doAsError) {
                        ## symmetric additive error form
                        ## I(x-z) + I(x+z)
                        form[[3]] <- call("+", call("I", call("-", xvar, zvar)),
                                          call("I", call("+", xvar, zvar)))
                        callArg(playState, "centers") <- xvar
                        callArg(playState, "draw.bands") <- FALSE
                    }
                    if (!is.null(cond))
                        form[[3]] <- call("|", form[[3]], cond)
                    callArg(playState, 1) <- form
                    ## colors coded by "level"
                    if (do.gdisc) groups <- groups[[2]]
                    callArg(playState, "level") <- groups
                    callArg(playState, "groups") <- NULL

                } else {
                    ## TRIVARIATE 3D
                    ## use cloud
                    callArg(playState, 0) <- quote(cloud)

                    ## 3D NUMERIC (3D SCATTER)
                    form <- call("~", zvar, call("*", xvar, yvar))
                    if (!is.null(cond))
                        form[[3]] <- call("|", form[[3]], cond)
                    callArg(playState, 1) <- form
                    if (doLines)
                        callArg(playState, "type") <- c("p", "h")
                    ## TODO: support color covariate

                }
            }

            ## generic stuff...

            ## aspect and scales
                                        #if (identical(callArg(playState, 0, eval=FALSE), quote(cloud))) {
            if (is.call.to(mainCall(playState), "cloud")) { ## TODO: ok?
                ## for 3D plots, aspect widget applies to "panel.aspect".
                ## set panel.aspect to "fill" by default if only one panel
                if (is.null(aspect) && is.null(cond))
                    aspect <- "fill"
                if (identical(eval(aspect), "fill"))
                    aspect <- round(dev.size()[2] / dev.size()[1], 2)
                if (is.numeric(aspect))
                    callArg(playState, "panel.aspect") <- aspect
            } else {
                callArg(playState, "aspect") <- aspect ## may be NULL
            }

            if (!is.null(x.relation) || !is.null(y.relation)) {
                ## either of these may be NULL
                callArg(playState, quote(scales$x$relation)) <- x.relation
                callArg(playState, quote(scales$y$relation)) <- y.relation
            }

            anyNumerics <- ((!is.null(xvar) && !is.categorical(xVal)) ||
                            (!is.null(yvar) && !is.categorical(yVal)) ||
                            (!is.null(zvar)))
            anyNumerics <- (anyNumerics ||
                            is.call.to(mainCall(playState), "splom"))
            ## style settings for points
            if (anyNumerics) {
                theme <- call("simpleTheme")
                if (is.categorical(groupsVal))
                    theme$cex <- 0.6
                if (ncond > 2)
                    theme$cex <- 0.6
                if (ncond > 6)
                    theme$cex <- 0.4
                if ((nPoints >= LOTS) &&
                    is.null(callArg(playState, "f.value")))
                {
                    theme$alpha.points <- if (nPoints >= HEAPS) 0.15 else 0.3
                    ## bug in lattice: grouped lines take alpha from points setting
                    if (!is.null(groups) &&
                        (packageDescription("lattice")$Version <= "0.17-12")) ##fixed yet?
                        theme$alpha.points <- if (nPoints >= HEAPS) 0.4 else 0.6
                }
                if (nPoints >= HEAPS) {
                    if (is.call.to(mainCall(playState), "xyplot") ||
                        is.call.to(mainCall(playState), "stripplot")) {
                        theme$pch <- "." ## or 0, empty square?
                        theme$cex <- 3
                    }
                }
                callArg(playState, "par.settings") <- theme

            }
            ## add a grid if there are multiple panels
            if (anyNumerics && !is.null(cond)) {
                type <- callArg(playState, "type")
                if (is.null(type)) type <- "p" ## assumed
                type <- unique(c("g", type))
                callArg(playState, "type") <- type
            }

            ## set up key
            if (is.categorical(groupsVal))
            {
                auto.key <- list()
                ## work out key type
                typeVal <- callArg(playState, "type")
                if (all(c("p", "l") %in% typeVal)) {
                    typeVal <- c(setdiff(typeVal, c("p", "l")), "o")
                }
                ## all type values other than "p" and "g" imply lines
                if (any(typeVal %in% c("p", "g") == FALSE)) {
                    auto.key$lines <- TRUE
                    if (any(c("o", "b") %in% typeVal))
                        auto.key$type <- "o"
                    if (("p" %in% typeVal) == FALSE)
                        auto.key$points <- FALSE
                }
                ## get group levels that will appear in key
                levs <- levelsOK(groupsVal)
                ## if groups are discretised, need a title
                if (do.gdisc) {
                    auto.key$title <- groupsOrigStr
                    auto.key$cex.title <- 1
                }
                n.items <- length(levs)
                if (n.items > 1 && n.items <= 4)
                    auto.key$columns <- n.items
                if (n.items > 4)
                    auto.key$space <- "right"
                if (n.items == 4)
                    auto.key$between.columns <- 1
                if (n.items >= 3)
                    auto.key$cex <- 0.7
                ## TODO: check lengths of text, abbreviate?
                callArg(playState, "auto.key") <- auto.key
            }

            ## fix up long x axis labels
            if (is.categorical(xVal) &&
                !is.call.to(mainCall(playState), "histogram"))
            {
                if (nlevels(xVal) >= 4) {
                    scales <- callArg(playState, "scales")
                    if (!is.list(scales)) scales <- list()
                    if (max(sapply(levelsOK(xVal), nchar)) >= 12) {
                        scales$x$rot <- 30
                        scales$x$cex <- 0.7
                                        #scales$x$abbreviate <- TRUE
                    } else
                    if ((nlevels(xVal) >= 8) ||
                        (mean(sapply(levelsOK(xVal), nchar)) >= 8)) {
                        scales$x$rot <- 60
                    }
                    callArg(playState, "scales") <- scales
                }
            }

            callArg(playState, "subscripts") <- TRUE


            ## sub-title
            Rvers <- paste("R ", R.version$major, ".",
                           R.version$minor, sep="")
            subt <- if (nPoints > 0)
                paste("N = ", nPoints, ", ", sep="") else ""
            subt <- paste(subt, toString(Sys.Date()), ", ",
                          Rvers, sep="")
            if (!isTRUE(subset)) {
                subsetStr <- deparseOneLine(subset)
                if (nchar(subsetStr) > 30)
                    subt <- call("paste", subsetStr, subt, sep="\n")
                else subt <- call("paste", subsetStr, subt, sep=", ")
            }
            callArg(playState, "sub") <- list(subt, x=0.99, just="right",
                                              cex=0.7, font=1)

            ## check whether anything changed
            updateMainCall(playState) ## set canonical arguments
            if (identical(deparse(playState$call, control=NULL),
                          deparse(oldCall, control=NULL)))
                return()
            ## need playNewPlot not playReplot so as to reload latticist
            playNewPlot(playState)
        }
        doRecompose <- function(widget, playState)
            try(composePlot(playState))
        doRecomposeNewXY <- function(widget, playState)
            try(composePlot(playState, newXY=TRUE))
        doRecomposeOnSelect <- function(widget, playState) {
            if (widget["active"] > -1)
                doRecompose(playState = playState)
        }
        doRecomposeNewXYOnSelect <- function(widget, playState) {
            if (widget["active"] > -1)
                doRecomposeNewXY(playState = playState)
        }
        doLinesSetting <- function(widget, playState) {
            playState$latticist$linesSetting <- widget["active"]
            doRecompose(playState = playState)
        }

        handler.flip <- function(widget, event, playState) {
                                        #if (!isTRUE(playState$tmp$plot.ready)) {alarm(); return(FALSE)}
            playState$tmp$plot.ready <- FALSE
            xvarActive <- xvarW["active"]
            yvarActive <- yvarW["active"]
            xdisc <- xdiscW["active"]
            ydisc <- ydiscW["active"]
            xsens <- xonW["sensitive"]
            ysens <- yonW["sensitive"]
            xvarW["active"] <- yvarActive
            yvarW["active"] <- xvarActive
            xdiscW["active"] <- ydisc
            ydiscW["active"] <- xdisc
            xonW["sensitive"] <- ysens
            yonW["sensitive"] <- xsens
            playState$tmp$plot.ready <- TRUE
            doRecompose(playState = playState)
            return(FALSE)
        }
        handler.hexbin <- function(widget, event, playState) {
            playState$tmp$plot.ready <- FALSE
            xdiscW["active"] <- TRUE
            ydiscW["active"] <- TRUE
            playState$tmp$plot.ready <- TRUE
            doRecompose(playState = playState)
            return(FALSE)
        }
        handler.unbin <- function(widget, event, playState) {
            playState$tmp$plot.ready <- FALSE
            xdiscW["active"] <- FALSE
            ydiscW["active"] <- FALSE
            playState$tmp$plot.ready <- TRUE
            doRecompose(playState = playState)
            return(FALSE)
        }
        handler.superpose <- function(widget, event, playState) {
            playState$tmp$plot.ready <- FALSE
            c1Active <- c1W["active"]
            c2Active <- c2W["active"]
            if (c1Active <= 1) return(FALSE)
            if (c2Active <= 1) {
                groupsW["active"] <- c1Active
                c1W["active"] <- 0
            } else {
                groupsW["active"] <- c2Active
                c2W["active"] <- 0
            }
            widget["visible"] <- FALSE
            playState$tmp$plot.ready <- TRUE
            doRecompose(playState = playState)
            return(FALSE)
        }
        handler.explode <- function(widget, event, playState) {
            playState$tmp$plot.ready <- FALSE
            grActive <- groupsW["active"]
            if (grActive <= 1) return(FALSE)
            c1Active <- c1W["active"]
            c2Active <- c2W["active"]
            if (c1Active <= 1)
                c1W["active"] <- grActive
            else
                c2W["active"] <- grActive
            groupsW["active"] <- 0
            widget["visible"] <- FALSE
            playState$tmp$plot.ready <- TRUE
            doRecompose(playState = playState)
            return(FALSE)
        }
        handler.go3D <- function(widget, event, playState) {
            playState$latticist$do3DTable <- TRUE
            playState$tmp$plot.ready <- FALSE
            grActive <- groupsW["active"]
            if (grActive <= 1) return(FALSE)
            zvarW["active"] <- grActive
            groupsW["active"] <- 0
            widget["visible"] <- FALSE
            playState$tmp$plot.ready <- TRUE
            doRecompose(playState = playState)
            return(FALSE)
        }
        handler.squash <- function(widget, event, playState) {
            playState$latticist$do3DTable <- FALSE
            playState$tmp$plot.ready <- FALSE
            zActive <- zvarW["active"]
            if (zActive <= 1) return(FALSE)
            groupsW["active"] <- zActive
            zvarW["active"] <- 0
            widget["visible"] <- FALSE
            playState$tmp$plot.ready <- TRUE
            doRecompose(playState = playState)
            return(FALSE)
        }
        handler.gohyper <- function(widget, opt) {
            playState$latticist$defaultPlotType <- opt
            ## reset
            playState$tmp$plot.ready <- FALSE
            xvarW["active"] <- 0
            yvarW["active"] <- 0
            playState$tmp$plot.ready <- TRUE
            ## prompt for variables before plotting (can be slow with too many)
            handler.choosevars(playState = playState)
        }
        handler.choosevars <- function(widget, event, playState) {
            ## applies to hypervariate plots:
            ## splom, parallel and marginal.plot
            ## note, this is called by handler.gohyper
            vars <- names(dat)
            checked <- playState$latticist$varsubset
            if (!is.null(checked))
                checked <- vars %in% checked
            if (is.null(checked))
                checked <- TRUE
            theW <- ggroup(horizontal = FALSE)
            tmpg <- ggroup(horizontal = TRUE, container = theW)
            varsW <- gcheckboxgroup(vars, checked = checked, container = theW)
            gbutton("All", container = tmpg,
                    handler = function(h, ...) svalue(varsW) <- vars )
            gbutton("None", container = tmpg,
                    handler = function(h, ...) svalue(varsW) <- NULL )
            glabel(paste("NOTE: too many variables will result in a very slow",
                         "\nplot, especially so for splom (scatter plot matrix)."),
                   container = theW)
            gbasicdialog(title = "Choose variables to plot",
                         widget = theW,
                         handler = function(h, ...) {
                             varsub <- svalue(varsW)
                             if (identical(varsub, vars))
                                 varsub <- NULL
                             playState$latticist$varsubset <- varsub
                             dispose(h$obj)
                             doRecomposeNewXY(playState = playState)
                         })
        }
        ## TODO: this is currently disabled...
        handler.subsetSelect <- function(widget, event, playState) {
                                        #if (!isTRUE(playState$tmp$plot.ready)) {alarm(); return(FALSE)}
                                        #selectScales <- c(if (!is.null(xvar)) "x",
                                        #                  if (!is.null(yvar)) "y")
            foo <- playRectInput(playState, #scales=selectScales,
                                 prompt=paste("Click and drag to define a data subset",
                                 "(hold Shift to constrain),",
                                 "Right-click or Esc to cancel."))
            if (is.null(foo)) return(FALSE)
            if (is.null(foo$coords)) return(FALSE)
            if (foo$is.click) return(FALSE)
            coords <- foo$coords
            coords <- lapply(coords, signif, 5)
            xStr <- deparseOneLine(xvar)
            yStr <- deparseOneLine(yvar)
            xsub <- call("(",
                         call("&", call("<", min(coords$x), xvar),
                              call("<", xvar, max(coords$x))))
            ysub <- call("(",
                         call("&", call("<", min(coords$y), yvar),
                              call("<", yvar, max(coords$y))))
            ## construct factor subsets
            packet <- as.numeric(sub("packet ", "", foo$space))
            x.limits <- playState$trellis$x.limits
            y.limits <- playState$trellis$y.limits
            if (is.list(x.limits)) x.limits <- x.limits[[packet]]
            if (is.list(y.limits)) y.limits <- y.limits[[packet]]
            if (!is.null(xvar) && is.character(x.limits)) {
                limfrom <- max(1, ceiling(min(coords$x)))
                limto <- min(length(x.limits), max(coords$x))
                newlevels <- x.limits[seq(limfrom, limto)]
                xsub <- call("%in%", xvar, newlevels)
            }
            if (!is.null(yvar) && is.character(y.limits)) {
                limfrom <- max(1, ceiling(min(coords$y)))
                limto <- min(length(y.limits), max(coords$y))
                newlevels <- y.limits[seq(limfrom, limto)]
                ysub <- call("%in%", yvar, newlevels)
            }
            subset <- call("&", xsub, ysub)
            if (is.null(yvar)) subset <- xsub
            if (is.null(xvar)) subset <- ysub
            subsetW$getChild()$setText(deparseOneLine(subset))
            doRecompose(playState = playState)
            return(FALSE)
        }

        niceButton <- function(label) {
            butt <- gtkEventBox()
            tmp <- gtkLabel(label)
            tmp$setMarkup(paste('<span foreground="blue"><u>',
                                label, '</u></span>', sep=""))
            butt$add(tmp)
                                        #butt <- gtkButtonNew()
                                        #butt["relief"] <- GtkReliefStyle["none"]
                                        #butt["can-default"] <- FALSE
                                        #butt["height-request"] <- 12
                                        #butt$modifyStyle(GtkRcStyle ## TODO reduce padding
            butt
        }

        ## set up widgets
        box <- gtkHBox()

        ## TITLE, HYPERVAR, SUBSET
        setBox <- gtkVBox()
        titleBox <- gtkHBox()
        ## "help" button
        helpW <- niceButton("help")
        gSignalConnect(helpW, "button-press-event",
                       function(...) print(help("latticist")))
        titleBox$packStart(helpW, padding = 1, expand = FALSE)
        ## (Title)
        txt <- paste("Latticist",
                     packageDescription("playwith")$Version)
        titleW <- gtkLabel(txt)
        titleW$setMarkup(paste("<big><b><i>", txt, "</i></b></big>"))
        titleBox$packStart(titleW, expand = TRUE)
        setBox$packStart(titleBox, expand=FALSE)
        ## (prompt)
        promptxt <- paste('<span foreground="#666666">',
                          "Select variables to begin --&gt;",
                          '</span>', sep = "")
        promptW <- gtkLabel("")
        promptW$setMarkup(promptxt)
        promptW["visible"] <- (is.null(xvar) && is.null(yvar))
        ## SUBSET
        subsetBox <- gtkHBox()
        subsetBox$packStart(gtkLabel(" Subset: "), expand = FALSE)
        subsetBox$packEnd(promptW, expand = FALSE)
        ## "interactive" subset button
                                        #subsetSelW <- niceButton("interactive...")
                                        #showSub <- !is.null(xvar) || !is.null(yvar)
                                        #arg1 <- callArg(playState, 1, eval = FALSE)
                                        #if (isHypervar ||
                                        #    is.call.to(arg1, "xtabs") ||
                                        #    is.call.to(arg1, "prop.table") ||
                                        #    is.call.to(arg1, "margin.table"))
                                        #    showSub <- FALSE
                                        #subsetSelW["visible"] <- showSub
                                        #gSignalConnect(subsetSelW, "button-press-event",
                                        #               handler.subsetSelect, data=playState)
                                        #subsetBox$packStart(subsetSelW)
        setBox$packStart(subsetBox, expand=FALSE)
        ## subset
        subsetW <- gtkComboBoxEntryNewText()
        subsetW$show()
        subsetW["width-request"] <- -1
        for (item in subsetopts) subsetW$appendText(item)
        index <- match(deparseOneLine(subset), subsetopts)
        if (is.na(index)) index <- 1 ## should never happen
        subsetW["active"] <- (index - 1)
        ## "changed" emitted on typing and selection
        gSignalConnect(subsetW, "changed",
                       doRecomposeOnSelect, data=playState)
        gSignalConnect(subsetW$getChild(), "activate",
                       doRecompose, data=playState)
        setBox$packStart(subsetW, expand = FALSE)
        ## HYPERVAR
        hyperBox0 <- gtkHBox()
        hyperBox0$packStart(gtkLabel(" Hyper-variate plots: "),
                            expand = FALSE)
        ## "choose variables" button
        choosevarsW <- niceButton("choose variables")
        choosevarsW["visible"] <- isHypervar
        gSignalConnect(choosevarsW, "button-press-event",
                       handler.choosevars, data=playState)
        hyperBox0$packStart(choosevarsW)
        setBox$packStart(hyperBox0, expand=FALSE, padding = 1)
        ## hypervar reset buttons
        hyperBox <- gtkHBox()
        marginalsW <- gtkButton("marginals")
        marginalsW["tooltip-text"] <- "Show marginal distributions"
        splomW <- gtkButton("splom (pairs)")
        splomW["tooltip-text"] <- "Show a scatterplot matrix with all pairs of variables"
        parallelW <- gtkButton("parallel")
        parallelW["tooltip-text"] <- "Show a parallel coordinates plot"
        gSignalConnect(marginalsW, "clicked",
                       handler.gohyper, data = "marginals")
        gSignalConnect(splomW, "clicked",
                       handler.gohyper, data = "splom")
        gSignalConnect(parallelW, "clicked",
                       handler.gohyper, data = "parallel")
        hyperBox$packStart(marginalsW, expand = FALSE, padding = 2)
        hyperBox$packStart(splomW, expand = FALSE, padding = 2)
        hyperBox$packStart(parallelW, expand = FALSE, padding = 2)
        setBox$packStart(hyperBox, expand = FALSE)
        box$packStart(setBox, expand=FALSE, padding=1)

        box$packStart(gtkVSeparator(), expand=FALSE, padding=1)

        ## X Y VARS
        varsBox <- gtkVBox()
        xyBox <- gtkHBox()
        isBivarNumeric <-
            (!is.null(xvar) && !is.null(yvar) && is.null(zvar) &&
             !xcat && !ycat)
        isBivarCat <-
            (!is.null(xvar) && !is.null(yvar) && is.null(zvar) &&
             xcat && ycat)
        labtxt <- " Variables / expressions on axes: "
        #if (isBivarNumeric) ## abbreviate
        #    labtxt <- " Variables on axes: "
        xyBox$packStart(gtkLabel(labtxt), expand=FALSE)
        ## "switch" button
        xyflipW <- niceButton("switch")
        xyflipW["visible"] <- !is.null(xvar) || !is.null(yvar)
        gSignalConnect(xyflipW, "button-press-event",
                       handler.flip, data=playState)
        xyBox$packStart(xyflipW, padding = 2)
        ## "hexbin" button
        hexbinW <- niceButton("hexbin")
        hexbinW["visible"] <- (isBivarNumeric && !xdisc && !ydisc &&
                               require("hexbin", quietly = TRUE))
        gSignalConnect(hexbinW, "button-press-event",
                       handler.hexbin, data=playState)
        xyBox$packStart(hexbinW, padding = 2)
        ## "points" button
        unbinW <- niceButton("points")
        unbinW["visible"] <- isBivarNumeric && xdisc && ydisc
        gSignalConnect(unbinW, "button-press-event",
                       handler.unbin, data=playState)
        xyBox$packStart(unbinW, padding = 2)
        ## "go 2D" button
        squashW <- niceButton("go 2D")
        squashW["visible"] <- (isBivarCat &&
                               playState$callName == "cloud")
        gSignalConnect(squashW, "button-press-event",
                       handler.squash, data=playState)
        xyBox$packStart(squashW)
        ## "go 3D" button
        go3DW <- niceButton("go 3D")
        go3DW["visible"] <- (isBivarCat &&
                             playState$callName == "levelplot")
        gSignalConnect(go3DW, "button-press-event",
                       handler.go3D, data=playState)
        xyBox$packStart(go3DW)
        varsBox$packStart(xyBox, expand=FALSE)
        ## Y VAR
        yvarBox <- gtkHBox()
        yonW <- gtkCheckButton("y= ")
        yonW["active"] <- TRUE
        yonW["sensitive"] <- !is.null(yvar)
        gSignalConnect(yonW, "clicked",
                       doRecomposeNewXY, data=playState)
        yvarBox$packStart(yonW, expand=FALSE)
        yvarW <- gtkComboBoxEntryNewText()
        yvarW$show()
        yvarW["width-request"] <- 100
        for (item in varexprs) yvarW$appendText(item)
        index <- match(deparseOneLine(yvar), varexprs)
        if (is.na(index)) index <- 1
        yvarW["active"] <- (index - 1)
        ## "changed" emitted on typing and selection
        gSignalConnect(yvarW, "changed",
                       doRecomposeNewXYOnSelect, data=playState)
        gSignalConnect(yvarW$getChild(), "activate",
                       doRecomposeNewXY, data=playState)
        yvarBox$packStart(yvarW)
        ## discretize -- for numerics
        ydiscW <- gtkCheckButton("discretize")
        ydiscW["active"] <- ydisc
        ydiscW["sensitive"] <- !is.null(yvar)
        ydiscW["visible"] <- !ycat
        gSignalConnect(ydiscW, "clicked",
                       doRecomposeNewXY, data=playState)
        yvarBox$packStart(ydiscW, expand=FALSE)
        ## "proportions" -- for categoricals
        ypropW <- gtkCheckButton("proportions")
        ypropW["active"] <- yprop
        ypropW["visible"] <- ycat
        gSignalConnect(ypropW, "clicked",
                       doRecompose, data=playState)
        yvarBox$packStart(ypropW, expand=FALSE)
        varsBox$packStart(yvarBox, expand=FALSE)

        ## X VAR
        xvarBox <- gtkHBox()
        xonW <- gtkCheckButton("x= ")
        xonW["active"] <- TRUE
        xonW["sensitive"] <- !is.null(xvar)
        gSignalConnect(xonW, "clicked",
                       doRecomposeNewXY, data=playState)
        xvarBox$packStart(xonW, expand=FALSE)
        xvarW <- gtkComboBoxEntryNewText()
        xvarW$show()
        xvarW["width-request"] <- 100
        for (item in varexprs) xvarW$appendText(item)
        index <- match(deparseOneLine(xvar), varexprs)
        if (is.na(index)) index <- 1
        xvarW["active"] <- (index - 1)
        ## "changed" emitted on typing and selection
        gSignalConnect(xvarW, "changed",
                       doRecomposeNewXYOnSelect, data=playState)
        gSignalConnect(xvarW$getChild(), "activate",
                       doRecomposeNewXY, data=playState)
        xvarBox$packStart(xvarW)
        ## "discretize" -- for numerics
        xdiscW <- gtkCheckButton("discretize")
        xdiscW["active"] <- xdisc
        xdiscW["sensitive"] <- !is.null(xvar)
        xdiscW["visible"] <- !xcat
        gSignalConnect(xdiscW, "clicked",
                       doRecomposeNewXY, data=playState)
        xvarBox$packStart(xdiscW, expand=FALSE)
        ## "proportions" -- for categoricals
        xpropW <- gtkCheckButton("proportions")
        xpropW["active"] <- xprop
        xpropW["visible"] <- xcat
        gSignalConnect(xpropW, "clicked",
                       doRecompose, data=playState)
        xvarBox$packStart(xpropW, expand=FALSE)
        varsBox$packStart(xvarBox, expand=FALSE)

        ## XY OPTS
        xyOptsBox <- gtkHBox()
        ## ASPECT
        xyOptsBox$packStart(gtkLabel(" Aspect:"), expand=FALSE)
        aspectW <- gtkComboBoxEntryNewText()
        aspectW$show()
        aspectW["sensitive"] <- !is.null(xvar) || !is.null(yvar)
        aspectW["width-request"] <- 50
        for (item in aspectopts) aspectW$appendText(item)
        if (!is.null(aspectVal)) {
            index <- match(aspectVal, aspectopts)
            if (is.na(index)) index <- 0
            aspectW["active"] <- (index - 1)
        }
        ## "changed" emitted on typing and selection
        gSignalConnect(aspectW, "changed",
                       doRecomposeOnSelect, data=playState)
        gSignalConnect(aspectW$getChild(), "activate",
                       doRecompose, data=playState)
        xyOptsBox$packStart(aspectW)
        xyOptsBox$packStart(gtkLabel(""), padding=1)
        ## LINES
        linesW <- gtkCheckButton("Lines. ")
        linesW["active"] <- playState$latticist$linesSetting
        linesW["sensitive"] <- !is.null(xvar) || !is.null(yvar)
        gSignalConnect(linesW, "clicked",
                       doLinesSetting, data=playState)
        xyOptsBox$packStart(linesW, expand=FALSE)
        xyOptsBox$packStart(gtkLabel(""), padding=1)
        ## LEVELS
        xyOptsBox$packStart(gtkLabel("Levels:"), expand=FALSE)
        nLevelsW <- gtkSpinButton(min=1, max=16, step=1)
        nLevelsW["width-request"] <- 40
        nLevelsW["digits"] <- 0
        nLevelsW$setValue(nLevels)
        gSignalConnect(nLevelsW, "value-changed",
                       doRecompose, data=playState)
        xyOptsBox$packStart(nLevelsW, expand=FALSE)
        varsBox$packStart(xyOptsBox, expand=FALSE, padding=1)
        box$packStart(varsBox, padding=1)

        box$packStart(gtkVSeparator(), expand=FALSE, padding=1)

        ## GROUPS / Z VARIABLE
        gzBox <- gtkVBox()
        ## GROUPS
        groupsBox <- gtkHBox()
        groupsBox$packStart(gtkLabel(" Groups / Color: "), expand=FALSE)
        ## "explode" button
        explodeW <- niceButton("explode")
        explodeW["visible"] <- !is.null(groups) && gcat
        gSignalConnect(explodeW, "button-press-event",
                       handler.explode, data=playState)
        groupsBox$packStart(explodeW)
        ## "go 3D" button
        go3DW <- niceButton("go 3D")
        go3DW["visible"] <- ((!is.null(groups) && !gcat) ||
                             (playState$callName == "levelplot"))
        gSignalConnect(go3DW, "button-press-event",
                       handler.go3D, data=playState)
        groupsBox$packStart(go3DW)
        gzBox$packStart(groupsBox, expand=FALSE, padding=1)
        ## groups
        gBox <- gtkHBox()
        groupsW <- gtkComboBoxEntryNewText()
        groupsW$show()
        groupsW["sensitive"] <- (playState$callName != "marginal.plot")
        groupsW["width-request"] <- 100
        for (item in varexprs) groupsW$appendText(item)
        index <- match(deparseOneLine(groups), varexprs)
        if (is.na(index)) index <- 1
        groupsW["active"] <- (index - 1)
        ## "changed" emitted on typing and selection
        gSignalConnect(groupsW, "changed",
                       doRecomposeOnSelect, data=playState)
        gSignalConnect(groupsW$getChild(), "activate",
                       doRecompose, data=playState)
        gBox$packStart(groupsW)
        ## tile
        tileW <- gtkCheckButton("tile")
        tileW["active"] <- tileVal
        tileW["visible"] <- !is.null(groups) && !gcat
        gSignalConnect(tileW, "clicked",
                       doRecompose, data=playState)
        gBox$packStart(tileW)
        gzBox$packStart(gBox, expand=FALSE)

        ## Z / SEGMENTS VARIABLE
        zBox <- gtkHBox()
        zBox$packStart(gtkLabel(" Depth (3D) or"), expand=FALSE)
        ## segments option
        segmentsW <- gtkCheckButton("Segments (x--z) ")
        segmentsW["visible"] <-
        segmentsW["active"] <- segmentsVal
        segmentsW["sensitive"] <- !is.null(xvar) && !is.null(yvar)
        gSignalConnect(segmentsW, "clicked",
                       doRecomposeNewXY, data=playState)
        zBox$packStart(segmentsW, expand=FALSE)
        ## "squash" button
        squashW <- niceButton("squash")
        squashW["visible"] <- !is.null(zvar)
        gSignalConnect(squashW, "button-press-event",
                       handler.squash, data=playState)
        zBox$packStart(squashW)
        gzBox$packStart(zBox, expand=FALSE, padding=1)
        ## z (3D depth)
        zvarBox <- gtkHBox()
        zvarBox$packStart(gtkLabel(" z= "), expand = FALSE)
        zvarW <- gtkComboBoxEntryNewText()
        zvarW$show()
        zvarW["sensitive"] <- !is.null(xvar) && !is.null(yvar)
        zvarW["width-request"] <- 100
        for (item in varexprs) zvarW$appendText(item)
        index <- match(deparseOneLine(zvar), varexprs)
        if (is.na(index)) index <- 1
        zvarW["active"] <- (index - 1)
        ## "changed" emitted on typing and selection
        gSignalConnect(zvarW, "changed",
                       doRecomposeOnSelect, data=playState)
        gSignalConnect(zvarW$getChild(), "activate",
                       doRecompose, data=playState)
        zvarBox$packStart(zvarW)
        ## asError option
        aserrorW <- gtkCheckButton("as error") # (x+/-z)")
        aserrorW["active"] <- aserrorVal
        aserrorW["visible"] <- segmentsVal
        gSignalConnect(aserrorW, "clicked",
                       doRecompose, data=playState)
        zvarBox$packStart(aserrorW)
        gzBox$packStart(zvarBox, expand=FALSE)
        box$packStart(gzBox, expand=FALSE, padding=1)

        box$packStart(gtkVSeparator(), expand=FALSE, padding=1)

        ## CONDITIONING VARS
        cvarsBox <- gtkVBox()
        cBox <- gtkHBox()
        cBox$packStart(gtkLabel(" Conditioning: "), expand=FALSE)
        ## "superpose" button
        superposeW <- niceButton("superpose")
        superposeW["visible"] <- !is.null(c1)
        gSignalConnect(superposeW, "button-press-event",
                       handler.superpose, data=playState)
        cBox$packStart(superposeW)
        cvarsBox$packStart(cBox, expand=FALSE, padding=1)
        ## first conditioning variable
        c1Box <- gtkHBox()
        c1W <- gtkComboBoxEntryNewText()
        c1W$show()
        c1W["sensitive"] <- (playState$callName != "marginal.plot")
        c1W["width-request"] <- 100
        for (item in varexprs) c1W$appendText(item)
        index <- match(deparseOneLine(c1), varexprs)
        if (is.na(index)) index <- 1
        c1W["active"] <- (index - 1)
        ## "changed" emitted on typing and selection
        gSignalConnect(c1W, "changed",
                       doRecomposeOnSelect, data=playState)
        gSignalConnect(c1W$getChild(), "activate",
                       doRecompose, data=playState)
        c1Box$packStart(c1W)
        cvarsBox$packStart(c1Box, expand=FALSE)
        ## second conditioning variable
        c2Box <- gtkHBox()
        c2W <- gtkComboBoxEntryNewText()
        c2W$show()
        c2W["sensitive"] <- !is.null(c1)
        c2W["width-request"] <- 100
        for (item in varexprs) c2W$appendText(item)
        index <- match(deparseOneLine(c2), varexprs)
        if (is.na(index)) index <- 1
        c2W["active"] <- (index - 1)
        ## "changed" emitted on typing and selection
        gSignalConnect(c2W, "changed",
                       doRecomposeOnSelect, data=playState)
        gSignalConnect(c2W$getChild(), "activate",
                       doRecompose, data=playState)
        c2Box$packStart(c2W)
        cvarsBox$packStart(c2Box, expand=FALSE)
        ## SCALES
        scalesBox <- gtkHBox()
        scalesBox$packStart(gtkLabel("Scales:"), expand=FALSE)
        scalesW <- gtkComboBoxNewText()
        scalesW$show()
        scalesW["sensitive"] <- (prod(dim(playState$trellis)) > 1)
        scalesW["width-request"] <- 80
        for (item in scalesopts) scalesW$appendText(item)
        if (!is.null(scalesVal)) {
            index <- match(scalesVal, scalesopts)
            if (is.na(index)) index <- 0
            scalesW["active"] <- (index - 1)
        }
        ## "changed" emitted on typing and selection
        gSignalConnect(scalesW, "changed",
                       doRecomposeOnSelect, data=playState)
        scalesBox$packStart(scalesW)
        cvarsBox$packStart(scalesBox, expand=FALSE, padding=1)
        box$packStart(cvarsBox, padding=1)

        ## add it directly to the window (not a toolbar!)
        if (!is.null(playState$widgets$latticist))
            playState$widgets$latticist$destroy()
        playState$widgets$latticist <- box

        playState$widgets$vbox$packEnd(box, expand=FALSE)

        return(NA)
    }
}

is.call.to <- function(x, name)
    is.call(x) && identical(x[[1]], as.symbol(name))

is.categorical <- function(x)
    is.factor(x) || is.shingle(x) || is.character(x) || is.logical(x)

levelsOK <- function(x) {
    if (is.logical(x)) return(c(TRUE, FALSE))
    levels(x)
}

reorderByFreq <- function(x) {
    reorder(x, x, function(z) -length(z))
}

cutEq <- function(x, n, type=2, dig.lab=4, ...)
{
    stopifnot(length(n) == 1)
    br <- quantile(x, seq(0, 1, length=n+1), type=type,
                   na.rm=TRUE, names=FALSE)
    br[length(br)] <- max(x, na.rm=TRUE)
    br <- unique(br)
    cut(x, br, dig.lab=dig.lab, right=FALSE,
        include.lowest=TRUE, ordered_result=TRUE)
}

try.prepanel.loess <- function(...) {
    result <- try(prepanel.loess(...), silent=TRUE)
    if (inherits(result, "try-error"))
        return(list())
    result
}

levelplot.table <-
    function(x, data = NULL, ...)
{
    if (!is.null(data))
        warning("explicit 'data' specification ignored")
    stopifnot(length(dim(x)) >= 2)
    data <- as.data.frame(x)
    nms <- names(data)
    freq <- which(nms == "Freq")
    nms <- nms[-freq]
    form <- sprintf("Freq ~ %s * %s", nms[1], nms[2])
    nms <- nms[-(1:2)]
    len <- length(nms)
    if (len > 0) {
        rest <- paste(nms, collapse = "+")
        form <- paste(form, rest, sep = "|")
    }
    levelplot(as.formula(form), data, ...)
}

layer.col <-
    function(x, pch = 21, col = "transparent",
             ...)
{
    layerCol <- level.colors(x, at = do.breaks(range(x), 30))
    expr <- bquote(panel.xyplot(..., pch = .(pch), col = .(col),
                                fill = layerCol[subscripts]))
    eval(call("layer", expr, data = list(layerCol = layerCol)))
}

.profLatticist <- function(n = 20000) {
    audit <- read.csv(system.file("csv", "audit.csv", package = "rattle"))
    audit <- lapply(audit, rep, length.out=n)
    gc()
    Rprof(tmp <- tempfile())
    latticist(audit)
    Rprof()
    print(summaryRprof(tmp))
    unlink(tmp)
}

