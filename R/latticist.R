## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

## audit <- read.csv(system.file("csv", "audit.csv", package = "rattle"))
## latticist(audit)

latticist <-
    function(dat, plot.call = quote(marginals(dat)), ...)
{
    title <- paste("Latticist:",
                   toString(deparse(substitute(dat)), width=24))

    dat <- as.data.frame(dat)

    playwith(plot.call = plot.call,
             title=title, ...,
             bottom.tools=list(latticist=makeLatticistTool(dat)))
    invisible(playDevCur())
}

marginals <-
    function(data,
             reorder = TRUE,
             subset = TRUE,
             ref = TRUE,
             levels.fos = NULL,
             plot.points = FALSE,
             xlab = NULL, ylab = NULL,
             cex = 0.5,
             ...,
             as.table = TRUE,
             subscripts = TRUE,
             default.scales = list(relation="free", draw=FALSE))
{
    data <- as.data.frame(data)
    nvar <- ncol(data)
    factors <- sapply(data, is.categorical)
    ## apply subset
    subset <- eval(substitute(subset), data)
    if (!isTRUE(subset)) data <- data[subset,]
    if (any(factors)) {
        facdat <- lapply(data[factors], function(Value)
                         as.data.frame(table(Value)) )
        facdat <- do.call(make.groups, facdat)
        ## order packets by number of levels, same effect as index.cond
        facdat$which <- with(facdat, reorder(which, which, quote(length)))
        ## reorder factor levels within each group
        if (reorder) facdat$Value <-
            with(facdat, reorder(reorder(Value, -Freq), as.numeric(which)))
        ## make trellis object for factors
        factobj <-
            dotplot(Freq ~ Value | which, data=facdat, subscripts=TRUE,
                    ...,
                    type=c("p","h"), cex=cex, ref=ref,
                    levels.fos = levels.fos,
                    prepanel = function(...) {
                        result <- prepanel.default.xyplot(...)
                        ## ensure zero is included on scale
                        list(ylim = range(c(result$ylim, 0)))
                    },
                    as.table = as.table,
                    default.scales = default.scales,
                    xlab=xlab, ylab=ylab)
        if (all(factors)) return(factobj)
    }
    if (any(!factors)) {
        numdat <- do.call(make.groups, data[!factors])
        ## order packets by mean, same effect as index.cond
        numdat$which <- with(numdat, reorder(which, data, mean, na.rm=TRUE))
        ## make trellis object for numerics
        numobj <-
            densityplot(~ data | which, data=numdat, subscripts=TRUE,
                        ...,
                        plot.points=plot.points, ref=ref,
                        as.table = as.table,
                        default.scales = default.scales,
                        xlab=xlab, ylab=ylab)
        if (FALSE)
            qqmath(~ data | which, data=numdat, subscripts=TRUE,
                   ...,
                   distribution=qunif,
                   f.value=ppoints(100),
                   type=c("p", "l"), pch=".", cex=2,
                   as.table = as.table,
                   default.scales = default.scales,
                   xlab=xlab, ylab=ylab)

        if (all(!factors)) return(numobj)
    }
    ## construct trellis object with combined layout but no data
    ## This stores the two trellis objects and uses their
    ## prepanel and panel functions.
    ## It would be nicer to actually merge the trellis objects
    ## (i.e. merge $panel.args, $x.limits, $y.limits, etc)
    ## -- would still need custom panel and prepanel functions
    nfactors <- sum(factors)
    pktnames <- c(dimnames(factobj)$which,
              dimnames(numobj)$which)
    pktnames <- factor(pktnames, levels=pktnames)
    dummyobj <-
        xyplot(1:ncol(data) ~ 1:ncol(data) | pktnames, subscripts=TRUE,
               ...,
               OBJ1 = factobj, OBJ2 = numobj, OFFSET = nfactors,
               prepanel = function(x, ..., OBJ1, OBJ2, OFFSET) {
                   n <- x ## which is packet.number()
                   obj <- OBJ1
                   if (n > OFFSET) {
                       obj <- OBJ2
                       n <- n - OFFSET
                   }
                   ## can not use default prepanel with mixed fac/num
                   xlim <- obj$x.num.limit[[n]]
                   ylim <- obj$y.num.limit[[n]]
                   if (any(is.na(xlim))) xlim <- obj$x.limits[[n]]
                   if (any(is.na(ylim))) ylim <- obj$y.limits[[n]]
                   ## need to apply axis padding to factors manually
                   pad <- lattice.getOption("axis.padding")$factor
                   if (is.categorical(obj$x.limits[[n]]))
                       xlim <- xlim + ifelse(is.unsorted(xlim), -1, 1) *
                           c(-pad, pad)
                   list(xlim=xlim, ylim=ylim)
               },
               panel = function(..., OBJ1, OBJ2, OFFSET) {
                   n <- packet.number()
                   obj <- OBJ1
                   if (n > OFFSET) {
                       obj <- OBJ2
                       n <- n - OFFSET
                   }
                   panel <- obj$panel
                   if (is.character(panel)) panel <- get(panel)
                   do.call(panel, trellis.panelArgs(obj, n))
               },
               as.table = as.table,
               default.scales = default.scales,
               xlab=xlab, ylab=ylab)

    dummyobj
}


makeLatticistTool <- function(dat)
{
    is.call.to <- function(x, name)
        is.call(x) && identical(x[[1]], as.symbol(name))
    isInteraction <- function(x)
        is.call.to(x, "*") || is.call.to(x, "+")
    isUnordered <- function(x, val) {
        ## need this because is.ordered(cut()) == FALSE!
        if (is.call.to(x, "cut")) return(FALSE)
        ## assumes is.categorical(val)
        !is.ordered(val) && !is.shingle(val)
    }

    function(playState)
    {
        ## create list to store some settings
        if (is.null(playState$latticist))
            playState$latticist <- list()
        ## get arguments to current call
        callName <- toString(callArg(playState, 0, eval=FALSE))
        arg1 <- callArg(playState, 1, eval=FALSE)
        groups <- callArg(playState, "groups", eval=FALSE)
        if (isTRUE(groups) || identical(groups, FALSE))
            groups <- NULL
        subset <- callArg(playState, "subset", eval=FALSE)

        LOTS <- 800
        HEAPS <- 5000

        discFun <- "cut"    # or "equal.count"
        ## note: xtabs can't handle equal.count(?)

        ## parse variables from existing plot call
        xvar <- yvar <- c1 <- c2 <- NULL
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
            isxy <- FALSE
            if (is.call.to(arg1, "prop.table")) {
                arg1 <- arg1[[2]]
                isxy <- TRUE
            }
            xform <- arg1[[2]]
            ## parse xtabs formula
            vars.expr <- attr(terms(eval(xform)), "variables")
            vars <- as.list(vars.expr)[-1]
            if (isxy) {
                yvar <- vars[[1]]
                if (length(vars) >= 2) {
                    xvar <- vars[[length(vars)]]
                    vars <- vars[-length(vars)]
                }
            } else {
                if (identical(callArg(playState, "horizontal"), FALSE)) {
                    ## variable is on x axis
                    xvar <- vars[[1]]
                } else {
                    ## variable is on y axis
                    yvar <- vars[[1]]
                }
                if (!identical(groups, FALSE)) {
                    if (length(vars) >= 2) {
                        groups <- vars[[length(vars)]]
                        vars <- vars[-length(vars)]
                    }
                }
            }
            if (length(vars) >= 2) c1 <- vars[[2]]
            if (length(vars) >= 3) c2 <- vars[[3]]
            subset <- arg1$subset
        } else {
            ## unrecognised object

        }

        ## strip discretization code for display
        nLevels <- 4 ## default
        nlevset <- FALSE
        stripDisc <- function(x, env=parent.frame()) {
            if (is.call.to(x, discFun) &&
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
        groups <- stripDisc(groups)
        ## histogram by convention has x discretized
        if (callName == "histogram") {
            xdisc <- TRUE
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

        ## set up variables and options
        xvarStr <- deparseOneLine(xvar)
        yvarStr <- deparseOneLine(yvar)
        c1Str <- deparseOneLine(c1)
        c2Str <- deparseOneLine(c2)
        groupsStr <- deparseOneLine(groups)

        iscat <- sapply(dat, is.categorical)

        ## variables and expressions
        varnames <- c("NULL", names(dat),
                      xvarStr, yvarStr,
                      c1Str, c2Str, groupsStr)
        varnames <- unique(varnames)
        NULLNAMES <- c("(none)", "")
        varnames[[1]] <- NULLNAMES[[1]]

        ## TODO: preload log() of positive numerics
        ## TODO: preload is.na() of vars with missing values
        ## TODO: split into numeric and categorical in lists

        ## subset
        subsetopts <- playState$latticist$subsets
        if (is.null(subsetopts)) {
            ## preload some useful subsets
            subsetopts <- c(NULLNAMES[[1]],
                            "complete.cases(dat)")
            ## preload factor levels (only most frequent two of each)
            toplev <- lapply(names(dat)[iscat], function(nm) {
                tmp <- names(sort(table(dat[[nm]]), decreasing=TRUE))
                tmp <- tmp[seq_len(min(2, length(tmp)))] ## top 2
                paste(nm, "==", sapply(tmp, deparse))
            })
            subsetopts <- c(subsetopts, unlist(toplev))
            ## TODO: preload is.finite() of numerics with missing values
            ## TODO: preload > 0 of relevant numerics
        }
        subsetopts <-
            unique(c(subsetopts, if (!is.null(subset) && !isTRUE(subset))
                     deparseOneLine(subset)))
        playState$latticist$subsets <- subsetopts

        ## aspect
        aspectVal <- callArg(playState, "aspect")
        aspectopts <- c('"fill"', '"iso"', '"xy"',
                        "0.5", "1", "2")
        if (!is.null(aspectVal))
            aspectopts <-
                unique(c(aspectopts, deparseOneLine(aspectVal)))
                                        #playState$trellis$aspect.ratio
                                        #playState$trellis$aspect.fill
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

        ## labels
        labelsopts <- c("rownames(dat)", "1:nrow(dat)", names(dat))


        tryParse <- function(x) {
            if (x %in% NULLNAMES) x <- "NULL"
            result <- tryCatch(parse(text=x)[[1]], error=function(e)e)
            ## check whether there was a syntax error
            if (inherits(result, "error")) {
                gmessage.error(conditionMessage(result))
                stop(result)
            }
            result
        }

        ## generate formula and other args from widget settings
        composePlot <- function(playState, newXY=FALSE) {
            if (!isTRUE(playState$plot.ready)) return()
            ## parse variables / expressions
            xvar <- tryParse(xvarW$getActiveText())
            yvar <- tryParse(yvarW$getActiveText())
            if (xonW[["active"]] == FALSE) xvar <- NULL
            if (yonW[["active"]] == FALSE) yvar <- NULL
            c1 <- tryParse(c1W$getActiveText())
            c2 <- tryParse(c2W$getActiveText())
            groups <- tryParse(groupsW$getActiveText())
            ## keep for titles etc
            xvarOrigStr <- deparseOneLine(xvar)
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
            xVal <- eval(xvar, dat)
            yVal <- eval(yvar, dat)
            c1Val <- eval(c1, dat)
            c2Val <- eval(c2, dat)
            groupsVal <- eval(groups, dat)
            subsetVal <- eval(subset, dat)
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
            nlev <- nLevelsW[["value"]]
            do.xdisc <- (is.numeric(xVal) && xdiscW[["active"]])
            do.ydisc <- (is.numeric(yVal) && ydiscW[["active"]])
            do.c1disc <- (is.numeric(c1Val))
            do.c2disc <- (is.numeric(c2Val))
            do.gdisc <- (is.numeric(groupsVal))
            if (do.xdisc) xvar <- call(discFun, xvar, nlev)
            if (do.ydisc) yvar <- call(discFun, yvar, nlev)
            if (do.c1disc) c1 <- call(discFun, c1, nlev)
            if (do.c2disc) c2 <- call(discFun, c2, nlev)
            if (do.gdisc) groups <- call(discFun, groups, nlev)

            ## re-evaluate data if changed
            if (do.xdisc) xVal <- eval(xvar, dat)
            if (do.ydisc) yVal <- eval(yvar, dat)
            if (do.c1disc) c1Val <- eval(c1, dat)
            if (do.c2disc) c2Val <- eval(c2, dat)
            if (do.gdisc) groupsVal <- eval(groups, dat)

            ## if only one conditioning term, call it c1
            if (is.null(c1) && !is.null(c2)) {
                c1 <- c2
                c1Val <- c2Val
                c2 <- c2Val <- NULL
            }

            ## reorder factor levels of groups
            if (!is.null(groupsVal)) {
                if (isUnordered(groups, groupsVal)) {
                    groups <- call("reorderByFreq", groups)
                    groupsVal <- eval(groups, dat)
                }
            }

            ## reorder conditioning (TODO: index.cond)
            if (!is.null(c1Val)) {
                if (isUnordered(c1, c1Val)) {
                    c1 <- call("reorderByFreq", c1)
#                    if (!is.null(yVar) || !is.null(xVar)) {
#                        c1 <- call("reorder", c1,
#                                   if (!is.null(yVal)) yvar else xvar)
#                    }
                    c1Val <- eval(c1, dat)
                }
            }

            ## combined conditioning term (may be NULL)
            cond <- c1
            if (!is.null(c2)) cond <- call("*", c1, c2)

            ## create plot call
            oldCall <- mainCall(playState)
            mainCall(playState) <- quote(xyplot(0 ~ 0, data=dat))
            callArg(playState, "subset") <- subset #if (!isTRUE(subset)) subset ## interferes with "sub"!
            callArg(playState, "groups") <- groups
            callArg(playState, "subscripts") <- TRUE

            ## TODO: layout, if too many panels
            ## TODO: set up titles

            ## choose plot type and formula
            if (is.null(xVal) && is.null(yVal)) {
                ## NO VARIABLES CHOSEN
                mainCall(playState) <- quote(marginals(dat))
                callArg(playState, "subset") <- subset
                                        #if (!isTRUE(subset)) subset else NULL

            } else if (is.null(xVal) || is.null(yVal)) {
                ## UNIVARIATE

                if (is.categorical(xVal) || is.categorical(yVal)) {
                    ## UNIVARIATE CATEGORICAL
                    if (!is.null(yVal)) {
                        ## data on y axis, use dotplot
                        callArg(playState, 0) <- quote(dotplot)
                        callArg(playState, "data") <- NULL
                        callArg(playState, "subset") <- NULL
                        ## reorder factor levels
                        if (isUnordered(yvar, yVal)) {
                            yvar <- call("reorderByFreq", yvar)
                            yVal <- eval(yvar, dat)
                        }
                        xterms <- paste(c(deparseOneLine(yvar),
                                          if (!is.null(c1)) deparseOneLine(c1),
                                          if (!is.null(c2)) deparseOneLine(c2),
                                          if (!is.null(groups)) deparseOneLine(groups)),
                                        collapse=" + ")
                        ## and set logical `groups` argument
                        callArg(playState, "groups") <- !is.null(groups)
                        xform <- as.formula(paste("~", xterms))
                        callArg(playState, 1) <-
                            call("xtabs", xform, quote(dat), subset=subset)
                        if (!is.null(groups))
                            callArg(playState, "type") <- "o"
                        else
                            callArg(playState, "type") <- c("p", "h")
                        callArg(playState, "origin") <- 0
                    } else {
                        ## data on x axis, use barchart
                        ## (just for variety? & dotplot.table has no horizontal=FALSE)
                        ## BUT if x is a discretized numeric, use histogram
                        if (do.xdisc) {
                            xvar <- xvar[[2]]
                            callArg(playState, 0) <- quote(histogram)
                            if (!is.null(cond))
                                callArg(playState, 1) <- call("~", call("|", xvar, cond))
                            else
                                callArg(playState, 1) <- call("~", xvar)
                        } else {
                            callArg(playState, 0) <- quote(barchart)
                            callArg(playState, "data") <- NULL
                            callArg(playState, "subset") <- NULL
                            ## reorder factor levels
                            if (isUnordered(xvar, xVal)) {
                                xvar <- call("reorderByFreq", xvar)
                                xVal <- eval(xvar, dat)
                            }
                            xterms <- paste(c(deparseOneLine(xvar),
                                              if (!is.null(c1)) deparseOneLine(c1),
                                              if (!is.null(c2)) deparseOneLine(c2),
                                              if (!is.null(groups)) deparseOneLine(groups)),
                                            collapse=" + ")
                            ## and set logical `groups` argument
                            callArg(playState, "groups") <- !is.null(groups)
                            xform <- as.formula(paste("~", xterms))
                            callArg(playState, 1) <-
                                call("xtabs", xform, quote(dat), subset=subset)
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
                        if (nPoints > HEAPS) {
                            callArg(playState, "plot.points") <- FALSE
                        } else if (nPoints > LOTS) {
                            callArg(playState, "plot.points") <- "jitter"
                            callArg(playState, "pch") <- "+" ## like jittered rug
                        }
                    } else {
                        ## data on y axis, use qqmath
                        callArg(playState, 0) <- quote(qqmath)
                        if (!is.null(cond))
                            callArg(playState, 1) <- call("~", call("|", yvar, cond))
                        else
                            callArg(playState, 1) <- call("~", yvar)
                        ## settings depend on number of points, groups
                        if (nPoints > HEAPS) {
                            callArg(playState, "f.value") <- 100
                            callArg(playState, "type") <- "l"
                        }
                        if (!is.null(groups)) {
                            callArg(playState, "type") <- "o"
                            callArg(playState, "cex") <- 0.5
                        }
                        ## decide when to use normal vs uniform etc
                                        #madsad <- function(x, na.rm=TRUE)
                                        #    mad(x, na.rm=na.rm) / sd(x, na.rm=na.rm)
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
                            callArg(playState, "xlab") <- "Probability"
                        } else {
                            callArg(playState, "xlab") <- "Standard normal quantiles"
                        }
                    }
                }

            } else {
                ## BIVARIATE

                if (is.categorical(xVal) && is.categorical(yVal)) {
                    ## BIVARIATE CATEGORICAL
                    callArg(playState, 0) <- quote(barchart)
                    callArg(playState, "data") <- NULL
                    callArg(playState, "subset") <- NULL
                    ## reorder factor levels
                    if (isUnordered(xvar, xVal)) {
                        xvar <- call("reorderByFreq", xvar)
                        xVal <- eval(xvar, dat)
                    }
                    if (isUnordered(yvar, yVal)) {
                        yvar <- call("reorder", yvar,
                                     call("unclass", xvar), na.rm=T)
                        yVal <- eval(yvar, dat)
                    }
                    ## use xvar as groups (for stacking)
                    xterms <- paste(c(deparseOneLine(yvar),
                                      if (!is.null(c1)) deparseOneLine(c1),
                                      if (!is.null(c2)) deparseOneLine(c2),
                                      deparseOneLine(xvar)),
                                    collapse=" + ")
                    xform <- as.formula(paste("~", xterms))
                    callArg(playState, 1) <-
                        call("prop.table",
                             call("xtabs", xform, quote(dat), subset=subset),
                             margin=1)
                    ## ignore groups setting
                    callArg(playState, "groups") <- TRUE
                } else

                if (is.categorical(yVal) || is.categorical(xVal)) {
                    ## BIVARIATE CATEGORICAL AND NUMERIC

                    ## TODO: if only one value for each level use dotplot

                    ## reorder factor levels
                    if (is.categorical(yVal) && isUnordered(yvar, yVal)) {
                        yvar <- call("reorder", yvar, xvar, na.rm=T)
                        yVal <- eval(yvar, dat)
                    }
                    if (is.categorical(xVal) && isUnordered(xvar, xVal)) {
                        xvar <- call("reorder", xvar, yvar, na.rm=T)
                        xVal <- eval(xvar, dat)
                    }
                    ## formula
                    if (!is.null(cond))
                        callArg(playState, 1) <- call("~", yvar, call("|", xvar, cond))
                    else
                        callArg(playState, 1) <- call("~", yvar, xvar)

                    if (!is.null(groups)) {
                        callArg(playState, 0) <- quote(stripplot)
                        callArg(playState, "jitter.data") <- TRUE
                        callArg(playState, "type") <- c("p", "a")
                        callArg(playState, "fun") <- quote(median)

                    } else {
                        callArg(playState, 0) <- quote(bwplot)
                        ## check that quantiles are sensible, i.e. different?
                        ## but what if they are not -- panel.violin not great
                    }

                } else {
                    ## BIVARIATE NUMERIC
                    callArg(playState, 0) <- quote(xyplot)
                    if (!is.null(cond))
                        callArg(playState, 1) <- call("~", yvar, call("|", xvar, cond))
                    else
                        callArg(playState, 1) <- call("~", yvar, xvar)
                    type <- "p"
                    if (!is.unsorted(xVal, na.rm=TRUE))
                        type <- c("p", "l")
                    else {              #if (!is.null(groups))
                        type <- c("p", "smooth")
                        callArg(playState, "span") <- 1
                    }
                    callArg(playState, "type") <- type
                }
            }

            ## generic stuff...
            if (!is.null(xVal) || !is.null(yVal)) {

                ## aspect and scales
                callArg(playState, "aspect") <- aspect ## may be NULL
                if (!is.null(x.relation) || !is.null(y.relation)) {
                    ## either of these may be NULL
                    callArg(playState, quote(scales$x$relation)) <- x.relation
                    callArg(playState, quote(scales$y$relation)) <- y.relation
                }

                anyNumerics <- ((!is.null(xvar) && !is.categorical(xVal)) ||
                                (!is.null(yvar) && !is.categorical(yVal)))
                ## style settings for points
                if (anyNumerics) {
                    if (nPoints > LOTS) {
                        alpha <- if (nPoints > HEAPS) 0.05 else 0.25
                        if (!is.null(groups))
                            callArg(playState, quote(par.settings$superpose.symbol$alpha)) <- alpha
                        else {
                            callArg(playState, quote(par.settings$plot.symbol$alpha)) <- alpha
                        }
                    }
                    if (nPoints > HEAPS) {
                        ## TODO: if qqmath leave pch alone
                        callArg(playState, "pch") <- "." ## 0 ## empty square
                        callArg(playState, "cex") <- 2
                    }
                }
                ## add a grid if there are multiple panels
                if (anyNumerics && !is.null(cond)) {
                    type <- callArg(playState, "type")
                    if (is.null(type)) type <- "p" ## assumed!
                    type <- c(type, "g")
                    callArg(playState, "type") <- type
                }

                ## set up key
                if (!is.null(groups) ||
                    (is.categorical(xVal) && is.categorical(yVal)))
                {
                    auto.key <- list()
                    auto.key$title <- groupsOrigStr
                    levs <- levelsOK(groupsVal)
                    if (is.categorical(xVal) && is.categorical(yVal)) {
                        auto.key$title <- xvarOrigStr
                        levs <- levelsOK(xVal)
                    }
                    n.items <- length(levs)
                    auto.key$columns <-
                        if (n.items <= 1) NULL else
                    if (n.items <= 3) n.items else
                    if (n.items <= 4) 2 else
                    if (n.items <= 6) 3 else 1
                    if (n.items > 3) {
                        auto.key$cex <- 0.7
                        auto.key$cex.title <- 1
                    }
                    ## TODO: check lengths of text, abbreviate?
                    if (n.items > 6) {
                        auto.key$space <- "right"
                    }
                    callArg(playState, "auto.key") <- auto.key
                }

                ## fix up long x axis labels
                if (is.categorical(xVal) && !is.categorical(yVal) &&
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
                            scales$x$rot <- 90
                        }
                        callArg(playState, "scales") <- scales
                    }
                }

            }

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
            callArg(playState, "sub") <- list(subt, x=0.99, just="right")
            callArg(playState, quote(par.settings$par.sub.text)) <-
                list(cex=0.7, font=1)

            ## check whether anything changed
            if (identical(mainCall(playState), oldCall))
                return()
            playNewPlot(playState)
        }
        doRecompose <- function(widget, playState)
            composePlot(playState)
        doRecomposeOnSelect <- function(widget, playState) {
            if (widget[["active"]] > -1)
                composePlot(playState)
        }
        doRecomposeNewXY <- function(widget, playState)
            composePlot(playState, newXY=TRUE)
        doRecomposeNewXYOnSelect <- function(widget, playState) {
            if (widget[["active"]] > -1)
                composePlot(playState, newXY=TRUE)
        }
        doLabels <- function(widget, playState) {
            labels <- tryParse(labelsW$getActiveText())
            playState$labels <- eval(labels, dat)
            ## TODO: replot if showing labels?
        }
        doLabelsOnSelect <- function(widget, playState) {
            if (widget[["active"]] > -1)
                doLabels(widget, playState)
        }

        handler.flip <- function(widget, event, playState) {
            if (!isTRUE(playState$plot.ready)) {alarm(); return(FALSE)}
            playState$plot.ready <- FALSE
            xvarActive <- xvarW[["active"]]
            yvarActive <- yvarW[["active"]]
            xdisc <- xdiscW[["active"]]
            ydisc <- ydiscW[["active"]]
            xsens <- xonW[["sensitive"]]
            ysens <- yonW[["sensitive"]]
            xvarW[["active"]] <- yvarActive
            yvarW[["active"]] <- xvarActive
            xdiscW[["active"]] <- ydisc
            ydiscW[["active"]] <- xdisc
            xonW[["sensitive"]] <- ysens
            yonW[["sensitive"]] <- xsens
            playState$plot.ready <- TRUE
            composePlot(playState)
            return(FALSE)
        }
        handler.superpose <- function(widget, event, playState) {
            if (!isTRUE(playState$plot.ready)) {alarm(); return(FALSE)}
            playState$plot.ready <- FALSE
            c1Active <- c1W[["active"]]
            c2Active <- c2W[["active"]]
            if (c1Active <= 1) return()
            groupsW[["active"]] <- c1Active
            ## TODO: combine two conditioning variables?
            c1W[["active"]] <- 0
            c2W[["active"]] <- 0
            widget[["visible"]] <- FALSE
            playState$plot.ready <- TRUE
            composePlot(playState)
            return(FALSE)
        }
        handler.explode <- function(widget, event, playState) {
            if (!isTRUE(playState$plot.ready)) {alarm(); return(FALSE)}
            playState$plot.ready <- FALSE
            grActive <- groupsW[["active"]]
            if (grActive <= 1) return()
            c1Active <- c1W[["active"]]
            c2Active <- c2W[["active"]]
            if (c1Active <= 1)
                c1W[["active"]] <- grActive
            else
                c2W[["active"]] <- grActive
            groupsW[["active"]] <- 0
            widget[["visible"]] <- FALSE
            playState$plot.ready <- TRUE
            composePlot(playState)
            return(FALSE)
        }
        handler.subsetSelect <- function(widget, event, playState) {
            if (!isTRUE(playState$plot.ready)) {alarm(); return(FALSE)}
            selectScales <- c(if (!is.null(xvar)) "x",
                              if (!is.null(yvar)) "y")
            foo <- playRectInput(playState, scales=selectScales,
                                 prompt="Click and drag to define a data subset")
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
            ## TODO: if is.categorical(xVar) && is.categorical(yVar)
            subset <- call("&", xsub, ysub)
            if (is.null(yvar)) subset <- xsub
            if (is.null(xvar)) subset <- ysub
            subsetW$getChild()$setText(deparseOneLine(subset))
            composePlot(playState)
            return(FALSE)
        }

        niceButton <- function(label) {
            butt <- gtkEventBox()
            tmp <- gtkLabel(label)
            tmp$setMarkup(paste('<span foreground="blue"><u>',
                                label, '</u></span>', sep=""))
            butt$add(tmp)
                                        #butt <- gtkButtonNew()
                                        #butt[["relief"]] <- GtkReliefStyle["none"]
                                        #butt[["can-default"]] <- FALSE
                                        #butt[["height-request"]] <- 12
                                        #butt$modifyStyle(GtkRcStyle ## TODO reduce padding
            butt
        }

        ## set up widgets
        box <- gtkHBox()

        ## X Y VARS
        varsBox <- gtkVBox()
        xyBox <- gtkHBox()
        xyBox$packStart(gtkLabel(" Variables / expressions on axes:"),
                        expand=FALSE)
        xyflipW <- niceButton("switch")
        xyflipW[["visible"]] <- !is.null(xvar) || !is.null(yvar)
        gSignalConnect(xyflipW, "button-press-event",
                       handler.flip, data=playState)
        xyBox$packStart(xyflipW)
        varsBox$packStart(xyBox, expand=FALSE)
        ## Y VAR
        yvarBox <- gtkHBox()
        yonW <- gtkCheckButton("y= ")
        yonW[["active"]] <- TRUE
        yonW[["sensitive"]] <- !is.null(yvar)
        gSignalConnect(yonW, "clicked",
                       doRecomposeNewXY, data=playState)
        yvarBox$packStart(yonW, expand=FALSE)
        yvarW <- gtkComboBoxEntryNewText()
        yvarW$show()
        yvarW[["width-request"]] <- 100
        for (item in varnames) yvarW$appendText(item)
        index <- match(deparseOneLine(yvar), varnames)
        if (is.na(index)) index <- 1
        yvarW[["active"]] <- (index - 1)
        ## "changed" emitted on typing and selection
        gSignalConnect(yvarW, "changed",
                       doRecomposeNewXYOnSelect, data=playState)
        gSignalConnect(yvarW$getChild(), "activate",
                       doRecomposeNewXY, data=playState)
        yvarBox$packStart(yvarW)
        ydiscW <- gtkCheckButton("discretize")
        ydiscW[["active"]] <- ydisc
        gSignalConnect(ydiscW, "clicked",
                       doRecomposeNewXY, data=playState)
        yvarBox$packStart(ydiscW, expand=FALSE)
        varsBox$packStart(yvarBox, expand=FALSE)

        ## X VAR
        xvarBox <- gtkHBox()
        xonW <- gtkCheckButton("x= ")
        xonW[["active"]] <- TRUE
        xonW[["sensitive"]] <- !is.null(xvar)
        gSignalConnect(xonW, "clicked",
                       doRecomposeNewXY, data=playState)
        xvarBox$packStart(xonW, expand=FALSE)
        xvarW <- gtkComboBoxEntryNewText()
        xvarW$show()
        xvarW[["width-request"]] <- 100
        for (item in varnames) xvarW$appendText(item)
        index <- match(deparseOneLine(xvar), varnames)
        if (is.na(index)) index <- 1
        xvarW[["active"]] <- (index - 1)
        ## "changed" emitted on typing and selection
        gSignalConnect(xvarW, "changed",
                       doRecomposeNewXYOnSelect, data=playState)
        gSignalConnect(xvarW$getChild(), "activate",
                       doRecomposeNewXY, data=playState)
        xvarBox$packStart(xvarW)
        xdiscW <- gtkCheckButton("discretize")
        xdiscW[["active"]] <- xdisc
        gSignalConnect(xdiscW, "clicked",
                       doRecomposeNewXY, data=playState)
        xvarBox$packStart(xdiscW, expand=FALSE)
        varsBox$packStart(xvarBox, expand=FALSE)

        ## XY OPTS
        xyOptsBox <- gtkHBox()
        ## ASPECT
        xyOptsBox$packStart(gtkLabel(" Aspect:"), expand=FALSE)
        aspectW <- gtkComboBoxEntryNewText()
        aspectW$show()
        aspectW[["width-request"]] <- 50
        for (item in aspectopts) aspectW$appendText(item)
        if (!is.null(aspectVal)) {
            index <- match(aspectVal, aspectopts)
            if (is.na(index)) index <- 0
            aspectW[["active"]] <- (index - 1)
        }
        ## "changed" emitted on typing and selection
        gSignalConnect(aspectW, "changed",
                       doRecomposeOnSelect, data=playState)
        gSignalConnect(aspectW$getChild(), "activate",
                       doRecompose, data=playState)
        xyOptsBox$packStart(aspectW)
        xyOptsBox$packStart(gtkLabel(""), padding=1)
        ## LEVELS
        xyOptsBox$packStart(gtkLabel("Levels:"), expand=FALSE)
        nLevelsW <- gtkSpinButton(min=1, max=16, step=1)
        nLevelsW[["width-request"]] <- 40
        nLevelsW[["digits"]] <- 0
        nLevelsW$setValue(nLevels)
        gSignalConnect(nLevelsW, "value-changed",
                       doRecompose, data=playState)
        xyOptsBox$packStart(nLevelsW, expand=FALSE)
        varsBox$packStart(xyOptsBox, expand=FALSE, padding=1)
        box$packStart(varsBox, padding=1)

        box$packStart(gtkVSeparator(), expand=FALSE, padding=1)

        ## CONDITIONING VARS
        cvarsBox <- gtkVBox()
        cvarsBox[["sensitive"]] <- !is.null(xvar) || !is.null(yvar)
        cBox <- gtkHBox()
        cBox$packStart(gtkLabel(" Conditioning:"), expand=FALSE)
        superposeW <- niceButton("superpose")
        superposeW[["visible"]] <- !is.null(c1)
        gSignalConnect(superposeW, "button-press-event",
                       handler.superpose, data=playState)
        cBox$packStart(superposeW)
        cvarsBox$packStart(cBox, expand=FALSE, padding=1)
        ## first conditioning variable
        c1Box <- gtkHBox()
        c1W <- gtkComboBoxEntryNewText()
        c1W$show()
        c1W[["width-request"]] <- 100
        for (item in varnames) c1W$appendText(item)
        index <- match(deparseOneLine(c1), varnames)
        if (is.na(index)) index <- 1
        c1W[["active"]] <- (index - 1)
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
        c2W[["sensitive"]] <- !is.null(c1)
        c2W[["width-request"]] <- 100
        for (item in varnames) c2W$appendText(item)
        index <- match(deparseOneLine(c2), varnames)
        if (is.na(index)) index <- 1
        c2W[["active"]] <- (index - 1)
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
        scalesW[["sensitive"]] <- !is.null(c1)
        scalesW[["width-request"]] <- 80
        for (item in scalesopts) scalesW$appendText(item)
        if (!is.null(scalesVal)) {
            index <- match(scalesVal, scalesopts)
            if (is.na(index)) index <- 0
            scalesW[["active"]] <- (index - 1)
        }
        ## "changed" emitted on typing and selection
        gSignalConnect(scalesW, "changed",
                       doRecomposeOnSelect, data=playState)
        scalesBox$packStart(scalesW)
        cvarsBox$packStart(scalesBox, expand=FALSE, padding=1)
        box$packStart(cvarsBox, padding=1)

        box$packStart(gtkVSeparator(), expand=FALSE, padding=1)

        ## GROUPS
        gvarsBox <- gtkVBox()
        gvarsBox[["sensitive"]] <- !is.null(xvar) || !is.null(yvar)
        ## GROUPS
        grBox <- gtkHBox()
        grBox$packStart(gtkLabel(" Groups:"), expand=FALSE)
        explodeW <- niceButton("explode")
        explodeW[["visible"]] <- !is.null(groups)
        gSignalConnect(explodeW, "button-press-event",
                       handler.explode, data=playState)
        grBox$packStart(explodeW)
        gvarsBox$packStart(grBox, expand=FALSE, padding=1)
        ## groups
        gBox <- gtkHBox()
        groupsW <- gtkComboBoxEntryNewText()
        groupsW$show()
        groupsW[["width-request"]] <- 100
        for (item in varnames) groupsW$appendText(item)
        index <- match(deparseOneLine(groups), varnames)
        if (is.na(index)) index <- 1
        groupsW[["active"]] <- (index - 1)
        ## "changed" emitted on typing and selection
        gSignalConnect(groupsW, "changed",
                       doRecomposeOnSelect, data=playState)
        gSignalConnect(groupsW$getChild(), "activate",
                       doRecompose, data=playState)
        gBox$packStart(groupsW)
        gvarsBox$packStart(gBox, expand=FALSE)

        ## LABELS
        laBox <- gtkHBox()
        laBox$packStart(gtkLabel(" Labels:"), expand=FALSE)
        gvarsBox$packStart(laBox, expand=FALSE, padding=1)
        labelsW <- gtkComboBoxEntryNewText()
        labelsW$show()
        labelsW[["width-request"]] <- 100
        for (item in labelsopts) labelsW$appendText(item)
        ## TODO: detect current labels setting (from playState)
        labelsW[["active"]] <- 0
        ## "changed" emitted on typing and selection
        gSignalConnect(labelsW, "changed",
                       doLabelsOnSelect, data=playState)
        gSignalConnect(labelsW$getChild(), "activate",
                       doLabels, data=playState)
        gvarsBox$packStart(labelsW, expand=FALSE)
        box$packStart(gvarsBox, expand=FALSE, padding=1)

        box$packStart(gtkVSeparator(), expand=FALSE, padding=1)

        ## SETS
        setBox <- gtkVBox()
        ## SUBSET
        subsetBox <- gtkHBox()
        subsetBox$packStart(gtkLabel(" Subset:"), expand=FALSE)
        subsetSelW <- niceButton("interactive...")
        subsetSelW[["visible"]] <- !is.null(xvar) || !is.null(yvar)
        gSignalConnect(subsetSelW, "button-press-event",
                       handler.subsetSelect, data=playState)
        subsetBox$packStart(subsetSelW)
        setBox$packStart(subsetBox, expand=FALSE, padding=1)
        subsetW <- gtkComboBoxEntryNewText()
        subsetW$show()
        subsetW[["width-request"]] <- -1
        for (item in subsetopts) subsetW$appendText(item)
        index <- match(deparseOneLine(subset), subsetopts)
        if (is.na(index)) index <- 1 ## should never happen
        subsetW[["active"]] <- (index - 1)
        ## "changed" emitted on typing and selection
        gSignalConnect(subsetW, "changed",
                       doRecomposeOnSelect, data=playState)
        gSignalConnect(subsetW$getChild(), "activate",
                       doRecompose, data=playState)
        setBox$packStart(subsetW, expand=FALSE)
        ## HOTSET
        ## TODO
        hotsetBox <- gtkHBox()
        hotsetBox$packStart(gtkLabel(" Hot-set:"), expand=FALSE)
        hotsetSelW <- niceButton("interactive...")
        hotsetSelW[["visible"]] <- FALSE #!is.null(xvar) || !is.null(yvar)
        hotsetBox$packStart(hotsetSelW)
        setBox$packStart(hotsetBox, expand=FALSE, padding=1)
        hotsetW <- gtkComboBoxEntryNewText()
        hotsetW$show()
        hotsetW[["sensitive"]] <- FALSE
        hotsetW[["width-request"]] <- -1
        setBox$packStart(hotsetW, expand=FALSE)
        box$packStart(setBox, expand=FALSE, padding=1)

        ## add it directly to the window (not a toolbar!)
        if (!is.null(playState$widgets$latticist))
            playState$widgets$latticist$destroy()
        playState$widgets$latticist <- box
        playState$widgets$vbox$packStart(box, expand=FALSE)

        return(NA)
    }
}

is.categorical <- function(x)
    is.factor(x) || is.shingle(x) || is.character(x) || is.logical(x)

levelsOK <- function(x) {
    if (is.logical(x)) return(c(TRUE, FALSE))
    levels(x)
}

reorderByFreq <- function(x) {
    reorder(x, x, function(z) -length(z))
}
