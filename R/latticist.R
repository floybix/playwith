## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2008 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

NULLNAMES <- c("(none)", "", "NULL")

latticist <-
    function(dat,
             spec = alist(),
             reorder.levels = TRUE,
             ...,
             labels = rownames(dat),
             time.mode = FALSE,
             height = playwith.getOption("height") - 1,
             plot.call)
{
    generateCall <- FALSE
    if (!missing(dat)) {
        ## dat argument was given (normal usage)
        datArg <- substitute(dat)
        if (missing(plot.call))
            generateCall <- TRUE

    } else {
        ## dat argument missing
        if (missing(plot.call))
            stop("Give one of 'dat' or 'plot.call'.")
        ## plot.call was given; try to extract data
        dat <- NULL
        datArg <- quote(unknown)
        ## assuming the relevant lattice function is the outer call
        ## check for named 'data' argument
        if (!is.null(plot.call$data)) {
            datArg <- plot.call$data
        } else {
            if (length(plot.call) <= 1) {
                ## no arguments or NULL call
                stop("Can not extract data from plot.call.")
            } else {
                ## one or more arguments
                datArg <- plot.call[[2]]
                dat <- eval.parent(datArg)
                if (inherits(dat, "formula")) {
                    ## try second argument if exists and un-named
                    if ((length(plot.call) > 2) &&
                        (is.null(names(plot.call)) ||
                         identical(names(plot.call)[3], "")))
                    {
                        datArg <- plot.call[[3]]
                    } else {
                        stop("Can not extract data from plot.call.")
                    }
                }
            }
        }
        if (is.null(dat))
            dat <- eval.parent(datArg)
    }
    title <- paste("Latticist:",
                   toString(deparse(datArg), width=30))

    if (is.table(dat)) reorder.levels <- FALSE
    isOK <- is.data.frame(dat) || is.table(dat)
    makeLocalCopy <- (isTRUE(reorder.levels) || !isOK)

    if (makeLocalCopy) {

        if (!is.data.frame(dat))
            dat <- as.data.frame(dat)

        ## convert numerics with discrete values in {-1, 0, 1} to factors
        isnum <- sapply(dat, is.numeric)
        for (nm in names(dat)[isnum]) {
            dd <- dat[[nm]]
            ## test first 50 values first (quick check)
            vals <- unique(head(dd, n=50))
            if (all(vals[is.finite(vals)] %in% -1:1)) {
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

        ## make a local copy of dat
        if (is.symbol(datArg)) {
            datNm <- paste(as.character(datArg),
                           ".mod", sep = "")
            datArg <- as.symbol(datNm)
            assign(datNm, dat)
        } else {
            datNm <- "dat"
            datArg <- as.symbol(datNm)
        }
    }

    if (generateCall)
        plot.call <-
            latticistCompose(dat, spec, datArg = datArg)

    ## lattInit is the constructor (an init.action)
    lattAction <- latticistToolConstructor(dat, datArg = datArg)
    ## this list will store state in playState$latticist
    lattList <- latticistInitOptions(dat, datArg = datArg)
    lattList$spec <- spec
                                        #if (!is.list(eval.args))
                                        #    eval.args <- as.list(eval.args)
                                        #eval.args$envir <- parent.frame()

    playwith(plot.call = plot.call,
             title = title, ...,
             height = height,
             labels = labels,
             time.mode = time.mode,
             init.actions = list(latticist = lattAction),
             latticist = lattList)
}

latticistInitOptions <- function(dat, datArg)
{
    stuff <- list()
    datNm <- toString(deparse(datArg))

    ## options
    stuff$linesSetting <- TRUE

    if (is.table(dat)) {
        ## dat is a table
        stuff$varexprs <-
            c(NULLNAMES[[1]],
              names(dimnames(dat)),
              "-------------------",
              sprintf("complete.cases(%s)",
                      datNm))

        ## subsets -- preload some useful subsets
        subsetopts <- NULLNAMES[[1]]
        ## preload factor levels (only most frequent two of each)
        dimn <- dimnames(dat)
        toplev <- lapply(names(dimn), function(nm) {
            paste(nm, "==", head(dimn[[nm]], 2))
        })
        subsetopts <- c(subsetopts, unlist(toplev))
        subsetopts <- c(subsetopts, "------------------")
        stuff$subsetopts <- subsetopts

    } else {
        ## dat is a data.frame

        ## which variables are categorical (vs numeric)
        iscat <- sapply(dat, is.categorical)

        ## variables and expressions
        ## group into categorical vs numeric
        stuff$varexprs <-
            c(NULLNAMES[[1]],
              names(dat)[iscat],
              if (any(iscat) && any(!iscat))
              "------------------",
              names(dat)[!iscat],
              "-------------------",
              sprintf("1:nrow(%s)", datNm))

        ## subsets -- preload some useful subsets
        subsetopts <- NULLNAMES[[1]]
        ## preload factor levels (only first two of each)
        toplev <- lapply(names(dat)[iscat], function(nm) {
            if (is.factor(dat[[nm]])) {
                paste(nm, "==", head(levels(dat[[nm]]), 2))
            } else if (is.logical(dat[[nm]])) {
                paste(nm, "==", c("TRUE", "FALSE"))
            } else {
                tmp <- names(sort(table(dat[[nm]]), decreasing=TRUE))
                tmp <- tmp[seq_len(min(2, length(tmp)))] ## top 2
                paste(nm, "==", sapply(tmp, deparse))
            }
        })
        subsetopts <- c(subsetopts, unlist(toplev))
        subsetopts <- c(subsetopts, "------------------")
        if (nrow(dat) >= LOTS) {
            ## a regular sample down by one order of magnitude
            subN <- 10 ^ (round(log10(nrow(dat))) - 1)
            subsetopts <- c(subsetopts,
                            sprintf("seq(1, nrow(%s), length = %i)",
                                    datNm, subN))
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
            subsetopts <- c(subsetopts,
                            sprintf("complete.cases(%s)", datNm))
        }
        subsetopts <- c(subsetopts, missings)
        stuff$subsetopts <- subsetopts
    }

    ## aspect
    stuff$aspectopts <-
        c('"fill"', '"iso"', '"xy"',
          '0.5', '1', '2')
    ## scales
    stuff$scalesopts <-
        c("x same, y same",
          "x same, y free",
          "x free, y same",
          "x free, y free",
          "------------------",
          "x sliced, y sliced",
          "x sliced, y same",
          "x sliced, y free",
          "x same, y sliced",
          "x free, y sliced")

    stuff
}

latticistToolConstructor <- function(dat, datArg)
{
    ## this is the init.action
    function(playState)
    {
        ## check that it is a sensible call
        if (!isTRUE(playState$accepts.arguments)) return()

        ## this function is triggered by user changes to widgets
        reCompose <- function(playState, newPlot = FALSE)
        {
            spec <- playState$latticist$spec
            if (newPlot) {
                ## ignore old aspect and scales for new plot types
                spec$aspect <- NULL
                spec$x.relation <- NULL
                spec$y.relation <- NULL
            }
            if (isTRUE(spec$doXDisc) && isTRUE(spec$doYDisc) &&
                require("hexbin")) {
                ## if both are discretized numerics, use 2D binning
                spec$doXDisc <- FALSE
                spec$doYDisc <- FALSE
                spec$doHexbin <- TRUE
            }
            ## compose plot call from latticist specification
            oldCall <- playState$call
            playState$call <-
                latticistCompose(dat, spec, datArg = datArg)
            ## check whether anything changed
            updateMainCall(playState) ## set canonical arguments
            if (identical(deparse(playState$call, control=NULL),
                          deparse(oldCall, control=NULL)))
                return()
            ## need playNewPlot (not playReplot) to reload latticist
            playNewPlot(playState)
        }

        tryParse <- function(x) {
            if (x %in% NULLNAMES) return(NULL)
            tryCatch(parse(text = x)[[1]],
                     error = error_handler)
        }

        doChooseVars <- function(...) {
            ## applies to hypervariate plots:
            ## splom, parallel and marginal.plot
            if (is.table(dat)) return(TRUE)
            vars <- names(dat)
            checked <- playState$latticist$spec$varSubset
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
                             playState$latticist$spec$varSubset <- varsub
                             dispose(h$obj)
                         })
        }

        spec <- playState$latticist$spec
        xvar <- spec$xvar
        yvar <- spec$yvar
        zvar <- spec$zvar
        groups <- spec$groups
        cond <- spec$cond
        cond2 <- spec$cond2
        subset <- spec$subset
        aspect <- spec$aspect
        aspect3D <- spec$aspect3D
        doXDisc <- isTRUE(spec$doXDisc)
        doYDisc <- isTRUE(spec$doYDisc)

        nLevels <- INIT.NLEVELS
        if (!is.null(spec$nLevels))
            nLevels <- spec$nLevels

        ## set up variables and options
        xvarStr <- deparseOneLine(xvar)
        yvarStr <- deparseOneLine(yvar)
        zvarStr <- deparseOneLine(zvar)
        groupsStr <- deparseOneLine(groups)
        condStr <- deparseOneLine(cond)
        cond2Str <- deparseOneLine(cond2)

        ## update stored lists of variable expressions
        varexprs <- playState$latticist$varexprs
        varexprs <- unique(c(varexprs,
                             xvarStr, yvarStr, zvarStr,
                             condStr, cond2Str, groupsStr))
        ## replace all synonyms of "NULL" with just one
        varexprs <- varexprs[!(varexprs %in% NULLNAMES)]
        varexprs <- c(NULLNAMES[[1]], varexprs)
        playState$latticist$varexprs <- varexprs

        ## update subset options
        subsetopts <- playState$latticist$subsetopts
        subsetopts <-
            unique(c(subsetopts,
                     if (!is.null(subset) && !isTRUE(subset))
                     deparseOneLine(subset)))
        playState$latticist$subsetopts <- subsetopts

        ## aspect setting
        aspectopts <- playState$latticist$aspectopts
        aspectVal <- spec$aspect
        if (!is.null(aspectVal)) {
            aspectVal <- deparseOneLine(aspectVal)
            aspectopts <-
                unique(c(aspectopts, aspectVal))
        }

        ## scales setting
        scalesopts <- playState$latticist$scalesopts
        scalesVal <- NULL
        if (!is.null(spec$x.relation) &&
            !is.null(spec$y.relation))
        {
            scalesVal <- paste("x ", spec$x.relation, ", ",
                               "y ", spec$y.relation, sep="")
        }

        ## evaluate variables to determine data types
        xIsCat <- yIsCat <- groupsIsCat <- TRUE
        if (!is.table(dat)) {
            xIsCat <- is.categorical(eval(xvar, dat))
            yIsCat <- is.categorical(eval(yvar, dat))
            groupsIsCat <- is.categorical(eval(groups, dat))
        }


        ## CREATE THE GUI

        ## set up widgets
        box <- gtkHBox()

        niceButton <- function(label) {
            butt <- gtkEventBox()
            tmp <- gtkLabel(label)
            tmp$setMarkup(paste('<span foreground="blue"><u>',
                                label, '</u></span>', sep=""))
            butt$add(tmp)
            butt
        }

        ## action handlers for gtkComboBoxEntry
        addCBEHandlers <- function(widget, handler, data = NULL)
        {
            gSignalConnect(widget, "changed",
                           function(widget, data) {
                               if (widget["active"] > -1)
                                   handler(widget, data)
                           }, data = data)
            gSignalConnect(widget$getChild(), "activate",
                           handler, data = data)
        }

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
                          "Select variables --&gt;",
                          '</span>', sep = "")
        promptW <- gtkLabel("")
        promptW$setMarkup(promptxt)
        promptW["visible"] <- (is.null(xvar) && is.null(yvar))
        ## SUBSET
        subsetBox <- gtkHBox()
        subsetBox$packStart(gtkLabel(" Subset: "), expand = FALSE)
        subsetBox$packEnd(promptW, expand = FALSE)
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
        addCBEHandlers(subsetW,
                       function(...) {
                           playState$latticist$spec$subset <-
                               tryParse(subsetW$getActiveText())
                           reCompose(playState)
                       })
        setBox$packStart(subsetW, expand = FALSE)
        ## HYPERVAR
        hyperBox0 <- gtkHBox()
        hyperBox0$packStart(gtkLabel(" Hyper-variate plots: "),
                            expand = FALSE)
        ## "choose variables" button
        choosevarsW <- niceButton("choose variables")
        choosevarsW["visible"] <- (is.null(xvar) && is.null(yvar))
        gSignalConnect(choosevarsW, "button-press-event",
                       function(...) {
                           if (isTRUE(doChooseVars()))
                               reCompose(playState)
                       })
        hyperBox0$packStart(choosevarsW)
        setBox$packStart(hyperBox0, expand=FALSE, padding = 1)
        ## hypervar reset buttons
        hyperBox <- gtkHBox()
        marginalsW <- gtkButton("marginals")
        marginalsW["tooltip-text"] <-
            "Show marginal distributions"
        splomW <- gtkButton("splom")
        splomW["tooltip-text"] <-
            "Show a scatterplot matrix (all pairs)"
        parallelW <- gtkButton("parallel")
        parallelW["tooltip-text"] <-
            "Show a parallel coordinates plot"
        gSignalConnect(marginalsW, "clicked",
                       function(...) {
                           playState$latticist$spec$defaultPlot <-
                               "marginal.plot"
                           if (isTRUE(doChooseVars())) {
                               ## reset x/y/z to force hypervariate
                               playState$latticist$spec$xvar <- NULL
                               playState$latticist$spec$yvar <- NULL
                               playState$latticist$spec$zvar <- NULL
                               reCompose(playState, newPlot = TRUE)
                           }
                       })
        gSignalConnect(splomW, "clicked",
                       function(...) {
                           playState$latticist$spec$defaultPlot <-
                               "splom"
                           if (isTRUE(doChooseVars())) {
                               ## reset x/y/z to force hypervariate
                               playState$latticist$spec$xvar <- NULL
                               playState$latticist$spec$yvar <- NULL
                               playState$latticist$spec$zvar <- NULL
                               reCompose(playState, newPlot = TRUE)
                           }
                       })
        gSignalConnect(parallelW, "clicked",
                       function(...) {
                           playState$latticist$spec$defaultPlot <-
                               "parallel"
                           if (isTRUE(doChooseVars())) {
                               ## reset x/y/z to force hypervariate
                               playState$latticist$spec$xvar <- NULL
                               playState$latticist$spec$yvar <- NULL
                               playState$latticist$spec$zvar <- NULL
                               reCompose(playState, newPlot = TRUE)
                           }
                       })
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
             !xIsCat && !yIsCat)
        isBivarCat <-
            (!is.null(xvar) && !is.null(yvar) && is.null(zvar) &&
             xIsCat && yIsCat)
        labtxt <- " Variables / expressions on axes: "
        if (!is.null(xvar) || !is.null(yvar))
            labtxt <- " Variables on axes: "
        xyLabelW <- gtkLabel(labtxt)
        xyLabelW$setMarkup(paste("<b>", labtxt, "</b>", sep=""))
        xyBox$packStart(xyLabelW, expand=FALSE)
        ## "switch" button
        xyflipW <- niceButton("switch")
        xyflipW["visible"] <- !is.null(xvar) || !is.null(yvar)
        gSignalConnect(xyflipW, "button-press-event",
                       function(...) {
                           xvar <- playState$latticist$spec$xvar
                           yvar <- playState$latticist$spec$yvar
                           playState$latticist$spec$xvar <- yvar
                           playState$latticist$spec$yvar <- xvar
                           reCompose(playState, newPlot = TRUE)
                       })
        xyBox$packStart(xyflipW, padding = 2)
        ## "hexbin" button
        hexbinW <- niceButton("hexbin")
        hexbinW["visible"] <- (isBivarNumeric && !doXDisc && !doYDisc &&
                               require("hexbin", quietly = TRUE))
        gSignalConnect(hexbinW, "button-press-event",
                       function(...) {
                           playState$latticist$spec$doHexbin <- TRUE
                           reCompose(playState, newPlot = TRUE)
                       })
        xyBox$packStart(hexbinW, padding = 2)
        ## "points" button
        unbinW <- niceButton("points")
        unbinW["visible"] <- isBivarNumeric && isTRUE(spec$doHexbin)
        gSignalConnect(unbinW, "button-press-event",
                       function(...) {
                           playState$latticist$spec$doHexbin <- FALSE
                           reCompose(playState, newPlot = TRUE)
                       })
        xyBox$packStart(unbinW, padding = 2)
        varsBox$packStart(xyBox, expand=FALSE)
        ## Y VAR
        yvarBox <- gtkHBox()
        yonW <- gtkCheckButton("y= ")
        yonW["active"] <- TRUE
        yonW["sensitive"] <- !is.null(yvar)
        gSignalConnect(yonW, "clicked",
                       function(...) {
                           playState$latticist$spec$yvar <- NULL
                           reCompose(playState, newPlot = TRUE)
                       })
        yvarBox$packStart(yonW, expand=FALSE)
        yvarW <- gtkComboBoxEntryNewText()
        yvarW$show()
        yvarW["width-request"] <- 100
        for (item in varexprs) yvarW$appendText(item)
        index <- match(deparseOneLine(yvar), varexprs)
        if (is.na(index)) index <- 1
        yvarW["active"] <- (index - 1)
        ## "changed" emitted on typing and selection
        addCBEHandlers(yvarW,
                       function(...) {
                           playState$latticist$spec$yvar <-
                               tryParse(yvarW$getActiveText())
                           reCompose(playState, newPlot = TRUE)
                       })
        yvarBox$packStart(yvarW)
        ## discretize -- for numerics
        ydiscW <- gtkCheckButton("discretize")
        ydiscW["active"] <- doYDisc
        ydiscW["sensitive"] <- !is.null(yvar)
        ydiscW["visible"] <- !yIsCat
        gSignalConnect(ydiscW, "clicked",
                       function(widget, ...) {
                           playState$latticist$spec$doYDisc <-
                               widget["active"]
                           reCompose(playState, newPlot = TRUE)
                       })
        yvarBox$packStart(ydiscW, expand=FALSE)
        varsBox$packStart(yvarBox, expand=FALSE)

        ## X VAR
        xvarBox <- gtkHBox()
        xonW <- gtkCheckButton("x= ")
        xonW["active"] <- TRUE
        xonW["sensitive"] <- !is.null(xvar)
        gSignalConnect(xonW, "clicked",
                       function(...) {
                           playState$latticist$spec$xvar <- NULL
                           reCompose(playState, newPlot = TRUE)
                       })
        xvarBox$packStart(xonW, expand=FALSE)
        xvarW <- gtkComboBoxEntryNewText()
        xvarW$show()
        xvarW["width-request"] <- 100
        for (item in varexprs) xvarW$appendText(item)
        index <- match(deparseOneLine(xvar), varexprs)
        if (is.na(index)) index <- 1
        xvarW["active"] <- (index - 1)
        ## "changed" emitted on typing and selection
        addCBEHandlers(xvarW,
                       function(...) {
                           playState$latticist$spec$xvar <-
                               tryParse(xvarW$getActiveText())
                           reCompose(playState, newPlot = TRUE)
                       })
        xvarBox$packStart(xvarW)
        ## "discretize" -- for numerics
        xdiscW <- gtkCheckButton("discretize")
        xdiscW["active"] <- doXDisc
        xdiscW["sensitive"] <- !is.null(xvar)
        xdiscW["visible"] <- !xIsCat
        gSignalConnect(xdiscW, "clicked",
                       function(widget, ...) {
                           playState$latticist$spec$doXDisc <-
                               widget["active"]
                           reCompose(playState, newPlot = TRUE)
                       })
        xvarBox$packStart(xdiscW, expand=FALSE)
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
        addCBEHandlers(aspectW,
                       function(...) {
                           playState$latticist$spec$aspect <-
                               tryParse(aspectW$getActiveText())
                           reCompose(playState)
                       })
        xyOptsBox$packStart(aspectW)
        xyOptsBox$packStart(gtkLabel(""), padding=1)
        ## LINES
        linesW <- gtkCheckButton("Lines. ")
        linesW["active"] <- !identical(spec$doLines, FALSE)
        linesW["sensitive"] <- !is.null(xvar) || !is.null(yvar)
        gSignalConnect(linesW, "clicked",
                       function(widget, ...) {
                           playState$latticist$spec$doLines <-
                               widget["active"]
                           reCompose(playState)
                       })
        xyOptsBox$packStart(linesW, expand=FALSE)
        xyOptsBox$packStart(gtkLabel(""), padding=1)
        ## LEVELS
        xyOptsBox$packStart(gtkLabel("Levels:"), expand=FALSE)
        nLevelsW <- gtkSpinButton(min=1, max=16, step=1)
        nLevelsW["width-request"] <- 40
        nLevelsW["digits"] <- 0
        nLevelsW$setValue(nLevels)
        gSignalConnect(nLevelsW, "value-changed",
                       function(widget, ...) {
                           playState$latticist$spec$nLevels <-
                               widget["value"]
                           reCompose(playState)
                       })
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
        explodeW["visible"] <- !is.null(groups) && groupsIsCat
        gSignalConnect(explodeW, "button-press-event",
                       function(...) {
                           groups <- playState$latticist$spec$groups
                           if (is.null(playState$latticist$spec$cond)) {
                               playState$latticist$spec$cond <- groups
                           } else {
                               playState$latticist$spec$cond2 <- groups
                           }
                           playState$latticist$spec$groups <- NULL
                           reCompose(playState, newPlot = TRUE)
                       })
        groupsBox$packStart(explodeW)
        ## "go 3D" button
        go3DW <- niceButton("go 3D")
        go3DW["visible"] <- !is.null(groups) && !groupsIsCat
        gSignalConnect(go3DW, "button-press-event",
                       function(...) {
                           groups <- playState$latticist$spec$groups
                           playState$latticist$spec$zvar <- groups
                           playState$latticist$spec$groups <- NULL
                           reCompose(playState, newPlot = TRUE)
                       })
        groupsBox$packStart(go3DW)
        gzBox$packStart(groupsBox, expand=FALSE, padding=1)
        ## groups
        gBox <- gtkHBox()
        groupsW <- gtkComboBoxEntryNewText()
        groupsW$show()
        groupsW["width-request"] <- 100
        for (item in varexprs) groupsW$appendText(item)
        index <- match(deparseOneLine(groups), varexprs)
        if (is.na(index)) index <- 1
        groupsW["active"] <- (index - 1)
        ## "changed" emitted on typing and selection
        addCBEHandlers(groupsW,
                       function(...) {
                           playState$latticist$spec$groups <-
                               tryParse(groupsW$getActiveText())
                           reCompose(playState)
                       })
        gBox$packStart(groupsW)
        ## tile
        tileW <- gtkCheckButton("tile")
        tileW["active"] <- isTRUE(spec$doTile)
        tileW["visible"] <- !is.null(groups) && !groupsIsCat
        gSignalConnect(tileW, "clicked",
                       function(widget, ...) {
                           playState$latticist$spec$doTile <-
                               widget["active"]
                           reCompose(playState)
                       })
        gBox$packStart(tileW)
        gzBox$packStart(gBox, expand=FALSE)

        ## Z / SEGMENTS VARIABLE
        zBox <- gtkHBox()
        zBox$packStart(gtkLabel(" Depth (3D)"), expand=FALSE)
        ## segments option
        segmentsW <- gtkCheckButton("Segments (x--z) ")
        segmentsW["active"] <- isTRUE(spec$doSegments)
        segmentsW["visible"] <- !is.null(xvar) && !is.null(yvar)
        gSignalConnect(segmentsW, "clicked",
                       function(widget, ...) {
                           playState$latticist$spec$doSegments <-
                               widget["active"]
                           reCompose(playState, newPlot = TRUE)
                       })
        orLabelW <- gtkLabel(" or")
        orLabelW["visible"] <- segmentsW["visible"]
        zBox$packStart(orLabelW)
        zBox$packStart(segmentsW, expand=FALSE)
        ## "squash" button
        squashW <- niceButton("squash")
        squashW["visible"] <- !is.null(zvar)
        gSignalConnect(squashW, "button-press-event",
                       function(...) {
                           zvar <- playState$latticist$spec$zvar
                           playState$latticist$spec$groups <- zvar
                           playState$latticist$spec$zvar <- NULL
                           reCompose(playState, newPlot = TRUE)
                       })
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
        addCBEHandlers(zvarW,
                       function(...) {
                           playState$latticist$spec$zvar <-
                               tryParse(zvarW$getActiveText())
                           reCompose(playState, newPlot = TRUE)
                       })
        zvarBox$packStart(zvarW)
        ## asError option
        aserrorW <- gtkCheckButton("as error") # (x+/-z)")
        aserrorW["active"] <- isTRUE(spec$doAsError)
        aserrorW["visible"] <- isTRUE(spec$doSegments)
        gSignalConnect(aserrorW, "clicked",
                       function(widget, ...) {
                           playState$latticist$spec$doAsError <-
                               widget["active"]
                           reCompose(playState)
                       })
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
        superposeW["visible"] <- !is.null(cond)
        gSignalConnect(superposeW, "button-press-event",
                       function(...) {
                           conds <- playState$latticist$spec$cond
                           if (!is.null(playState$latticist$spec$cond2)) {
                               conds <- call("paste", conds,
                                            playState$latticist$spec$cond2)
                           }
                           playState$latticist$spec$groups <- conds
                           playState$latticist$spec$cond <- NULL
                           playState$latticist$spec$cond2 <- NULL
                           reCompose(playState, newPlot = TRUE)
                       })
        cBox$packStart(superposeW)
        cvarsBox$packStart(cBox, expand=FALSE, padding=1)
        ## first conditioning variable
        condBox <- gtkHBox()
        condW <- gtkComboBoxEntryNewText()
        condW$show()
        condW["sensitive"] <- (playState$callName != "marginal.plot")
        condW["width-request"] <- 100
        for (item in varexprs) condW$appendText(item)
        index <- match(deparseOneLine(cond), varexprs)
        if (is.na(index)) index <- 1
        condW["active"] <- (index - 1)
        ## "changed" emitted on typing and selection
        addCBEHandlers(condW,
                       function(...) {
                           playState$latticist$spec$cond <-
                               tryParse(condW$getActiveText())
                           reCompose(playState, newPlot = TRUE)
                       })
        condBox$packStart(condW)
        cvarsBox$packStart(condBox, expand=FALSE)
        ## second conditioning variable
        cond2Box <- gtkHBox()
        cond2W <- gtkComboBoxEntryNewText()
        cond2W$show()
        cond2W["sensitive"] <- !is.null(cond)
        cond2W["width-request"] <- 100
        for (item in varexprs) cond2W$appendText(item)
        index <- match(deparseOneLine(cond2), varexprs)
        if (is.na(index)) index <- 1
        cond2W["active"] <- (index - 1)
        ## "changed" emitted on typing and selection
        addCBEHandlers(cond2W,
                       function(...) {
                           playState$latticist$spec$cond2 <-
                               tryParse(cond2W$getActiveText())
                           reCompose(playState, newPlot = TRUE)
                       })
        cond2Box$packStart(cond2W)
        cvarsBox$packStart(cond2Box, expand=FALSE)
        ## SCALES
        scalesBox <- gtkHBox()
        if (playState$callName %in% c("mosaic", "cotabplot")) {
            ## vcd plot
            strataW <- gtkCheckButton("separate strata")
            strataW["active"] <- !identical(spec$doSeparateStrata, FALSE)
            gSignalConnect(strataW, "clicked",
                       function(widget, ...) {
                           playState$latticist$spec$doSeparateStrata <-
                               widget["active"]
                           reCompose(playState, newPlot = TRUE)
                       })
            scalesBox$packStart(strataW)

        } else {
            ## Lattice plot
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
                           function(...) {
                               scalesIdx <- scalesW$getActive() + 1
                               scalesopts <- playState$latticist$scalesopts
                               tmp <- strsplit(scalesopts[scalesIdx], ", ")[[1]]
                               x.relation <- substring(tmp[1], first=3)
                               y.relation <- substring(tmp[2], first=3)
                               playState$latticist$spec$x.relation <- x.relation
                               playState$latticist$spec$y.relation <- y.relation
                               reCompose(playState)
                           })
            scalesBox$packStart(scalesW)
        }
        cvarsBox$packStart(scalesBox, expand=FALSE, padding=1)
        box$packStart(cvarsBox, padding=1)

        ## add it directly to the window (not a toolbar!)
        ## use blockRedraws() to maintain current device size

        if (!is.null(playState$widgets$latticist)) {
            hideWidgetNoRedraw(playState$widgets$latticist,
                               horiz = TRUE,
                               playState = playState)
            playState$widgets$latticist$destroy()
        }
        blockRedraws({
            playState$widgets$vbox$packEnd(box, expand=FALSE)
        }, playState = playState)

        playState$widgets$latticist <- box

        return(NA)
    }
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

