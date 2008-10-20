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
             eval.args = playwith.getOption("eval.args"),
             height = playwith.getOption("height") - 1,
             plot.call)
{
    if (!missing(dat)) {
        ## dat argument was given (normal usage)
        datArg <- substitute(dat)
        if (missing(plot.call))
            plot.call <-
                latticistCompose(dat, spec, datArg = datArg)

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

    ## lattInit is the constructor (an init.action)
    lattAction <- latticistToolConstructor(dat, datArg = datArg)
    ## this list will store state in playState$latticist
    lattList <- latticistInitOptions(dat)

    if (!is.list(eval.args))
        eval.args <- as.list(eval.args)
    eval.args$envir <- parent.frame()

    playwith(plot.call = plot.call,
             eval.args = eval.args,
             title = title, ...,
             height = height,
             labels = labels,
             time.mode = time.mode,
             init.actions = list(latticist = lattAction),
             latticist = lattList)
}

latticistInitOptions <- function(dat)
{
    stuff <- list()

    ## options
    stuff$linesSetting <- TRUE

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
          "1:nrow(dat)")

    ## subsets -- preload some useful subsets
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
    stuff$subsetopts <- subsetopts

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

        ## callback function
        ## creates a new plot from widget settings
        composePlot <- function(playState, newXY = FALSE) {
            ## this is needed by handler functions to fiddle with GUI:
            if (!isTRUE(playState$tmp$plot.ready)) return()
            ## latticist specification list
            spec <- list()
            ## get expressions from GUI widgets
            tryParse <- function(x) {
                if (x %in% NULLNAMES) return(NULL)
                parse(text = x)[[1]]
            }
            spec$xvar <- tryParse(xvarW$getActiveText())
            spec$yvar <- tryParse(yvarW$getActiveText())
            spec$zvar <- tryParse(zvarW$getActiveText())
            if (xonW["active"] == FALSE) spec$xvar <- NULL
            if (yonW["active"] == FALSE) spec$yvar <- NULL
            spec$cond <- tryParse(c1W$getActiveText())
            spec$cond2 <- tryParse(c2W$getActiveText())
            spec$groups <- tryParse(groupsW$getActiveText())
            spec$subset <- tryParse(subsetW$getActiveText())
            spec$xdisc <- xdiscW["active"]
            spec$ydisc <- ydiscW["active"]
            spec$xprop <- xpropW["active"]
            spec$yprop <- ypropW["active"]
            ## parse aspect and scales except if new xy structure
            if (!newXY) {
                spec$aspect <- tryParse(aspectW$getActiveText())
                scalesIdx <- scalesW$getActive() + 1
                if (scalesIdx > 0) {
                    scalesopts <- playState$latticist$scalesopts
                    tmp <- strsplit(scalesopts[scalesIdx], ", ")[[1]]
                    spec$x.relation <- substring(tmp[1], first=3)
                    spec$y.relation <- substring(tmp[2], first=3)
                }
            }
            ## options settings
            spec$nLevels <- nLevelsW["value"]
            spec$doLines <- playState$latticist$linesSetting
            spec$doTile <- tileW["active"] #### TODO: does not keep state...
            spec$doSegments <- segmentsW["active"]
            spec$doAsError <- aserrorW["active"]
            spec$do3DTable <- playState$latticist$do3DTable
            spec$defaultPlot <- playState$latticist$defaultPlot
            spec$varSubset <- playState$latticist$varSubset
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
            playState$tmp$plot.ready <- FALSE
            on.exit(playState$tmp$plot.ready <- TRUE)
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
            on.exit(playState$tmp$plot.ready <- TRUE)
            xdiscW["active"] <- TRUE
            ydiscW["active"] <- TRUE
            playState$tmp$plot.ready <- TRUE
            doRecompose(playState = playState)
            return(FALSE)
        }
        handler.unbin <- function(widget, event, playState) {
            playState$tmp$plot.ready <- FALSE
            on.exit(playState$tmp$plot.ready <- TRUE)
            xdiscW["active"] <- FALSE
            ydiscW["active"] <- FALSE
            playState$tmp$plot.ready <- TRUE
            doRecompose(playState = playState)
            return(FALSE)
        }
        handler.superpose <- function(widget, event, playState) {
            playState$tmp$plot.ready <- FALSE
            on.exit(playState$tmp$plot.ready <- TRUE)
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
            on.exit(playState$tmp$plot.ready <- TRUE)
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
            on.exit(playState$tmp$plot.ready <- TRUE)
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
            on.exit(playState$tmp$plot.ready <- TRUE)
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
            playState$latticist$defaultPlot <- opt
            ## reset
            playState$tmp$plot.ready <- FALSE
            on.exit(playState$tmp$plot.ready <- TRUE)
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
            checked <- playState$latticist$varSubset
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
                             playState$latticist$varSubset <- varsub
                             dispose(h$obj)
                             doRecomposeNewXY(playState = playState)
                         })
        }


        niceButton <- function(label) {
            butt <- gtkEventBox()
            tmp <- gtkLabel(label)
            tmp$setMarkup(paste('<span foreground="blue"><u>',
                                label, '</u></span>', sep=""))
            butt$add(tmp)
            butt
        }

        ## extract latticist specification from plot call
        spec <- latticistParse(mainCall(playState),
                               trellis = playState$trellis)
        xvar <- spec$xvar
        yvar <- spec$yvar
        zvar <- spec$zvar
        c1 <- spec$cond
        c2 <- spec$cond2
        groups <- spec$groups
        subset <- spec$subset
        aspect <- spec$aspect
        aspect3D <- spec$aspect3D
        xdisc <- isTRUE(spec$xdisc)
        ydisc <- isTRUE(spec$ydisc)

        nLevels <- INIT.NLEVELS
        if (!is.null(spec$nLevels))
            nLevels <- spec$nLevels

        ## set up variables and options
        xvarStr <- deparseOneLine(xvar)
        yvarStr <- deparseOneLine(yvar)
        zvarStr <- deparseOneLine(zvar)
        c1Str <- deparseOneLine(c1)
        c2Str <- deparseOneLine(c2)
        groupsStr <- deparseOneLine(groups)

        ## update stored lists of variable expressions
        varexprs <- playState$latticist$varexprs
        varexprs <- unique(c(varexprs,
                             xvarStr, yvarStr, zvarStr,
                             c1Str, c2Str, groupsStr))
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

        ## lines setting
        linesVal <- playState$latticist$linesSetting
        if (is.null(linesVal)) {
            linesVal <- TRUE
            playState$latticist$linesSetting <- linesVal
        }

        ## evaluate variables to determine data types
        xcat <- is.categorical(eval(xvar, dat))
        ycat <- is.categorical(eval(yvar, dat))
        gcat <- is.categorical(eval(groups, dat))


        ## CREATE THE GUI

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
        choosevarsW["visible"] <- (is.null(xvar) && is.null(yvar))
        gSignalConnect(choosevarsW, "button-press-event",
                       handler.choosevars, data=playState)
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
                       handler.gohyper, data = "marginal.plot")
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
        if (!is.null(xvar) || !is.null(yvar))
            labtxt <- " Variables on axes: "
        xyLabelW <- gtkLabel(labtxt)
        xyLabelW$setMarkup(paste("<b>", labtxt, "</b>", sep=""))
        xyBox$packStart(xyLabelW, expand=FALSE)
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
        ypropW["active"] <- isTRUE(spec$yprop)
        ypropW["visible"] <- xcat && ycat
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
        xpropW["active"] <- isTRUE(spec$xprop)
        xpropW["visible"] <- xcat && ycat
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
        tileW["active"] <- isTRUE(playState$latticist$doTile)
        tileW["visible"] <- !is.null(groups) && !gcat
        gSignalConnect(tileW, "clicked",
                       doRecompose, data=playState)
        gBox$packStart(tileW)
        gzBox$packStart(gBox, expand=FALSE)

        ## Z / SEGMENTS VARIABLE
        zBox <- gtkHBox()
        zBox$packStart(gtkLabel(" Depth (3D)"), expand=FALSE)
        ## segments option
        segmentsW <- gtkCheckButton("Segments (x--z) ")
        segmentsW["active"] <- isTRUE(playState$latticist$doSegments)
        segmentsW["visible"] <- !is.null(xvar) && !is.null(yvar)
        gSignalConnect(segmentsW, "clicked",
                       doRecomposeNewXY, data=playState)
        orLabelW <- gtkLabel(" or")
        orLabelW["visible"] <- segmentsW["visible"]
        zBox$packStart(orLabelW)
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
        aserrorW["active"] <- isTRUE(playState$latticist$doAsError)
        aserrorW["visible"] <- isTRUE(playState$latticist$doSegments)
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
        ## use blockRedraws() to maintain current device size
        blockRedraws({
            if (!is.null(playState$widgets$latticist))
                playState$widgets$latticist$destroy()
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

