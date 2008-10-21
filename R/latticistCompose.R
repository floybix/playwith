## playwith: interactive plots in R using GTK+
##
## Copyright (c) 2008 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer

LOTS <- 1000
HEAPS <- 8000
MAXPANELS <- 16
INIT.NLEVELS <- 4

latticistCompose <- function(dat, spec = alist(),
                             datArg = substitute(dat))
{
    force(datArg)
    force(spec)
    if (inherits(spec, "formula"))
        spec <- list(spec)
    if (length(spec)) {
        form <- spec[[1]]
        if (inherits(spec[[1]], "formula") ||
            is.call.to(spec[[1]], "~")) {
            form <- eval(form)
            yvar <- form[[2]]
            if (length(form) > 2)
                xvar <- form[[3]]
            ## TODO...
        }

    }
    doCompose <-
        function(xvar = NULL, yvar = NULL, zvar = NULL,
                 groups = NULL, cond = NULL, cond2 = NULL,
                 subset = NULL, varSubset = NULL,
                 aspect = NULL, aspect3D = NULL,
                 x.relation = NULL, y.relation = NULL,
                 xdisc = FALSE, ydisc = FALSE, nLevels = NULL,
                 xprop = FALSE, yprop = FALSE, gprop = FALSE,
                 doTile = FALSE, doSegments = FALSE, doAsError = FALSE,
                 doLines = TRUE, do3DTable = FALSE,
                 defaultPlot = "marginal.plot")
        {
            ## TODO: handle gprop

            xvar <- substitute(xvar)
            yvar <- substitute(yvar)
            zvar <- substitute(zvar)
            groups <- substitute(groups)
            c1 <- substitute(cond)
            c2 <- substitute(cond2)
            subset <- substitute(subset)
            if (is.null(nLevels)) nLevels <- INIT.NLEVELS

            deparse1 <- function(expr)
                paste(deparse(expr, width = 500, control = NULL),
                      collapse = " ")

            ## keep for titles etc
            xvarOrigStr <- deparse1(xvar)
            yvarOrigStr <- deparse1(yvar)
            zvarOrigStr <- deparse1(zvar)
            c1OrigStr <- deparse1(c1)
            c2OrigStr <- deparse1(c2)
            groupsOrigStr <- deparse1(groups)
            if (is.null(subset)) subset <- TRUE

            ## evaluate to check types
            xVal <- eval(xvar, dat)
            yVal <- eval(yvar, dat)
            zVal <- eval(zvar, dat)
            c1Val <- eval(c1, dat)
            c2Val <- eval(c2, dat)
            groupsVal <- eval(groups, dat)
            subsetVal <- eval(subset, dat)
            ## calculate number of data points
            nPoints <- 0
            if (is.null(xVal) && is.null(yVal)) {
                ## hypervariate: just report size of subset
                if (isTRUE(subsetVal)) {
                    nPoints <- NROW(dat)
                } else {
                    ## handle integer/logical/recycling
                    tmp <- rep(TRUE, NROW(dat))
                    nPoints <- sum(tmp[subsetVal])
                }
            } else if (!is.null(xVal) && !is.null(yVal))
                nPoints <- sum(is.finite(xVal[subsetVal]) &
                               is.finite(yVal[subsetVal]))
            else if (!is.null(xVal))
                nPoints <- sum(is.finite(xVal[subsetVal]))
            else if (!is.null(yVal))
                nPoints <- sum(is.finite(yVal[subsetVal]))

            isUnordered <- function(x, val) {
                ## need this because is.ordered(cut()) == FALSE!
                if (is.call.to(x, "cut")) return(FALSE)
                if (is.call.to(x, "cut2")) return(FALSE)
                ## assumes is.categorical(val)
                !is.ordered(val) && !is.shingle(val)
            }

            ## discretize
            do.xdisc <- (!is.null(xvar) && !is.categorical(xVal) && xdisc)
            do.ydisc <- (!is.null(yvar) && !is.categorical(yVal) && ydisc)
            do.c1disc <- (!is.null(c1) && !is.categorical(c1Val))
            do.c2disc <- (!is.null(c2) && !is.categorical(c2Val))
            do.gdisc <- (!is.null(groups) && !is.categorical(groupsVal))

            ## if there are any numerical variables on plot
            ## we can use shingles (otherwise plotting a "table" method)
            anyNumerics <- ((!is.null(xvar) && !is.categorical(xVal) && !do.xdisc) ||
                            (!is.null(yvar) && !is.categorical(yVal) && !do.ydisc))
            if (anyNumerics) {
                ## use shingles where appropriate
                if (do.xdisc) xvar <- call("equal.count", xvar, nLevels) ## or cut?
                if (do.ydisc) yvar <- call("equal.count", yvar, nLevels) ## or cut?
                ## conditioning variables
                if (do.c1disc) c1 <- call("equal.count", c1, nLevels)
                if (do.c2disc) c2 <- call("equal.count", c2, nLevels)
            } else {
                ## table method, need factors not shingles
                if (do.xdisc) {
                    if (is.null(yvar) || do.ydisc)
                        xvar <- call("cut", xvar, nLevels)
                    else xvar <- call("cutEq", xvar, nLevels)
                }
                if (do.ydisc) {
                    if (is.null(xvar) || do.xdisc)
                        yvar <- call("cut", yvar, nLevels)
                    else yvar <- call("cutEq", yvar, nLevels)
                }
                if (do.c1disc) c1 <- call("cutEq", c1, nLevels)
                if (do.c2disc) c2 <- call("cutEq", c2, nLevels)
            }
            if (do.gdisc) groups <- call("cutEq", groups, nLevels)
            ## re-evaluate data if changed
            if (do.xdisc) xVal <- eval(xvar, dat)
            if (do.ydisc) yVal <- eval(yvar, dat)
            if (do.c1disc) c1Val <- eval(c1, dat)
            if (do.c2disc) c2Val <- eval(c2, dat)
            if (do.gdisc) groupsVal <- eval(groups, dat) ## TODO avoid this?

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
            call <- quote(xyplot(0 ~ 0))
            call$data <- datArg
            call$groups <- groups ## may be NULL
            call$subset <-
                if (!isTRUE(subset)) subset else NULL
            ## build up 'scales' list and assign at the end
            scales <- list()

            ## put shingle levels on axis
            if (anyNumerics) {
                if (do.xdisc)
                    scales$x$limits <-
                        as.character(levels(xVal))
                if (do.ydisc)
                    scales$y$limits <-
                        as.character(levels(yVal))
            }
            ## put shingle levels in strip
            if (anyNumerics && do.c1disc && (is.null(c2) || do.c2disc))
                call$strip <-
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
                call$main <- title
            }

            ## axis labels (not for categoricals)
            if (!is.null(xvar) && !is.categorical(xVal))
                call$xlab <- xvarOrigStr
            if (!is.null(yvar) && !is.categorical(yVal))
                call$ylab <- yvarOrigStr

            ## choose plot type and formula
            if (is.null(xVal) && is.null(yVal)) {
                ## HYPERVARIATE

                dat.expr <- datArg
                if (!is.null(varSubset))
                    dat.expr <- call("[", datArg, varSubset)
                dat.form <- call("~", dat.expr)
                if (!is.null(cond))
                    dat.form <- call("~", call("|", dat.expr, cond))

                if (defaultPlot == "marginal.plot") {
                    call[[1]] <- quote(marginal.plot)
                    call[[2]] <- dat.expr
                    call$reorder <- FALSE

                } else if (defaultPlot == "splom") {
                    call[[1]] <- quote(splom)
                    call[[2]] <- dat.form
                    call$varname.cex <- 0.7
                    call$pscales <- 0
                    if (doLines)
                        call$lower.panel <-
                            function(..., type)
                                panel.xyplot(..., type = "smooth")

                } else if (defaultPlot == "parallel") {
                    call[[1]] <- quote(parallel)
                    call[[2]] <- dat.form
                    if (is.null(groups))
                        call$col <- quote(trellis.par.get("plot.line")$col)
                }

            } else if (is.null(xVal) || is.null(yVal)) {
                ## UNIVARIATE

                if (is.categorical(xVal) || is.categorical(yVal)) {
                    ## UNIVARIATE CATEGORICAL
                    if (!is.null(yVal)) {
                        ## data on y axis, use dotplot
                        call[[1]] <- quote(dotplot)
                        call$data <- NULL
                        call$subset <- NULL
                        xterms <- paste(c(deparse1(yvar),
                                          if (!is.null(c1)) deparse1(c1),
                                          if (!is.null(c2)) deparse1(c2),
                                          if (!is.null(groups)) deparse1(groups)),
                                        collapse=" + ")
                        ## and set logical `groups` argument
                        call$groups <- !is.null(groups)
                        xform <- as.formula(paste("~", xterms))
                        tabcall <-
                            call("xtabs", xform, datArg)
                        tabcall$subset <- if (!isTRUE(subset)) subset
                        if (yprop) {
                            tabcall <- call("prop.table", tabcall, margin = 1)
                        }
                        call[[2]] <- tabcall

                        if (doLines) {
                            if (!is.null(groups))
                                call$type <- c("p", "l")
                            else
                                call$type <- c("p", "h")
                            call$origin <- 0
                        }

                    } else {
                        ## data on x axis, use barchart
                        ## (just for variety? & dotplot.table has no horizontal=FALSE)
                        ## BUT if x is a discretized numeric, use histogram
                        if (do.xdisc && is.null(groups)) {
                            xvar <- xvar[[2]] ## undo disc function
                            call[[1]] <- quote(histogram)
                            if (!is.null(cond))
                                call[[2]] <- call("~", call("|", xvar, cond))
                            else
                                call[[2]] <- call("~", xvar)
                        } else {
                            call[[1]] <- quote(barchart)
                            call$data <- NULL
                            call$subset <- NULL
                            xterms <- paste(c(deparse1(xvar),
                                              if (!is.null(c1)) deparse1(c1),
                                              if (!is.null(c2)) deparse1(c2),
                                              if (!is.null(groups)) deparse1(groups)),
                                            collapse=" + ")
                            ## and set logical `groups` argument
                            call$groups <- !is.null(groups)
                            xform <- as.formula(paste("~", xterms))
                            tabcall <-
                                call("xtabs", xform, datArg)
                            tabcall$subset <- if (!isTRUE(subset)) subset
                            if (xprop) {
                                tabcall <- call("prop.table", tabcall, margin = 1)
                            }
                            call[[2]] <- tabcall
                            ## TODO: make stack an option?
                            call$stack <- TRUE
                            call$horizontal <- FALSE
                        }
                    }

                } else {
                    ## UNIVARIATE NUMERIC
                    if (!is.null(xVal)) {
                        ## data on x axis, use densityplot
                        call[[1]] <- quote(densityplot)
                        if (!is.null(cond))
                            call[[2]] <- call("~", call("|", xvar, cond))
                        else
                            call[[2]] <- call("~", xvar)
                        ## settings depend on number of points, groups
                        if (nPoints >= HEAPS) {
                            call$plot.points <- FALSE
                        } else if (nPoints >= LOTS) {
                            call$plot.points <- "jitter"
                            call$pch <- "+" ## like jittered rug
                        }
                        call$ref <- TRUE
                    } else {
                        ## data on y axis, use qqmath
                        call[[1]] <- quote(qqmath)
                        if (!is.null(cond))
                            call[[2]] <- call("~", call("|", yvar, cond))
                        else
                            call[[2]] <- call("~", yvar)
                        ## settings depend on number of points, groups
                        type <- "p"
                        if (nPoints >= HEAPS) {
                            call$f.value <- quote(ppoints(100))
                            if (doLines) type <- "l"
                        } else {
                            if (doLines) type <- "o"
                        }
                        ## a grid is always useful with qqmath
                        call$type <- c("g", type)
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
                            call$distribution <- quote(qlnorm)
                            call$xlab <- "Log-normal quantiles"
                        } else if (tst <= 1.2) {
                            ## uniform distribution
                            call$distribution <- quote(qunif)
                            call$xlab <- expression("Proportion" <= y)
                        } else {
                            call$distribution <- quote(qnorm)
                            call$xlab <- "Probability (normal distribution)"
                            ## label probabilites on axis
                            ## TODO: axis.components function to do this
                            probs <- c(0.001, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 0.999)
                            scales$x$at <- bquote(qnorm(.(probs)))
                            scales$x$labels <- probs
                        }
                        call$prepanel <- quote(prepanel.qqmathline)
                    }
                }

            } else if (is.null(zVal)) {
                ## BIVARIATE

                if (is.categorical(xVal) && is.categorical(yVal)) {
                    ## BIVARIATE CATEGORICAL
                    if (do.xdisc && do.ydisc && require("hexbin")) {
                        ## if both are discretized numerics, use 2D binning
                        ## use hexbin package if available
                        call[[1]] <- quote(hexbinplot)
                        xvar <- xvar[[2]] ## undo disc function
                        yvar <- yvar[[2]] ## undo disc function
                        if (!is.null(cond))
                            call[[2]] <- call("~", yvar, call("|", xvar, cond))
                        else
                            call[[2]] <- call("~", yvar, xvar)
                        type <- if (doLines) "r"
                        if (!is.null(cond)) type <- c("g", type)
                        call$type <- type
                        if (is.null(aspect))
                            aspect <- 1
                        ## ignore groups setting
                        call$groups <- NULL

                    } else {
                        ## 2D TABLE (3D BARCHART)
                        call[[1]] <- quote(cloud)
                        call$data <- NULL
                        call$subset <- NULL
                        ## reorder factor levels
                        if (isUnordered(yvar, yVal)) {
                            if (nlevels(yVal) > 2) {
                                yvar <- call("reorder", yvar,
                                             call("unclass", xvar), na.rm=T)
                                yVal <- eval(yvar, dat)
                            }
                        }
                        xterms <- paste(c(deparse1(xvar),
                                          deparse1(yvar),
                                          if (!is.null(c1)) deparse1(c1),
                                          if (!is.null(c2)) deparse1(c2),
                                          if (!is.null(groups)) deparse1(groups)),
                                        collapse=" + ")
                        xform <- as.formula(paste("~", xterms))
                        tabcall <- call("xtabs", xform, datArg)
                        tabcall$subset <- if (!isTRUE(subset)) subset
                        if (xprop || yprop) {
                            mgn <- c(if (xprop) 1, if (yprop) 2)
                            tabcall <- call("prop.table", tabcall, margin = mgn)
                        }
                        call[[2]] <- tabcall
                        if (do3DTable) {
                            ## and set logical `groups` argument
                            call$groups <- !is.null(groups)
                            call$panel.3d.cloud <- quote(panel.3dbars)
                            call$col.facet <- "grey"
                            call$xbase <- 0.4
                            call$ybase <- 0.4
                            ## rotate view 180 around z axis (better with reordering)
                            call$screen <-
                                list(z = 180, z = 40, x = -60)
                            ## set aspect so that bars have square bases, like "iso"
                            ## aspect for cloud is in the form c(y/x, z/x)
                            asp.y.x <- length(levelsOK(yVal)) / length(levelsOK(xVal))
                            call$aspect <- round(c(asp.y.x, min(asp.y.x, 1)), 2)
                            scales$z$draw <- FALSE
                            call$xlab <- expression(NULL)
                            call$ylab <- expression(NULL)
                            call$zlab <- expression(NULL)
                                        # scales = list(rot = 90) # TODO use generic code below
                        } else {
                            ## levelplot: use color rather than depth for frequencies
                            call[[1]] <- quote(levelplot)
                        }
                    }
                } else

                if (is.categorical(yVal) || is.categorical(xVal)) {
                    ## BIVARIATE CATEGORICAL AND NUMERIC

                    ## TODO: if only one value for each level use dotplot

                    if (is.logical(xVal))
                        call$horizontal <- FALSE

                    ## reorder factor levels if more than 2
                    if (is.categorical(yVal) && isUnordered(yvar, yVal)) {
                        if (nlevels(yVal) > 2) {
                            yvar <- call("reorder", yvar, xvar, na.rm=T)
                            yVal <- eval(yvar, dat)
                        }
                    }
                    if (is.categorical(xVal) && isUnordered(xvar, xVal)) {
                        if (nlevels(xVal) > 2) {
                            xvar <- call("reorder", xvar, yvar, na.rm=T)
                            xVal <- eval(xvar, dat)
                        }
                    }
                    ## formula
                    if (!is.null(cond))
                        call[[2]] <- call("~", yvar, call("|", xvar, cond))
                    else
                        call[[2]] <- call("~", yvar, xvar)

                    if (!is.null(groups)) {
                        call[[1]] <- quote(stripplot)
                        call$jitter.data <- TRUE
                        if (doLines) {
                            call$type <- c("p", "a")
                            ## TODO: check whether there are any missing values
                                        #call$fun <- quote(median)
                            call$fun <- quote(function(x) median(x, na.rm=TRUE))
                        }

                    } else {
                        call[[1]] <- quote(bwplot)
                                        #call$origin <- 0
                        call$varwidth <- FALSE
                    }

                } else {
                    ## BIVARIATE NUMERIC
                    call[[1]] <- quote(xyplot)
                    if (!is.null(cond))
                        call[[2]] <- call("~", yvar, call("|", xvar, cond))
                    else
                        call[[2]] <- call("~", yvar, xvar)

                    if (is.numeric(groupsVal) || do.gdisc) {
                        if (doTile) {
                            call[[1]] <- quote(tileplot)
                            if (require("tripack", quietly = TRUE))
                                call$use.tripack <- TRUE
                        } else {
                            call[[1]] <- quote(levelplot)
                            call$panel <- quote(panel.levelplot.points)
                            call$prepanel <- quote(prepanel.default.xyplot)
                        }
                        ## un-discretize groups
                        if (do.gdisc) groups <- groups[[2]]
                        form <- call("~", groups, call("*", xvar, yvar))
                        if (!is.null(cond))
                            form[[3]] <- call("|", form[[3]], cond)
                        call[[2]] <- form
                        call$groups <- NULL
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
                            call$jitter.x <- TRUE
                            if (all(panelType$lines))
                                call$type <- c("p", "a")
                        } else {
                            if (all(panelType$lines))
                                call$type <- "o"
                        }

                        if (any(panelType$lines == FALSE)) {
                            ## use loess smoother
                            call$type <- c("p", "smooth")
                            call$prepanel <- quote(try.prepanel.loess)
                            ## do not worry about errors in loess.smooth
                            call$plot.args <- quote(list(panel.error = "warning"))
                        }
                    }
                }
            } else {
                ## TRIVARIATE

                if (doSegments) {
                    ## SEGMENTS
                    ## use segplot
                    call[[1]] <- quote(segplot)
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
                        call$centers <- xvar
                        call$draw.bands <- FALSE
                    }
                    if (!is.null(cond))
                        form[[3]] <- call("|", form[[3]], cond)
                    call[[2]] <- form
                    ## colors coded by "level"
                    if (do.gdisc) groups <- groups[[2]]
                    call$level <- groups
                    call$groups <- NULL

                } else {
                    ## TRIVARIATE 3D
                    ## use cloud
                    call[[1]] <- quote(cloud)

                    ## 3D NUMERIC (3D SCATTER)
                    form <- call("~", zvar, call("*", xvar, yvar))
                    if (!is.null(cond))
                        form[[3]] <- call("|", form[[3]], cond)
                    call[[2]] <- form
                    if (doLines)
                        call$type <- c("p", "h")
                    ## TODO: support color covariate

                }
            }

            ## generic stuff...

            ## aspect and scales

            if (is.call.to(call, "cloud")) {
                ## for 3D plots, aspect widget applies to "panel.aspect".
                ## set panel.aspect to "fill" by default if only one panel
                if (is.null(aspect) && is.null(cond))
                    aspect <- "fill"
                if (identical(eval(aspect), "fill"))
                    aspect <- round(dev.size()[2] / dev.size()[1], 2)
                if (is.numeric(aspect))
                    call$panel.aspect <- aspect
            } else {
                call$aspect <- aspect ## may be NULL
            }

            if (!is.null(x.relation) || !is.null(y.relation)) {
                ## either of these may be NULL
                scales$x$relation <- x.relation
                scales$y$relation <- y.relation
            }

            anyNumerics <- ((!is.null(xvar) && !is.categorical(xVal)) ||
                            (!is.null(yvar) && !is.categorical(yVal)) ||
                            (!is.null(zvar)))
            anyNumerics <- (anyNumerics ||
                            is.call.to(call, "splom"))
            ## style settings for points
            if (anyNumerics) {
                theme <- call("simpleTheme")
                if (is.categorical(groupsVal))
                    theme$cex <- 0.6
                if (ncond > 2)
                    theme$cex <- 0.6
                if (ncond > 6)
                    theme$cex <- 0.4
                if (is.call.to(call, "splom"))
                    theme$cex <- 0.5
                if ((nPoints >= LOTS) && is.null(call$f.value))
                {
                    theme$alpha.points <- if (nPoints >= HEAPS) 0.15 else 0.3
                    ## bug in lattice: grouped lines take alpha from points setting
                    if (!is.null(groups) &&
                        (packageDescription("lattice")$Version <= "0.17-12")) ##fixed yet?
                        theme$alpha.points <- if (nPoints >= HEAPS) 0.4 else 0.6
                }
                if (nPoints >= HEAPS) {
                    if (is.call.to(call, "xyplot") ||
                        is.call.to(call, "stripplot")) {
                        theme$pch <- "." ## or 0, empty square?
                        theme$cex <- 3
                    }
                }
                call$par.settings <- theme
            }
            ## add a grid if there are multiple panels
            if (anyNumerics && !is.null(cond)) {
                type <- call$type
                if (is.null(type)) type <- "p" ## assumed
                type <- unique(c("g", type))
                call$type <- type
            }

            ## set up key
            if (is.categorical(groupsVal)) {
                auto.key <- list()
                ## work out key type
                typeVal <- call$type
                if (all(c("p", "l") %in% typeVal)) {
                    typeVal <- c(setdiff(typeVal, c("p", "l")), "o")
                }
                if (is.call.to(call, "marginal.plot"))
                    typeVal <- c("p", "l")
                if (is.call.to(call, "parallel"))
                    typeVal <- "l"
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
                ## if groups are discretised, or hypervar, need a title
                if (do.gdisc || (is.null(xvar) && is.null(yvar))) {
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
                call$auto.key <- auto.key
            }

            ## fix up long x axis labels
            if (is.categorical(xVal) &&
                !is.call.to(call, "histogram"))
            {
                if (nlevels(xVal) >= 4) {
                    if (max(sapply(levelsOK(xVal), nchar)) >= 12) {
                        scales$x$rot <- 30
                        scales$x$cex <- 0.7
                                        #scales$x$abbreviate <- TRUE
                    } else
                    if ((nlevels(xVal) >= 8) ||
                        (mean(sapply(levelsOK(xVal), nchar)) >= 8)) {
                        scales$x$rot <- 60
                    }
                }
            }

            ## sub-title
            Rvers <- paste("R ", R.version$major, ".",
                           R.version$minor, R.version$status, sep="")
            subt <- if (nPoints > 0)
                paste("N = ", nPoints, ", ", sep="") else ""
            subt <- paste(subt, toString(Sys.Date()), ", ",
                          Rvers, sep="")
            if (!isTRUE(subset)) {
                subsetStr <- deparse1(subset)
                if (nchar(subsetStr) > 30)
                    subt <- call("paste", subsetStr, subt, sep="\n")
                else subt <- call("paste", subsetStr, subt, sep=", ")
            }
            call$sub <- list(subt, x=0.99, just="right",
                             cex=0.7, font=1)

            ## TODO: if (...)
            call$subscripts <- TRUE

            ## convert nested list 'scales' to quoted argument
            if (length(scales) > 0) {
                tmp <- try( parse(text = deparse(scales,
                                  control = NULL))[[1]] )
                if (!inherits(tmp, "try-error"))
                    call$scales <- tmp
            }

            ## layout
            if (tooManyPanels)
                call$layout <- c(0, min(MAXPANELS, ceiling(ncond / 2)))

            ## useOuterStrips with c1 and c2 (does not work with 'layout')
            if (!is.null(c1) && !is.null(c2) && !tooManyPanels) {
                call <- call("useOuterStrips", call)
            }

            call
        }
    do.call("doCompose", spec)
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

n.level.colors <-
    function(x, n.col = 30,
             at = do.breaks(range(x, finite = TRUE), n.col),
             ...)
{
    level.colors(x, at = at, ...)
}

simpleColorKey <-
    function(x, n.col = 30,
             at = do.breaks(range(x, finite = TRUE), n.col),
             ..., space = "right")
{
    foo <- list(space =
                list(fun = "draw.colorkey",
                     args = list(at = at, ...)))
    names(foo) <- space
    foo
}

