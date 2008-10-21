
library(playwith)

## A mini application (a playwith toolbar) for clustering.
## (still a work in progress...)

clusterApp <- function(data, ...)
{
    param <- list()
    param$clust.method <-
        list(c("complete", "ward", "single", "average",
               "mcquitty", "median", "centroid"),
             label = "Clustering method")
    param$dist.method <-
        list(c("euclidean", "maximum", "manhattan",
               "canberra", "binary", "minkowski"),
             label = "Distance method")
    param$cut.tree <-
        list(function(playState) {
            foo <- playPointInput(playState)
            if (is.null(foo$coords)) {
                ## user cancelled; remove clusters
                playState$env$clusters <-
                    rep(0, length(xyCoords()$x))
                playClear(playState, type = "annotations",
                          redraw = FALSE)
            } else {
                height <- foo$coords$y[1]
                ## store cluster vector
                playState$env$clusters <-
                    cutree(callArg(playState, 1),
                           h = height)
                annLine <- call("panel.abline", h = height,
                                col = "red", lty = 2)
                playAnnotate(playState, annLine, add = FALSE,
                             redraw = FALSE)
            }
            ## redraw to trigger the update.action
            playReplot(playState)
            ## and update any linked plots
            updateLinkedSubscribers(playState, redraw = TRUE)
        }, label = "Cut Tree")

    drawClusterPoints <- function(playState) {
        refState <- playState$env$refState
        if (is.null(refState))
            refState <- playState
        clusters <- refState$env$clusters
        if (is.null(clusters)) return()
        coords <- xyCoords(playState)
        playDo(playState,
               quote(panel.xyplot(coords$x, coords$y,
                  pch = 21, groups = clusters, subscripts = TRUE))
               )
    }

    param$parallel <-
        list(function(playState) {
            refState <- playState
            playwith(parallel(refState$env$data,
                         groups = refState$env$clusters,
                         main = "Parallel Coordinates plot"),
                     new = TRUE,
                     link.to = playState)
        }, label = "Parallel")

    param$marginals <-
        list(function(playState) {
            refState <- playState
            playwith(marginal.plot(refState$env$data,
                         groups = refState$env$clusters,
                         main = "Marginal distributions"),
                     new = TRUE,
                     link.to = playState)
        }, label = "Marginals")

    param$mds <-
        list(function(playState) {
            refState <- playState
            playwith({dist <- eval(refState$call[[2]][[2]], refState$env)
                      cmd <- cmdscale(dist)
                      plot(cmd, main = "Classical Multidimensional Scaling")
                      text(cmd, rownames(cmd))},
                     new = TRUE,
                     update.actions = list(drawClusterPoints),
                     link.to = playState)
        }, label = "MDS Plot")

    initClust <- function(playState)
        playState$env$clusters <- rep(0, length(xyCoords()$x))

    playwith(plot(hclust(dist(data, method = dist.method),
                         method = clust.method)),
             parameters = param,
             update.actions = list(drawClusterPoints),
             init.actions = list(initClust),
             click.mode = "Brush",
             ...)
}

clusterApp(USArrests)

