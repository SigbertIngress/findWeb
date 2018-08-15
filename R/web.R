#' @useDynLib findWeb
#' @importFrom Rcpp sourceCpp
NULL

#' web
#'
#' Creates an initial configuration given by g on a set of portals. 
#'
#' @param g web: a given linking structure
#' @param px numeric matrix: xy-coordinates of portals
#' @param k numeric: number of neighbours (default: 10)
#' @param ... further parameters (unused)
#'
#' @return web with starting configuration
#' @rdname web
#' @export web
#'
#' @examples
#' xy <- cbind(runif(30), runif(30))
#' g  <- fishbone(8)
#' gf <- web(g, xy)
#' plot(gf)
web <- function(g, ...) UseMethod("web")

#' @return \code{NULL}
#'
#' @rdname web
#' @method web default
#' @export
#' @importFrom FNN knnx.index
web.default <- function (g, px, k=10, ...) {
  nx <- nrow(px)
  ng <- nrow(g$vertices)
  if (nx+2<ng) stop("vertices(x)<vertices(g)")
  if (k>ng) k <- ng
  #
  gv  <- scale(g$vertices)
  pr  <- sqrt(rowSums(gv^2))
  pa  <- atan2(gv[,2], gv[,1])
  ret <- list()
  for (a in 1:16) { # rotate structure
    gv <- cbind(pr*cos(pa+a*pi/8), pr*sin(pa+a*pi/8))
    pv <- scale(px)
    index <- knnx.index(data =cbind(atan2(pv[,2],pv[,1]), sqrt(rowSums(pv^2))),
                        query=cbind(atan2(gv[,2],gv[,1]), sqrt(rowSums(gv^2))),
                        k=k)
    map <- rep(NA, nrow(px))
    for (i in 1:nrow(index)) {
      success <- FALSE
      for (j in 1:k) {
        if (is.na(map[index[i,j]])) {
          map[index[i,j]] <- i
          success <- TRUE
          break
        }
      }
      if (!success) stop ("Please increase k")
    }
    gr <- list(vertices=px, map=map, edges=g$edges, faces=g$faces, pts=g$pts)
    class(gr) <- 'web'
    gr$error  <- evaluateC(gr)
    ret[[a]]  <- gr
  }  
  eret <- sapply(ret, evaluate)
  return(ret[[which.min(eret)]])
}