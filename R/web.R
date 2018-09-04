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
#' @param rot numeric: number of rotations (if >0) or iterations (if <0) to find a good starting position (default: 8)
#' @param ... further parameters (unused)
#'
#' @return web with starting configuration
#' @rdname web
#' @export web
#' @import magrittr
#' @importFrom stats optim
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
web.default <- function (g, px, k=10, rot=256, ...) {
  nx <- nrow(px)
  ng <- nrow(g$vertices)
  if (nx+2<ng) stop("vertices(x)<vertices(g)")
  if (k>ng) k <- ng
  #
  gv  <- scale(g$vertices)
  pv  <- scale(px)
  pr  <- sqrt(rowSums(gv^2))
  if (rot>0) {
    pa  <- atan2(gv[,2], gv[,1])
    ret <- list()
    for (a in 1:rot) { # rotate structure
      gv    <- cbind(pr*cos(pa+2*a*pi/rot), pr*sin(pa+2*a*pi/rot))
      index <- FNN::knnx.index(data =cbind(atan2(pv[,2],pv[,1]), sqrt(rowSums(pv^2))),
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
    ret  <- ret[[which.min(eret)]]
  } else {
    res <- optim(par=c(1,0,0,1,0,0), icpdist, query=gv, data=pv,
                 control=list(maxit=-rot))
    if (res$convergence) warning("No convergence reached, increase 'maxit'?")
    tgv <- icpdist(res$par, query=gv, data=pv, full=TRUE)
    map <- rep(NA, nrow(px))
    map[tgv$index] <- 1:ng
    ret <- list(vertices=px, map=map, edges=g$edges, faces=g$faces, pts=g$pts)
    class(ret) <- 'web'
    ret$error  <- evaluateC(ret)
  }
  return(ret)
}