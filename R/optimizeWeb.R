#' optimizeWeb
#'
#' Tries to find an error free linking structure according to the target linking structure 
#' on a given set of portals. A random search is done by swaping portals of the target linking 
#' structure to a portal of the given portal set. If a portal is selected then a swaping portal 
#' is choosen. If \code{prob=FALSE} then each other portal has the same probability to be selected.
#' Otherwise the probability is distant dependent, portals farer away are less likely to be selected. 
#'
#' @param gi a web
#' @param maxit numeric: maximal number of iterations (default: 100000)
#' @param plot numeric: minimal time (in seconds) between consecutive plots (default: 5)
#' @param maxt numeric: maximal time (in seconds) the optimization runs (default: Inf)
#' @param prob logical: if the swap probability is distant dependent or constant  (default: \code{TRUE})
#'
#' @return a web
#' @export
#' @importFrom stats dist
#'
#' @examples
#' xy <- cbind(runif(20), runif(20))
#' g  <- fishbone(8)
#' gf <- web(g, xy)
#' gs <- optimizeWeb(gf)
optimizeWeb <- function(gi, maxit=100000, plot=5, maxt=Inf, prob=TRUE) {
  fi <- evaluate(gi)
  if (plot>0) {
    plot(gi)
    Sys.sleep(0)
  }
  nv <- nrow(gi$vertices)
  if (prob) {
    probsel <- as.matrix(dist(gi$vertices))
    p       <- c(0, (nv-1):1)  
    for (i in 1:nv) probsel[order(probsel[,i]),i] <- p
  } else {
    probsel <- 1-diag(nv)
  }
  plott <- start <- Sys.time()
  seqn  <- 1:nv
  t     <- 1
  repeat {
    mapt <- gi$map
    swap1 <- sample(seqn[!is.na(gi$map)], 1)
    swap2 <- sample(seqn, 1, prob=probsel[,swap1])
    gt   <- swap(gi, swap1, swap2) 
    ft   <- evaluate(gt)
    #  print(ft)
    if (ft<=fi) { # improvement
      if (fi==0) break
      if ((maxit<0) && ((start-Sys.time())<maxit)) break
      gi <- gt
      fi <- ft
      if ((plot>0) && ((Sys.time()-plott)>plot)) {
        plott <- Sys.time()
        plot(gi)
        Sys.sleep(0)    
      }
    }
    if ((Sys.time()-start)>maxt) break
    t <- t+1
    if (t>maxit) break
  }
  if (plot>0) {
    plot(gi, sub=sprintf("%.0f iterations", t))
    Sys.sleep(0)  
  }
  attr(gi, 'iter') <- t
  gi
}