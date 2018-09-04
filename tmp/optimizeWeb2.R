#' optimizeWeb
#'
#' Optimizes 
#'
#' @param gi a web
#' @param maxit numeric: maximal number of iterations (default: 100000)
#' @param plot numeric: minimal time (in seconds) between consecutive plots (default: 5)
#' @param maxt numeric: maximal time (in seconds) the optimization runs (default: Inf)
#'
#' @return a web
#' @export
#'
#' @examples
#' xy <- cbind(runif(20), runif(20))
#' g  <- fishbone(8)
#' gf <- web(g, xy)
#' gs <- optimizeWeb(gf)
optimizeWeb2 <- function(gi, maxit=100000, plot=5, maxt=Inf) {
  fi <- evaluate(gi)
  if (plot>0) {
    plot(gi)
    Sys.sleep(0)
  }
  plott <- start <- Sys.time()
  seqn  <- 1:nrow(gi$vertices)
  probv <- as.matrix(1/dist(g$vertices, 'manhattan'))
  t     <- 1
  repeat {
    mapt <- gi$map
    swap1 <- sample(seqn[!is.na(gi$map)], 1)
    swap2 <- sample(seqn[-swap1], 1, prob=probv[-swap1,swap1])
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
  gi
}