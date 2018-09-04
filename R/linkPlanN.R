#' linkPlanN
#'
#' Tries to create a link plan for N agents, starting from the center of the target link structure. 
#' The portals are grouped in N sectors (one for each agent) which are accessed by distance from center
#' and linked back to all other portals.
#'
#' @note Is is not clear if this approach leads always to the maximum field number!
#'
#' @param g web: the current link plan
#' @param agents numeric: the number of agents (default: 1) 
#' @param start numeric: number of start portal (default: the "most" inner portal) 
#' @param dir numeric: direction to rotate sectors (default: 0)  
#'
#' @return a web with a way and link plan
#' @export
#' @importFrom grDevices chull
#' @importFrom stats quantile
#'
#' @examples
linkPlanN <- function (g, agents=1, start=NULL, dir=0) {
  conhull <- function(px, set) {
    ind <- chull(px[set,1], px[set,2])
    set[ind]
  }
  #
  ind <- rev(g)
  gv  <- g$vertices[ind,]
  nv  <- nrow(gv)
  if (is.null(start)) {
    seqn <- 1:nv
    while(length(seqn)) {
      last <- seqn
      seqn <- setdiff(seqn, conhull(gv, seqn))
    }
    start <- if (length(last)==1) last else sample(last, 1)
  } else {
    start <- which(ind==start)
  }
  g$plan <- cbind(1:agents, rep(start, agents), rep(NA, agents))
  gs     <- scale(gv, center=gv[start,], scale=FALSE)
  ga     <- atan2(gs[,2], gs[,1])
  gr     <- order(rowSums(gs^2))
  breaks <- if (agents==1) c(-pi, pi) else c(-pi, quantile(ga, (1:(agents-1))/agents), pi)
  gc     <- findInterval(ga, breaks)
  gc[start] <- 0
  for (i in 2:nv) {
    agent  <- gc[gr[i]]
    plani  <- linkBack(gv, g$edges, g$faces, g$pts, gr[i], which(gc==0))
    g$plan <- rbind(g$plan, cbind(rep(agent, nrow(plani)), plani))
    gc[gr[i]] <- 0
  }
  g
}