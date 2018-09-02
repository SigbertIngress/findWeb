
#' getPlan
#'
#' After the way(s) and the link plan(s) are created they are shown, printed and returned invisibly- 
#' If \code{quiet=TRUE} then no printout will be generated. If \code{wait=0} then the agents move will not be plotted. 
#' A negative value for \code{wait} forces a keypress to continue after each agent move and a positive value makes a break of \code{wait}
#' seconds after each agent move.
#'
#' @param g a web with  way(s) and link plan(s)
#' @param names character vector: names of portals (default: use numbers)
#' @param quiet logical: print ways and plans (default: FALSE)
#' @param wait logical: how long to wait in seconds between agent moves (default: -1)
#'
#' @return a list with ways &link plans, required items (keys, Softbank ultralinks)
#' @export
#' @importFrom graphics points lines
#'
#' @examples
#' set.seed(0)
#' g  <- homogeneous(2)
#' x  <- cbind(runif(10), runif(10))
#' gi <- web(g, x)
#' g1 <- optimizeWeb(gi)
#' g2 <- linkPlan1(g1)
#' plan <- getPlan(g2, wait=0)
#' plan$items
#' plan[[1]]
getPlan <- function(g, names=NULL, quiet=FALSE, wait=-1) {
  if (is.null(g$plan)) return(NULL)
  if (is.null(names)) names <- as.character(1:nrow(g$vertices))
  imap  <- rev.web(g)
  gn    <- names[imap]
  gv    <- g$vertices[imap,]
  nv    <- length(gn)
  gp <- g
  gp$plan <- NULL
  if (wait!=0) plot(gp, blue=FALSE)
  plan  <- by(g$plan[,-1], g$plan[,1], function(x){x}, simplify=FALSE)
  ways  <- lapply (plan, function(plani) { plani[!duplicated(plani[,1]),1] })
  last  <- rep(0, max(g$plan[,1]))
  t     <- 1
  ml    <- max(sapply(ways, length))
  sbuls <- rep(0, max(g$plan[,1]))
  while(t<=nrow(g$plan)) {
    agent <- g$plan[t,1]
    if(wait!=0) {
      if (last[agent]==0) {
        points(gv[g$plan[t,2],1], gv[g$plan[t,2],2], col="#FF00FF", pch=19)
      } else {
        ind <- c(last[agent], g$plan[t,2])
        lines(gv[ind,1], gv[ind,2], col="#FF00FF")
      }
    }
    last[agent] <- g$plan[t,2]
    subplan     <- subset(g$plan, (g$plan[,1]==agent) & (g$plan[,2]==last[agent]))
    links       <- subplan[,3] 
    t           <- t+length(links)
    links       <- links[!is.na(links)]
    if (wait!=0) {
      if (length(links)) {
        for (i in 1:length(links)) {
         if (!is.na(links[i])) lines(gv[c(last[agent], links[i]), 1],gv[c(last[agent], links[i]), 2], col="red") 
        }
      }
    }
    if (wait!=0) {
      cat(sprintf("Agent: %2.0f, Portal: %s, ", agent, gn[last[agent]]))
      cat(sprintf("SBULs: %.0f, ", trunc((length(links)-1)/8)))
      cat(sprintf("Link to: %s\n", paste0(gn[links], collapse=',')))
      if (wait<0) invisible(readline(prompt="Press [enter] to continue")) else Sys.sleep(wait)
    }      
    sbuls[agent] <- sbuls[agent] + trunc((length(links)-1)/8)
  }
  #
  agents <- max(g$plan[,1])
  ret    <- list()
  dist   <- rep(0, agents)
  # walk 'n link
  for (i in 1:agents) {
    ret[[i]] <- subset(g$plan[,-1], g$plan[,1]==i)
    ind      <- imap[ret[[i]][,1]] 
    dist[i]  <- sum(sqrt(diff(g$vertices[ind,1])^2+diff(g$vertices[ind,2])^2))
    ret[[i]] <- cbind(gn[ret[[i]][,1]], gn[ret[[i]][,2]])
    colnames(ret[[i]]) <- c("Portal", "Link to")
    if (!quiet) {
      cat(sprintf("\nWalk 'n link agent %.0f (%.0f m as the crow flies)\n", i, dist[i]))
      print(ret[[i]])      
    }
  }
  # distances
  ret$dist <- dist
  # items
  keys <- table(factor(g$plan[,1]), factor(g$plan[,3], levels=1:nv, labels=gn))
  rnk  <- rownames(keys)
  cnk  <- colnames(keys)
  keys <- cbind(keys, sbuls[as.integer(rnk)])
  keys <- rbind(keys, margin.table(keys,2))
  rownames(keys) <- c(rnk, 'Total')
  colnames(keys) <- c(cnk, 'SBUL')
  if (!quiet) {
    cat("\nRequired items\n")
    print(keys)
  }
  ret$items <- keys
  invisible(ret)
}