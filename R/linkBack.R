
#' linkBack
#'
#' Links back portal \code{f} to all portals in \code{t} if possible. 
#' If necessary links are ordered such that multilayers will be generated. 
#'
#' @param gv numerich matrix: position of portals
#' @param edges numerich matrix: matrix of links in a link configuration
#' @param faces numerich matrix: matrix of fields in a link configuration
#' @param pts numerich vector: vector of portals contained in each field
#' @param f numeric: index of portal to link from
#' @param t numeric vector: index of portals to link to
#'
#' @return a link plan
#' @export
#'
#' @examples
#' \dontrun{
#' "no example yet"
#' }
linkBack <- function(gv, edges, faces, pts, f, t) {
  plan <- matrix(0, ncol=2, nrow=0)
  for (ti in t) {
    if (any((edges[,1]==f) & (edges[,2]==ti))) plan <- rbind(plan, c(f, ti))
    if (any((edges[,2]==f) & (edges[,1]==ti))) plan <- rbind(plan, c(f, ti))
  }
  np <- nrow(plan)
  if (np==0) return(matrix(c(f,NA), ncol=2))
  if (np<3) return(plan)
  linkorder <- matrix(0, ncol=0, nrow=3)
  for (k in 1:(np-1)) {
    for (l in (k+1):np) {
      ind <- sort(unique(c(plan[k,], plan[l,])))
      pos <- which(apply(faces, 1, function(row, ind) { all(row==ind) }, ind=ind))
      if (length(pos)) linkorder <- cbind(linkorder, c(plan[c(k,l),2], pts[pos]))
    }
  }
  if (ncol(linkorder)>1) {
    linkorder <- linkorder[,order(linkorder[3,], decreasing=TRUE)] 
    newplan   <- as.vector(linkorder[1:2,])
  } else {
    newplan <- linkorder[1:2,1]
  }
  ret  <- rbind(cbind(rep(f, length(newplan)), newplan), plan)
  np   <- nrow(ret)
  keep <- rep(TRUE, np)
  for (k in 1:(np-1)) {
    if (keep[k]) {
      for (l in (k+1):np) {
        if (all(ret[k,]==ret[l,])) keep[l] <- FALSE
      }
    }
  }
  ret[keep,]
}
