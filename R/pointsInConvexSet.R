#' pointsInConvexSet
#'
#' Given the point indices in \code{set} form a convex set, it is checked 
#' for each point that if it is inside the convex set or not.  
#'
#' @param px numeric matrix: portal position
#' @param set numeric vector: index of portals which form a convex set
#'
#' @return logical vector with \code{TRUE} if point is in the set otherwise false
#' @export
#'
#' @examples
#' set.seed(1)
#' x <- cbind(runif(10), runif(10))
#' inside <- pointsInConvexSet(x, c(1,7,10))
#' plot(x, col=inside+1, asp=TRUE, pch=19)
#' polygon(x[c(1,7,10),], border="red")
pointsInConvexSet <- function(px, set) {
  area <- function (x1, y1, x2, y2, x3, y3) {
    (y2 - y1)*(x3 - x2) - (y3 - y2)*(x2 - x1)
  }
  #
  npx    <- nrow(px)
  center <- colMeans(px[set,])
  px     <- scale(px, center=center, scale=FALSE) 
  #  plot(px, type="n", asp=TRUE)
  #  text(px)
  #  points(0,0, col="red")
  angle  <- atan2(px[,2], px[,1])
  po     <- (1:npx)[order(angle)]
  ret    <- rep(NA, npx)
  spo    <- set[order(angle[set])]
  j      <- 0
  wedge  <- c(spo[length(spo)], spo[1])
  for (i in 1:nrow(px)) {
    if (po[i] %in% set) {
      ret[po[i]] <- TRUE
      j     <- j+1
      wedge <- if (j==length(spo)) spo[c(length(spo), 1)] else spo[c(j,j+1)]
    } else {
      a <- area(px[wedge[1], 1], px[wedge[1], 2], px[wedge[2], 1], px[wedge[2], 2], px[po[i],1], px[po[i],2])
      ret[po[i]] <- (a<0)
    }
  }
  ret
}
