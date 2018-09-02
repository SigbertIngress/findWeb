#' linkPlan1
#'
#' Creates a way through the portals and a link plan for a single agent.
#'
#' If \code{direction} is not set then the direction of the first principal component is as main direction choosen.
#' If \code{way} is set then first is checked if we can create a maximum field with the path. If yes you get the link plan bakc.
#'
#' @param g web
#' @param way numeric vector: a sequence of portals the agent should go (default: NULL)
#' @param direction numeric: angle main direction the agent will (default: NULL) 
#'
#' @return a web with a way and link plan
#' @export
#' @importFrom stats prcomp
#'
#' @examples
#' set.seed(0)
#' g  <- homogeneous(2)
#' x  <- cbind(runif(10), runif(10))
#' gi <- web(g, x)
#' g1 <- optimizeWeb(gi)
#' g2 <- linkPlan1(g1)
#' \dontrun{
#' runPlan(g2)
#' }
linkPlan1 <- function (g, way=NULL, direction=NULL) {
  tf <- !is.na(g$map)
  gv <- g$vertices[tf,]
  gv <- gv[order(g$map[tf]),]
  if (is.null(way)) {
    if (is.null(direction)) {
      rot <- prcomp(gv)$rotation
      direction <- atan2(rot[2,1], rot[1,1])
    } 
    pc1 <- gv[,1]*cos(pi+direction)+gv[,2]*sin(pi+direction)
    way <- order(pc1)
  } else { # check way
    if (!all(sort(way)==1:max(g$map, na.rm=TRUE))) stop("check way: each portal should appear exactly once!") 
    for (i in 3:length(way)) {
      if (sum(pointsInConvexSet(gv, way[1:i]))>i) stop("check way: invalid way")
    }    
    direction <- NA
  }
  plan <- rbind(c(way[1], NA), way[2:1])
  for (i in 3:length(way)) {
    plan <- rbind(plan, linkBack(gv, g$edges, g$faces, g$pts, way[i], way[1:(i-1)]))
  }
  if (nrow(plan)!=nrow(g$edges)+1) stop("plan length does not match")
  attr(plan, 'direction') <- direction
  print(direction)
  g$plan <- cbind(rep(1, nrow(plan)), plan)
  g
}