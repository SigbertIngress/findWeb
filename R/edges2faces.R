#' edges2faces
#'
#' Given a set of edges between vertices (with xy coordinates) it returns a matrix of all triangles formed by the edges.
#'
#' @param edges numeric matrix with two columns: index of points which have a edge
#'
#' @return  faces numeric matrix with three columns: index of points which form a triangle
#' @export
#'
#' @examples 
#' edges <- cbind(c(1,1,1,2,2,3), c(2,3,4,3,4,4))
#' edges2faces(edges)
edges2faces <- function(edges) {
  faces    <- matrix(0, ncol=3, nrow=0)
  for (i in 1:(nrow(edges)-1)) {
    for (j in (i+1):nrow(edges)) {
      if (edges[i,1]<edges[j,1]) break
      if (any((edges[i,2]==edges[,1]) & (edges[j,2]==edges[,2]))) {
        faces <- rbind(faces, c(edges[i,], edges[j,2]))
      }
    }
  }
  normalize(faces)
}