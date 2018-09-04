#' icpdist
#'
#' Computes the point distance between to set of 2D points \code{query} and \code{data}. 
#' Each point in \code{query}, transformed with \code{par}, is linked to a (different) point \code{data}.
#' if \code{full=FALSE} then only the summarized distances between the transformed and the matched 
#' points are returned. Otherwise for each point in \code{query} a list is returned with transformed
#' points \code{xy}, the index of the "nearest" point in data and the distance. 
#' 
#' \code{par} allows for a transformation of \code{query} as in 
#' \eqn{\verb#xy# = \left(\begin{array} \verb#par[1]#& \verb#par[3]# \\ \verb#par[2]# & \verb#par[4]#\end{array}\right)\verb#query#+{\left(\begin{array} \verb#par[5]#\\ \verb#par[6]# \end{array}\right)}
#' @param par numeric(6): the transformation parameters
#' @param query matrix: positions of 2D points to match in \code{data}
#' @param data matrix: positions of 2D points
#' @param full logical: return only summarized distances of full information (default: \code{FALSE})
#'
#' @return summarized distances or detailled list
#' @export
#' @importFrom FNN get.knnx
#'
#' @examples
#' n     <- 10
#' data  <- cbind(runif(n),runif(n))
#' query <- cbind(runif(3), runif(3))
#' icpdist(par=c(1,0,0,1,0,0), data=data, query=query)
#' icpdist(par=c(1,0,0,1,0,0), data=data, query=query, full=TRUE)
icpdist <- function(par, query, data, full=FALSE) {
  xy   <- cbind(par[1]*query[,1]+par[3]*query[,2]+par[5], 
                par[2]*query[,1]+par[4]*query[,2]+par[6])
  k    <- get.knnx(data, xy, k=nrow(data))
  ind  <- rep(0, nrow(query))
  dist <- rep(NA, nrow(query))
  while(sum(is.na(dist))) {
    pos <- which(k$nn.dist==min(k$nn.dist), arr.ind=TRUE)
    row <- pos[1]
    ind[row]  <- k$nn.index[pos]
    dist[row] <- k$nn.dist[pos]
    pos <- which(k$nn.index==k$nn.index[pos], arr.ind=TRUE)
    k$nn.dist[pos]  <- Inf
    k$nn.dist[row,] <- Inf
  }
  if (full) return(list(dist=dist, index=ind, xy=xy))
  sum(dist)
}