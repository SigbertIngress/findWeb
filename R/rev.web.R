#' rev
#'
#' Extracts the mapping from the real portals to the portals of the target linking structure
#'
#' @param x a web
#'
#' @return a vector of portal indices
#' @export
#'
#' @examples
#' data(leisepark)
#' xy <- ll2xy(leisepark$lon, leisepark$lat)
#' g <- fishbone(8)
#' g0 <- web(g, xy)
#' rev(g0)
rev.web <- function(x) {
  if(is.null(x$map)) return(NULL)
  po <- which(!is.na(x$map))
  ps <- x$map[!is.na(x$map)]
  po[order(ps)]
}