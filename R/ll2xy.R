#' ll2xy
#'
#' Converts portal coordinates from (longitude, latitude) to xy using the \href{https://en.wikipedia.org/wiki/Universal_Transverse_Mercator_coordinate_system}{UTM projection}.
#' 
#' To install the package rgdal you may need to install under linux the appropriate system libraries. 
#' For details see in stackoverflow.com: \href{https://stackoverflow.com/questions/15248815/rgdal-package-installation}{rgdal package installation} and
#' \href{https://stackoverflow.com/questions/12141422/error-gdal-config-not-found}{Error: gdal-config not found}
#' 
#' @param lon numeric vector: longitudes of portal data
#' @param lat numeric vector: latitude of portal data
#'
#' @return a numeric matrix with xy coordinates. 
#' @export
#' @importFrom rgdal project
#'
#' @examples
#' data(leisepark)
#' xy <- ll2xy(leisepark$lon, leisepark$lat)
#' g  <- homogeneous(2)
#' gi <- web(g, xy)
#' plot(gi)
ll2xy <- function(lon, lat) {
  zone <- 1+trunc((180+ mean(range(lon)))/6) 
  xy   <- project(cbind(lon,lat), sprintf("+proj=utm +zone=%.0f ellps=WGS84", zone))
  cbind(x=xy[,1], y=xy[,2])
}